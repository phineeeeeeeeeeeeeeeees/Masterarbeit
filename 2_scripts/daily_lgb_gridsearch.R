#####################################################################################################
# Masterarbeit
# Modeling
# Monthly model: Light gradient boosting machine (LightGBM)-- grid search of hyperparameters
# 2021-07-16
#####################################################################################################

# =====================================
# required packages
# =====================================
library(readr) 
library(sf) 
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(ggsci) ; library(ggthemes)
library(lubridate) ; library(stringr)
library(spdep)
library(Metrics)
library(lightgbm)


# =====================================
# load datasets
# =====================================
# training data 
data_daily_raw <- read_csv("1_data/processed/cleaned/extracted/daily_scaled.csv")

# cross validation
{
  in_filepath_CV <- list.files("1_data/processed/cleaned/extracted/" , "CV.shp$" , full.names = TRUE)
  sites_CV <- st_read(in_filepath_CV) %>% 
    rename(Station_name = Station)
  k_fold <- in_filepath_CV %>% 
    str_extract("\\d+-fold") %>% str_extract("\\d+") %>% 
    as.integer()
}

# implement the CV design in the training data
data_daily_raw <- data_daily_raw %>% 
  inner_join(sites_CV , by = "Station_name") %>% 
  select(-geometry)

# non-predictor columns
columns_nonpredictor <- c("Station_name" , "NO2" , "Type_of_zone" , "Type_of_station" , 
                          "Altitude" , "Canton_ID" , "Canton_name" , "X" , "Y" , 
                          "CV" , "spatial_CV" , "date")

# //////////////////////////////////////////////////////////////////////////
# naming the model
# //////////////////////////////////////////////////////////////////////////
SAT_product <- "TROPOMI"
# for now I grid-search the model using TROPOMI and use the same hyperparameter set for OMI


# //////////////////////////////////////////////////////////////////////////
# data preparation for xgboost
# //////////////////////////////////////////////////////////////////////////
if(SAT_product == "OMI"){
  data_daily <- data_daily_raw %>% 
    # OMI
    select(-TROPOMI_NO2, -ends_with("12H")) %>% 
    # drop NA
    drop_na() %>% 
    # date as DOY (numeric)
    mutate(DOY = yday(date))
}else if(SAT_product == "TROPOMI") {
  data_daily <- data_daily_raw %>% 
    # TROPOMI
    select(-OMI_NO2, -ends_with("15H")) %>% 
    # drop NA
    drop_na() %>% 
    # date as DOY (numeric)
    mutate(DOY = yday(date))
}

# =====================================
# response
# =====================================
response_full <- data_daily %>% 
  select(NO2) %>% 
  unlist() %>% unname()

# =====================================
# predictor
# screening of important predictor variables
# =====================================
# set.seed(123)
# lgb_screen <- lightgbm(
#   data = data_daily %>% 
#     select(-all_of(columns_nonpredictor)) %>% 
#     # random-value variables
#     mutate(R1 = runif(n()) , 
#            R2 = runif(n()) ,
#            R3 = runif(n()) ) %>% 
#     # matrix
#     as.matrix() %>% 
#     # lgb.Dataset object
#     lgb.Dataset(data = . , label = response_full) , 
#   params = list(
#     learning_rate = 0.05 , 
#     bagging_fraction = 0.8 , 
#     max_depth = 8
#   ) ,
#   objective = "regression" ,
#   boosting_type = "gbdt" , 
#   nrounds = 100
#   )
# 
# # importance
# lgb_screen_importance <- lgb_screen %>% 
#   lgb.importance(model = .) %>% 
#   as_tibble()
# # visualization
# lgb_screen_importance %>% 
#   mutate(class = ifelse(str_detect(Feature , "R[123]") , "Random" , "Predictor variables")) %>% 
#   # reorder for visualization
#   mutate(Feature = factor(Feature , levels = Feature[order(Gain)])) %>% 
#   # visualization
#   ggplot(aes(x = Feature , y = Gain , fill = class)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   scale_fill_lancet() +
#   labs(x = "Variables" , y = "Variable importance (Gain)" , fill = "" , 
#        title = "Screening of relevant predictor variables" , 
#        subtitle = sprintf("The variable importance of LightGBM (%s)" , SAT_product)) +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 4) , legend.position = "bottom")
# 
# # exclude not-important variables 
# included_var <- xgb_screen_importance %>% 
#   filter(Gain > xgb_screen_importance %>% 
#            filter(str_detect(Feature , "R[123]")) %>% 
#            # the max importance of the random-value variables 
#            summarize(Gain = max(Gain)) %>% 
#            unlist) %>% 
#   select(Feature) %>% 
#   unlist %>% unname()

# =====================================
# predictor variables (as a matrix)
# =====================================
predictor_full <- data_daily %>% 
  select(-all_of(columns_nonpredictor)) %>% 
  #select(all_of(included_var)) %>% 
  as.matrix()

# //////////////////////////////////////////////////////////////////////////
# grid search for best model hyperparameters
# //////////////////////////////////////////////////////////////////////////
# only looking at the cross-validation RMSE

# =====================================
# CV folds (as a list) (conventional random-split CV)
# =====================================
CV_folds <- data_daily %>% 
  mutate(rowID = 1:n()) %>% 
  select(CV , rowID) %>% 
  group_by(CV) %>% 
  group_map( ~ c(.x$rowID))

# =====================================
# create hyperparameter grid
# =====================================
# https://lightgbm.readthedocs.io/en/latest/Parameters.html#core-parameters 
# https://lightgbm.readthedocs.io/en/latest/Parameters-Tuning.html

# create hyperparameter grid
hyper_grid <- expand.grid(
  stringsAsFactors = FALSE , 
  # learning rate
  learning_rate = c(.01, .05, .1, .3),
  # tree depth
  max_depth = c(1, 3, 5, 8, 10),
  # number of leaves
  num_leaves = c(5,25,50,100,200),
  # bagging fraction
  bagging_fraction = c(0.5,0.7,0.85,1) ,
  # feature fraction
  # By default, LightGBM considers all features in a Dataset during the training process. 
  # This behavior can be changed by setting feature_fraction to a value > 0 and <= 1.0. 
  # Setting feature_fraction to 0.5, for example, tells LightGBM to randomly select 50% of features at the beginning of constructing each tree.
  feature_fraction = c(0.5,0.75,1) ,
  # booster
  boosting_type = c("gbdt" , "dart") ,
  optimal_trees = NA,               # a place to dump results
  min_RMSE = NA,                    # a place to dump results
  CV_R2 = NA                        # a place to dump results
)

# =====================================
# grid search 
# =====================================
pb <- txtProgressBar(min = 1 , max = nrow(hyper_grid) , style = 3 )
for(i in 1:nrow(hyper_grid)) {
  # create parameter list
  hyper_i <- list(
    learning_rate = hyper_grid$learning_rate[i] , 
    max_depth = hyper_grid$max_depth[i] , 
    num_leaves = hyper_grid$num_leaves[i] , 
    bagging_fraction = hyper_grid$bagging_fraction[i] , 
    feature_fraction = hyper_grid$bagging_fraction[i] , 
    boosting_type = hyper_grid$boosting[i]
  )
  # reproducibility
  set.seed(123)
  lgb_grid <- lgb.cv(
    params = hyper_i , 
    data = data_daily %>%
      select(-all_of(columns_nonpredictor)) %>%
      # matrix
      as.matrix() %>%
      # lgb.Dataset object
      lgb.Dataset(data = . , label = response_full) ,
    # label = response_full ,
    nrounds = 1000 , 
    objective = "regression" , 
    eval = "rmse" ,
    task = "prediction" ,
    folds = CV_folds , 
    showsd = FALSE , 
    verbose = 1 , # silent
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
  lgb_grid$best_iter
  lgb_grid$record_evals$valid$rmse$eval %>% unlist %>% which.min()
  lgb_grid$record_evals$valid$rmse$eval %>% unlist %>% plot()
  # evaluation (add to hyper_grid data.frame)
  # the number of iterations with the lowest CV-RMSE
  hyper_grid$optimal_trees[i] <- which.min(xgb_grid$evaluation_log$test_rmse_mean)
  # CV-RMSE
  hyper_grid$min_RMSE[i] <- min(xgb_grid$evaluation_log$test_rmse_mean)
  # CV-R2
  hyper_grid$CV_R2[i] <- cor(response_full , xgb_grid$pred)^2
  # progress bar
  setTxtProgressBar(pb,i)
  # clean environment
  rm(hyper_i , xgb_grid)
}
rm(pb,i)

lgb_test <- lightgbm(
  params = hyper_i , 
  data = data_daily %>%
    select(-all_of(columns_nonpredictor)) %>%
    # matrix
    as.matrix() %>%
    # lgb.Dataset object
    lgb.Dataset(data = . , label = response_full) ,
  # label = response_full ,
  nrounds = 1000 , 
  objective = "regression" , 
  task = "train" , 
  #metric = "rmse" , 
  verbose = 1 , # silent
  early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)

pred_test <- predict(lgb_test , predictor_full)
plot(pred_test , response_full)
# =====================================
# export grid search results
# =====================================
out_dirpath_hypergrid <- "3_results/output-data/model_daily/GBM_grid-search"
if(!dir.exists(out_dirpath_hypergrid)) dir.create(out_dirpath_hypergrid , recursive = TRUE)
hyper_grid %>%
  write_csv(sprintf("%s/hyper_evaluation.csv" , out_dirpath_hypergrid))



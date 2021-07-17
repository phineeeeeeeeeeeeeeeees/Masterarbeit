#####################################################################################################
# Masterarbeit
# Modeling
# Annual model: Generalized boosting machine-- grid search of hyperparameters
# 2021-06-22
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
library(xgboost) ; library(pdp)


# =====================================
# load datasets
# =====================================
# training data
data_annual_raw <- read_csv("1_data/processed/cleaned/extracted/annual_scaled.csv") 

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
data_annual_raw <- data_annual_raw %>% 
  inner_join(sites_CV , by = "Station_name") %>% # this also excludes the 3 sites that are outside Switzerland and was not counted in the CV groups
  select(-geometry)

# non-predictor columns
columns_nonpredictor <- c("Station_name" , "NO2" , "Type_of_zone" , "Type_of_station" , 
                          "Altitude" , "Canton_ID" , "Canton_name" , "X" , "Y" , 
                          "CV" , "spatial_CV")

# //////////////////////////////////////////////////////////////////////////
# naming the model
# //////////////////////////////////////////////////////////////////////////
SAT_product <- "TROPOMI"
# for now I grid-search the model using TROPOMI and use the same hyperparameter set for OMI


# //////////////////////////////////////////////////////////////////////////
# data preparation for xgboost
# //////////////////////////////////////////////////////////////////////////
if(SAT_product == "OMI"){
  data_annual <- data_annual_raw %>% 
    # OMI
    select(-TROPOMI_NO2, -ends_with("12H")) %>% 
    drop_na()
}else if(SAT_product == "TROPOMI") {
  data_annual <- data_annual_raw %>% 
    # TROPOMI
    select(-OMI_NO2, -ends_with("15H")) %>% 
    drop_na()
}

# =====================================
# response
# =====================================
response_full <- data_annual %>% 
  select(NO2) %>% 
  unlist() %>% unname()

# =====================================
# predictor
# screening of important predictor variables
# =====================================
set.seed(123)
xgb_screen <- xgboost(
  data = data_annual %>% 
    select(-all_of(columns_nonpredictor)) %>% 
    # random-value variables
    mutate(R1 = runif(n()) , 
           R2 = runif(n()) ,
           R3 = runif(n()) ) %>% 
    # matrix
    as.matrix(), 
  label = response_full , 
  nrounds = 100 , 
  objective = "reg:squarederror" , 
  verbose = FALSE ,  # silent
  early_stopping_rounds = 5 # stop if no improvement for 10 consecutive trees
)

xgb_screen_importance <- xgb_screen %>% 
  xgb.importance(model = .) %>% 
  as_tibble()
# visualization
xgb_screen_importance %>% 
  mutate(class = ifelse(str_detect(Feature , "R[123]") , "Random" , "Predictor variables")) %>% 
  # reorder for visualization
  mutate(Feature = factor(Feature , levels = Feature[order(Gain)])) %>% 
  # visualization
  ggplot(aes(x = Feature , y = Gain , fill = class)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_lancet() +
  labs(x = "Variables" , y = "Variable importance (Gain)" , fill = "" , 
       title = "Screening of relevant predictor variables" , 
       subtitle = sprintf("The variable importance of the gradient boosting machine (%s)" , SAT_product)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 4) , legend.position = "bottom")

# exclude not-important variables 
included_var <- xgb_screen_importance %>% 
  filter(Gain > xgb_screen_importance %>% 
           filter(str_detect(Feature , "R[123]")) %>% 
           # the max importance of the random-value variables 
           summarize(Gain = max(Gain)) %>% 
           unlist) %>% 
  select(Feature) %>% 
  unlist %>% unname()

# =====================================
# predictor variables (as a matrix)
# =====================================
predictor_full <- data_annual %>% 
  select(all_of(included_var)) %>% 
  as.matrix()

# //////////////////////////////////////////////////////////////////////////
# grid search for best model hyperparameters
# //////////////////////////////////////////////////////////////////////////
# only looking at the cross-validation RMSE

# =====================================
# CV folds (as a list) (conventional random-split CV)
# =====================================
CV_folds <- data_annual %>% 
  mutate(rowID = 1:n()) %>% 
  select(CV , rowID) %>% 
  group_by(CV) %>% 
  group_map( ~ c(.x$rowID))

# =====================================
# create hyperparameter grid
# =====================================
# create hyperparameter grid
hyper_grid <- expand.grid(
  # controls the learning rate
  eta = c(.01, .05, .1, .3),
  # tree depth
  max_depth = c(1, 3, 5, 10),
  # minimum number of observations required in each terminal node
  min_child_weight = c(1, 3, 5, 7),
  # percent of training data to sample for each tree
  subsample = c(.65, .8, 1), 
  # percent of columns to sample from for each tree
  colsample_bytree = c(.8, .9, 1) ,
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
    eta = hyper_grid$eta[i],
    max_depth = hyper_grid$max_depth[i],
    min_child_weight = hyper_grid$min_child_weight[i],
    subsample = hyper_grid$subsample[i],
    colsample_bytree = hyper_grid$colsample_bytree[i]
  )
  # reproducibility
  set.seed(123)
  # train model
  xgb_grid <- xgb.cv(
    params = hyper_i,
    data = predictor_full,
    label = response_full,
    nrounds = 1000 , 
    objective = "reg:squarederror" , 
    folds = CV_folds , 
    prediction = TRUE , 
    verbose = FALSE ,  # silent
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
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

# =====================================
# export grid search results
# =====================================
out_dirpath_hypergrid <- "3_results/output-data/model_annual/GBM_grid-search"
if(!dir.exists(out_dirpath_hypergrid)) dir.create(out_dirpath_hypergrid)
hyper_grid %>%
  write_csv(sprintf("%s/hyper_evaluation.csv" , out_dirpath_hypergrid))

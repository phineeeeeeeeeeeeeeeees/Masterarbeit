#####################################################################################################
# Masterarbeit
# Modeling
# Daily model: Light gradient boosting machine (LightGBM)-- grid search of hyperparameters
# 2021-08-16
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
library(parallel) ; library(pbapply)


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
# data preparation for LightGBM
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
  as.matrix()

# =====================================
# predictor
# screening of important predictor variables
# =====================================
set.seed(123)
lgb_screen <- lightgbm(
  data = data_daily %>%
    select(-all_of(columns_nonpredictor)) %>%
    # random-value variables
    mutate(R1 = runif(n()) ,
           R2 = runif(n()) ,
           R3 = runif(n()) ) %>%
    # matrix
    as.matrix() %>%
    # lgb.Dataset object
    lgb.Dataset(data = . , label = response_full) ,
  # default
  params = list(learning_rate = 0.1 , num_leaves = 31) ,
  objective = "regression" ,
  boosting_type = "gbdt" ,
  nrounds = 500
  )

# importance
lgb_screen_importance <- lgb_screen %>%
  lgb.importance(model = .) %>%
  as_tibble()
# visualization
lgb_screen_importance %>%
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
       subtitle = sprintf("The variable importance of LightGBM (%s)" , SAT_product)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 4) , legend.position = "bottom")

# exclude not-important variables
included_var <- lgb_screen_importance %>%
  filter(Gain > lgb_screen_importance %>%
           filter(str_detect(Feature , "R[123]")) %>%
           # the max importance of the random-value variables
           summarize(Gain = max(Gain)) %>%
           unlist) %>%
  select(Feature) %>%
  unlist %>% unname()

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
  # variable selection
  selection = c(TRUE , FALSE) , 
  # learning rate
  learning_rate = c(.01, .05, .1, .3),
  # tree depth
  max_depth = c(1, 3, 5, 8, 10),
  # number of leaves
  num_leaves = c(25,50,100,200),
  # bagging fraction
  bagging_fraction = c(0.5,0.7,0.85,1) ,
  # feature fraction
  # By default, LightGBM considers all features in a Dataset during the training process. 
  # This behavior can be changed by setting feature_fraction to a value > 0 and <= 1.0. 
  # Setting feature_fraction to 0.5, for example, tells LightGBM to randomly select 50% of features at the beginning of constructing each tree.
  feature_fraction = c(0.5,0.75,1) ,
  # boosters
  boosting_type = c("gbdt" , "dart") ,
  # a place to dump results
  optimal_trees = NA,               
  min_RMSE = NA
)

# =====================================
# grid search (parallelized)
# =====================================
# make clusters
cl <- makeCluster(detectCores())
# clusterExport (R objects across clusters)
clusterExport(
  cl = cl , 
  varlist = c("data_daily" , "CV_folds" , "included_var" , "response_full" , "columns_nonpredictor")
)
# random seed
clusterSetRNGStream(cl , 123)
# grid search function for "apply"
search_hyper_grid <- function(hyper_grid_row){
  # hyper_grid_row is the hyperparameter (one row of "hyper_grid") (a character vector)
  # ---------------------------------------
  # load packages for the clusters
  library(dplyr) ; library(lightgbm)
  # create parameter list
  hyper_i <- hyper_grid_row %>% 
    as.list() %>% as_tibble() %>% 
    select(-optimal_trees, -min_RMSE) %>% 
    mutate(across(selection , as.logical)) %>% 
    mutate(across(c(learning_rate , max_depth , num_leaves , bagging_fraction , feature_fraction) , as.numeric)) %>% 
    mutate(across(boosting_type , as.character)) %>% 
    as.list()
  # variable selection
  if(hyper_i$selection){
    predictor_full <- data_daily %>% 
      select(all_of(included_var)) %>% 
      as.matrix()
  }else{
    predictor_full <- data_daily %>% 
      select(-all_of(columns_nonpredictor)) %>% 
      as.matrix()
  }
  # # reproducibility
  # set.seed(123) # use clusterSetRNGStream(cl , 123) for parallel computation
  # cross validation
  lgb_grid <- lgb.cv(
    params = hyper_i , 
    data = predictor_full , 
    label = response_full ,
    nrounds = 1000 , 
    objective = "regression" , 
    eval = "rmse" ,
    task = "prediction" ,
    folds = CV_folds , 
    showsd = FALSE , 
    verbose = -1 , # silent
    early_stopping_rounds = 50 # stop if no improvement for 10 consecutive trees
  )
  # evaluation (add to hyper_grid data.frame)
  # the number of iterations with the lowest CV-RMSE
  hyper_grid_row$optimal_trees <- lgb_grid$record_evals$valid$rmse$eval %>% unlist %>% which.min()
  # CV-RMSE
  hyper_grid_row$min_RMSE <- lgb_grid$record_evals$valid$rmse$eval %>% unlist %>% min()
  # ---------------------------------------
  return(hyper_grid_row)
}

# grid search (parallelized)
hyper_grid_result <- hyper_grid %>% 
  pbapply(MARGIN = 1 , FUN = search_hyper_grid , cl = cl) %>% 
  bind_rows()

# turn off cluster
stopCluster(cl)

# =====================================
# export grid search results
# =====================================
out_dirpath_hypergrid <- "3_results/output-data/model_daily/LGB_grid-search"
if(!dir.exists(out_dirpath_hypergrid)) dir.create(out_dirpath_hypergrid , recursive = TRUE)
hyper_grid_result %>%
  write_csv(sprintf("%s/hyper_evaluation.csv" , out_dirpath_hypergrid))



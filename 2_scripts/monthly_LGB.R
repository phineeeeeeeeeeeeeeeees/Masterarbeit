#####################################################################################################
# Masterarbeit
# Modeling
# Monthly model: Light gradient boosting machines 
# 2021-08-22
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

source("2_scripts/utils_model-eval.R")

# =====================================
# load datasets
# =====================================
# training data 
data_monthly_raw <- read_csv("1_data/processed/cleaned/extracted/monthly_scaled.csv") 

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
data_monthly_raw <- data_monthly_raw %>% 
  inner_join(sites_CV , by = "Station_name") %>% 
  select(-geometry)

# non-predictor columns
columns_nonpredictor <- c("Station_name" , "NO2" , "Type_of_zone" , "Type_of_station" , 
                          "Altitude" , "Canton_ID" , "Canton_name" , "X" , "Y" , 
                          "CV" , "spatial_CV")

# //////////////////////////////////////////////////////////////////////////
# naming the model
# //////////////////////////////////////////////////////////////////////////
model_name <- "Light gradient boosting machine"
model_abbr <- "LGB"
SAT_product <- c("OMI" , "TROPOMI")[2]


# //////////////////////////////////////////////////////////////////////////
# data preparation for LightGBM
# //////////////////////////////////////////////////////////////////////////
if(SAT_product == "OMI"){
  data_monthly <- data_monthly_raw %>% 
    # OMI
    select(-TROPOMI_NO2, -ends_with("12H")) %>% 
    drop_na()
}else if(SAT_product == "TROPOMI") {
  data_monthly <- data_monthly_raw %>% 
    # TROPOMI
    select(-OMI_NO2, -ends_with("15H")) %>% 
    drop_na()
}

# =====================================
# response
# =====================================
response_full <- data_monthly %>% 
  select(NO2) %>% 
  as.matrix()

# =====================================
# predictor
# screening of important predictor variables
# =====================================
set.seed(123)
lgb_screen <- lightgbm(
  data = data_monthly %>%
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

# =====================================
# predictors (as a matrix)
# =====================================
predictor_full <- data_monthly %>% 
  select(all_of(included_var)) %>% 
  as.matrix()

# //////////////////////////////////////////////////////////////////////////
# final model
# //////////////////////////////////////////////////////////////////////////
# =====================================
# hyperparameters from grid search
# =====================================
hyper_grid <- read_csv("3_results/output-data/model_monthly/LGB_grid-search/hyper_evaluation.csv")

hyperparm_final <- hyper_grid %>% 
  arrange(min_RMSE) %>% 
  dplyr::slice(1) %>% 
  select(learning_rate , max_depth , num_leaves , bagging_fraction , feature_fraction , boosting_type) %>% 
  as.list()
# learning_rate max_depth num_leaves bagging_fraction feature_fraction boosting_type
#           0.3         5         50              0.5              0.5 dart 
nrounds <- hyper_grid %>% 
  arrange(min_RMSE) %>% 
  dplyr::slice(1) %>% 
  select(optimal_trees) %>% 
  unlist

# =====================================
# model with the full training set
# =====================================
set.seed(123)
lgb_final_full <- lightgbm(
  params = hyperparm_final , 
  data = predictor_full , 
  label = response_full ,
  nrounds = nrounds , 
  objective = "regression" , 
  eval = "rmse" ,
  task = "prediction" ,
  save_name = NA , 
  verbose = -1 # silent
)

# =====================================
# cross validation
# =====================================
# conventional CV
for(k in as.factor(1:k_fold)){
  # data preparation: partition
  training.data <- data_monthly %>% 
    filter(CV != k)
  testing.data <- data_monthly %>% 
    filter(CV == k)
  # data preparation: make matrix
  predictor_train <- training.data %>% 
    select(all_of(included_var)) %>% 
    as.matrix()
  response_train <- training.data %>% 
    select(NO2) %>% 
    as.matrix()
  predictor_test <- testing.data %>% 
    select(all_of(included_var)) %>% 
    as.matrix()
  response_test <- testing.data %>% 
    select(NO2) %>% 
    as.matrix()
  # define model
  set.seed(123)
  lgb_train <- lightgbm(
    params = hyperparm_final , 
    data = predictor_train , 
    label = response_train ,
    nrounds = nrounds , 
    objective = "regression" , 
    eval = "rmse" ,
    task = "prediction" ,
    verbose = -1 # silent
  )
  # prediction
  prediction_test <- testing.data %>% 
    select(month , Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = predict(lgb_train , predictor_test))
  # prediction data.frame
  if(as.character(k) == "1"){
    lgb_final_prediction_CV <- prediction_test # <-
  }else{ # append
    lgb_final_prediction_CV <- bind_rows(lgb_final_prediction_CV , prediction_test)
  }
  # clean environment
  rm(lgb_train)
  rm(k , training.data , testing.data , predictor_train , predictor_test , response_train , response_test , prediction_test)
}

# spatial CV
for(k in as.factor(1:k_fold)){
  # data preparation: partition
  training.data <- data_monthly %>% 
    filter(spatial_CV != k)
  testing.data <- data_monthly %>% 
    filter(spatial_CV == k)
  # data preparation: make matrix
  predictor_train <- training.data %>% 
    select(all_of(included_var)) %>% 
    as.matrix()
  response_train <- training.data %>% 
    select(NO2) %>% 
    as.matrix()
  predictor_test <- testing.data %>% 
    select(all_of(included_var)) %>% 
    as.matrix()
  response_test <- testing.data %>% 
    select(NO2) %>% 
    as.matrix()
  # define model
  set.seed(123)
  lgb_train <- lightgbm(
    params = hyperparm_final , 
    data = predictor_train , 
    label = response_train ,
    nrounds = nrounds , 
    objective = "regression" , 
    eval = "rmse" ,
    task = "prediction" ,
    verbose = -1 # silent
  )
  # prediction
  prediction_test <- testing.data %>% 
    select(month , Station_name , NO2 , Type_of_station , spatial_CV) %>% 
    # prediction
    mutate(predicted = predict(lgb_train , predictor_test))
  # prediction data.frame
  if(as.character(k) == "1"){
    lgb_final_prediction_CV_sp <- prediction_test # <-
  }else{ # append
    lgb_final_prediction_CV_sp <- bind_rows(lgb_final_prediction_CV_sp , prediction_test)
  }
  # clean environment
  rm(lgb_train)
  rm(k , training.data , testing.data , predictor_train , predictor_test , response_train , response_test , prediction_test)
}

# temporal cross validation
for(k in 1:12){
  # data preparation: partition
  training.data <- data_monthly %>% 
    filter(month != k)
  testing.data <- data_monthly %>%
    filter(month == k)
  # data preparation: make matrix
  predictor_train <- training.data %>% 
    select(all_of(included_var)) %>% 
    as.matrix()
  response_train <- training.data %>% 
    select(NO2) %>% 
    as.matrix()
  predictor_test <- testing.data %>% 
    select(all_of(included_var)) %>% 
    as.matrix()
  response_test <- testing.data %>% 
    select(NO2) %>% 
    as.matrix()
  # define model
  set.seed(123)
  lgb_train <- lightgbm(
    params = hyperparm_final , 
    data = predictor_train , 
    label = response_train ,
    nrounds = nrounds , 
    objective = "regression" , 
    eval = "rmse" ,
    task = "prediction" ,
    verbose = -1 # silent
  )
  # prediction
  prediction_test <- testing.data %>% 
    select(month , Station_name , NO2 , Type_of_station) %>% 
    # prediction
    mutate(predicted_temporalCV = predict(lgb_train , predictor_test))
  # prediction data.frame
  if(as.character(k) == "1"){
    lgb_final_prediction_CV_tp <- prediction_test # <-
  }else{ # append
    lgb_final_prediction_CV_tp <- bind_rows(lgb_final_prediction_CV_tp , prediction_test)
  }
  # clean environment
  rm(lgb_train)
  rm(k , training.data , testing.data , predictor_train , predictor_test , response_train , response_test , prediction_test)
}


# combine the prediction of the three CV
lgb_final_prediction_CV <- lgb_final_prediction_CV %>% 
  left_join(lgb_final_prediction_CV_sp , 
            by = c("month" , "Station_name" , "NO2" , "Type_of_station") , 
            suffix = c("_CV" , "_spatialCV")) %>% 
  left_join(lgb_final_prediction_CV_tp , 
            by = c("month" , "Station_name" , "NO2" , "Type_of_station"))


# //////////////////////////////////////////////////////////////////////////
# model evaluation
# //////////////////////////////////////////////////////////////////////////
# =====================================
# observed, predicted, residuals
# =====================================
lgb_final_prediction <- data_monthly %>% 
  select(month , Station_name , NO2 , Type_of_station , X , Y) %>% 
  # prediction
  mutate(predicted = predict(lgb_final_full , predictor_full)) %>% 
  # CV-prediction
  full_join(lgb_final_prediction_CV , 
            by = c("Station_name" , "NO2" , "Type_of_station" , "month")) %>% 
  # residuals
  mutate(residual = NO2 - predicted , 
         residual_CV = NO2 - predicted_CV , 
         residual_spatialCV = NO2 - predicted_spatialCV , 
         residual_temporalCV = NO2 - predicted_temporalCV)

# =====================================
# performance indices
# =====================================
# model performance indices as a data.frame
lgb_final_indices <- lgb_final_prediction %>%  
  eval_performance_indices()

# =====================================
# variable importance
# =====================================
# Gain: the relative contribution of the corresponding feature to the model 
# calculated by taking each feature’s contribution for each tree in the model.
lgb_final_importance <- lgb_final_full %>% 
  lgb.importance(model = .) %>%
  as_tibble()

# =====================================
# visualization
# =====================================
out_dirpath_plots <- sprintf("3_results/output-graph/model_monthly/%s" , model_abbr)
if(!dir.exists(out_dirpath_plots)) dir.create(out_dirpath_plots)

# predicted <-> observed
plot_obs_pred(lgb_final_prediction , # <-
              sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/obs-pred_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 5 , base_height = 3.5
)

# residual diagnostic plots
plot_resid(lgb_final_prediction , # <-
           title_text = sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/residuals_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 7.8 , base_height = 6
)

# residuals by month
plot_resid_month(lgb_final_prediction , 
                 subtitle_text = sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/residuals-month_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 6 , base_height = 3
)

# variable importance
lgb_final_importance %>% 
  arrange(-Gain) %>% 
  # re-order for visualization
  mutate(Feature = factor(Feature , levels = Feature[order(Gain)])) %>% 
  ggplot(aes(x = Feature , y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variables" , y = "Gain" , 
       title = sprintf("LGB variable importance (with %s)" , SAT_product) , 
       subtitle = "Gain: the relative contribution of the corresponding \nfeature to the model") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6))
save_plot(
  sprintf("%s/importance_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 6 , base_height = 6
)

# =====================================
# spatial autocorrelation of the residuals
# =====================================
moran_month_df <- eval_resid_moran(lgb_final_prediction , by_time = TRUE , col_time = "month")
any(moran_month_df$p < 0.05)
moran_mean_df <- eval_resid_moran(lgb_final_prediction %>% drop_na() , 
                                  by_time = FALSE)

# visualization
plot_resid_map(lgb_final_prediction , # <-
               sprintf("%s (%s)" , str_to_title(model_name) , SAT_product)) 
save_plot(
  sprintf("%s/residual-map_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 6 , base_height = 4
)

# =====================================
# export datasets
# =====================================
{
  # export the predicted values
  out_dirpath_predicted <- "3_results/output-data/model_monthly/observed-predicted"
  if(!dir.exists(out_dirpath_predicted)) dir.create(out_dirpath_predicted)
  lgb_final_prediction %>% # <-
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_predicted , model_abbr , SAT_product))
  
  # export the model performance indices
  out_dirpath_indices <- "3_results/output-data/model_monthly/indices"
  if(!dir.exists(out_dirpath_indices)) dir.create(out_dirpath_indices)
  lgb_final_indices %>% # <- 
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_indices , model_abbr , SAT_product))
  
  # Moran's I
  out_dirpath_Moran <- "3_results/output-data/model_monthly/Moran"
  if(!dir.exists(out_dirpath_Moran)) dir.create(out_dirpath_Moran)
  moran_month_df %>% 
    pivot_longer(cols = -month) %>% 
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/month_%s_%s.csv" , out_dirpath_Moran , model_abbr , SAT_product))
  moran_mean_df %>% 
    pivot_longer(cols = everything()) %>% 
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/mean_%s_%s.csv" , out_dirpath_Moran , model_abbr , SAT_product))
}

# =====================================
# export model
# =====================================
{
  out_dirpath_model <- "3_results/output-model/model_monthly"
  if(!dir.exists(out_dirpath_model)) dir.create(out_dirpath_model)
  lgb.save(
    lgb_final_full , 
    filename = sprintf("%s/%s_%s.txt" , out_dirpath_model , model_abbr , SAT_product)
  )
}

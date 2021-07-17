#####################################################################################################
# Masterarbeit
# Modeling
# Monthly model: Gradient boosting machines 
# 2021-07-16
#####################################################################################################

# =====================================
# required packages
# =====================================
library(readr) 
library(sf) 
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(ggsci) ; library(ggthemes) ; library(cowplot)
library(lubridate) ; library(stringr)
library(spdep)
library(Metrics)
library(xgboost) ; library(pdp)

#conflicted::conflict_prefer("slice" , "dplyr") # dplyr::slice may be overwritten by xgboost::slice

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
model_name <- "Gradient boosting machine"
model_abbr <- "GBM"
SAT_product <- c("OMI" , "TROPOMI")[2]


# //////////////////////////////////////////////////////////////////////////
# data preparation for xgboost
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
  unlist() %>% unname()

# =====================================
# predictor
# screening of important predictor variables
# =====================================
set.seed(123)
xgb_screen <- xgboost(
  data = data_monthly %>% 
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
  early_stopping_rounds = 5 # stop if no improvement for 5 consecutive trees
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
hyper_grid <- read_csv("3_results/output-data/model_monthly/GBM_grid-search/hyper_evaluation.csv")

hyperparm_final <- hyper_grid %>% 
  arrange(-CV_R2) %>%
  dplyr::slice(1) %>% 
  select(eta, max_depth, min_child_weight, subsample, colsample_bytree) %>% 
  as.list()
#  eta  max_depth min_child_weight subsample colsample_bytree
#  0.01         5                7      0.65              0.8
nrounds <- 450

# =====================================
# model with the full training set
# =====================================
set.seed(123)
xgb_final_full <- xgboost(
  data = predictor_full , 
  label = response_full , 
  params = hyperparm_final , 
  nrounds = nrounds , 
  objective = "reg:squarederror" , 
  verbose = FALSE ,  # silent
  early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)

# =====================================
# cross validation
# =====================================
# (can also use xgboost::xgb.cv)
# Here I use my own CV-loop for consistency with the other model types 

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
  xgb_train <- xgboost(
    data = predictor_train , 
    label = response_train , 
    params = hyperparm_final , 
    nrounds = nrounds , 
    objective = "reg:squarederror" , 
    verbose = FALSE ,  # silent
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
  # prediction
  prediction_test <- testing.data %>% 
    select(month , Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = predict(xgb_train , predictor_test))
  # prediction data.frame
  if(as.character(k) == "1"){
    xgb_final_prediction_CV <- prediction_test # <-
  }else{ # append
    xgb_final_prediction_CV <- bind_rows(xgb_final_prediction_CV , prediction_test)
  }
  # clean environment
  rm(xgb_train)
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
  xgb_train <- xgboost(
    data = predictor_train , 
    label = response_train , 
    params = hyperparm_final , 
    nrounds = nrounds , 
    objective = "reg:squarederror" , 
    verbose = FALSE ,  # silent
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
  # prediction
  prediction_test <- testing.data %>% 
    select(month , Station_name , NO2 , Type_of_station , spatial_CV) %>% 
    # prediction
    mutate(predicted = predict(xgb_train , predictor_test))
  # prediction data.frame
  if(as.character(k) == "1"){
    xgb_final_prediction_CV_sp <- prediction_test # <-
  }else{ # append
    xgb_final_prediction_CV_sp <- bind_rows(xgb_final_prediction_CV_sp , prediction_test)
  }
  # clean environment
  rm(xgb_train)
  rm(k , training.data , testing.data , predictor_train , predictor_test , response_train , response_test , prediction_test)
}

# temporal cross validation
for(k in unique(data_monthly$month)){
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
  xgb_train <- xgboost(
    data = predictor_train , 
    label = response_train , 
    params = hyperparm_final , 
    nrounds = nrounds , 
    objective = "reg:squarederror" , 
    verbose = FALSE ,  # silent
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
  # prediction
  prediction_test <- testing.data %>% 
    select(month , Station_name , NO2 , Type_of_station) %>% 
    # prediction
    mutate(predicted_temporalCV = predict(xgb_train , predictor_test))
  # prediction data.frame
  if(as.character(k) == "1"){
    xgb_final_prediction_CV_tp <- prediction_test # <-
  }else{ # append
    xgb_final_prediction_CV_tp <- bind_rows(xgb_final_prediction_CV_tp , prediction_test)
  }
  # clean environment
  rm(xgb_train)
  rm(k , training.data , testing.data , predictor_train , predictor_test , response_train , response_test , prediction_test)
}


# combine the prediction of the three CV
xgb_final_prediction_CV <- xgb_final_prediction_CV %>% 
  left_join(xgb_final_prediction_CV_sp , 
            by = c("month" , "Station_name" , "NO2" , "Type_of_station") , 
            suffix = c("_CV" , "_spatialCV")) %>% 
  left_join(xgb_final_prediction_CV_tp , 
            by = c("month" , "Station_name" , "NO2" , "Type_of_station"))


# //////////////////////////////////////////////////////////////////////////
# model evaluation
# //////////////////////////////////////////////////////////////////////////
# =====================================
# observed, predicted, residuals
# =====================================
xgb_final_prediction <- data_monthly %>% 
  select(month , Station_name , NO2 , Type_of_station , X , Y) %>% 
  # prediction
  mutate(predicted = predict(xgb_final_full , predictor_full)) %>% 
  # CV-prediction
  full_join(xgb_final_prediction_CV , 
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
xgb_final_indices <- xgb_final_prediction %>%  
  eval_performance_indices()

# =====================================
# variable importance
# =====================================
# Gain: the relative contribution of the corresponding feature to the model 
# calculated by taking each feature’s contribution for each tree in the model.
# This is synonymous with gbm’s relative.influence
xgb_final_importance <- xgb_final_full %>% 
  xgb.importance(model = .)

# =====================================
# visualization
# =====================================
out_dirpath_plots <- sprintf("3_results/output-graph/model_monthly/%s" , model_abbr)
if(!dir.exists(out_dirpath_plots)) dir.create(out_dirpath_plots)

# predicted <-> observed
plot_obs_pred(xgb_final_prediction , # <-
              sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/obs-pred_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 5 , base_height = 3.5
)

# residual diagnostic plots
plot_resid(xgb_final_prediction , # <-
           title_text = sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/residuals_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 7.8 , base_height = 6
)

# residuals by month
plot_resid_month(xgb_final_prediction , 
                 subtitle_text = sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/residuals-month_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 6 , base_height = 3
)

# variable importance
xgb_final_importance %>% 
  arrange(-Gain) %>% 
  # re-order for visualization
  mutate(Feature = factor(Feature , levels = Feature[order(Gain)])) %>% 
  # dplyr::slice(1:30) %>% 
  ggplot(aes(x = Feature , y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variables" , y = "Gain" , 
       title = sprintf("GBM variable importance (with %s)" , SAT_product) , 
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
moran_month_df <- eval_resid_moran(xgb_final_prediction , by_time = TRUE , col_time = "month")
moran_mean_df <- eval_resid_moran(xgb_final_prediction %>% drop_na() , 
                                  by_time = FALSE)

# visualization
plot_resid_map(xgb_final_prediction , # <-
               sprintf("%s (%s)" , str_to_title(model_name) , SAT_product)) +
  scale_color_gradient2(low = "dodgerblue3" , mid = "white" , high = "deeppink3" , 
                        #oob = scales::squish , 
                        limits = c(-5,5))
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
  xgb_final_prediction %>% # <-
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_predicted , model_abbr , SAT_product))
  
  # export the model performance indices
  out_dirpath_indices <- "3_results/output-data/model_monthly/indices"
  if(!dir.exists(out_dirpath_indices)) dir.create(out_dirpath_indices)
  xgb_final_indices %>% # <- 
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
  saveRDS(xgb_final_full , # <-
          file = sprintf("%s/%s_%s.rds" , out_dirpath_model , model_abbr , SAT_product))
}

#####################################################################################################
# Masterarbeit
# Modeling
# Daily model: Neural network
# 2021-08-03
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
library(keras)
library(nnet) ; library(NeuralNetTools)

source("2_scripts/utils_model-eval.R")


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

# DNN hyperparameters
# of the MONTHLY model
hyper_evaluation <- read_csv(
  "3_results/output-data/model_monthly/NN_grid-search/hyper_evaluation.csv", 
  col_types = cols(layers = col_integer(), 
                   neurons = col_integer(), epochs = col_integer(), 
                   batch.size = col_integer(), regularization = col_integer())
)

# non-predictor columns
columns_nonpredictor <- c("Station_name" , "NO2" , "Type_of_zone" , "Type_of_station" , 
                          "Altitude" , "Canton_ID" , "Canton_name" , "X" , "Y" , 
                          "CV" , "spatial_CV" , "date")


# //////////////////////////////////////////////////////////////////////////
# naming the model
# //////////////////////////////////////////////////////////////////////////
model_name <- "Neural network"
model_abbr <- "NN"
SAT_product <- c("OMI" , "TROPOMI")[2]

# subset data: satellite-product
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


# //////////////////////////////////////////////////////////////////////////
# model development
# //////////////////////////////////////////////////////////////////////////
# =====================================
# modularized NN model definition
# =====================================
source("2_scripts/utils_define-NN.R")

# =====================================
# feature selection
# =====================================
# single-hidden-layer neural network for screening
set.seed(20210803)
NN_screen <- nnet(
  x = data_daily %>% 
    drop_na() %>% 
    select(-all_of(columns_nonpredictor)) %>% 
    # random-value variables
    mutate(R1 = runif(n()) , 
           R2 = runif(n()) ,
           R3 = runif(n()) ) , 
  y = data_daily %>% 
    drop_na() %>% 
    select(NO2) , 
  size = 50 , linout = TRUE , MaxNWts = 1e5
)

# variables importance using Garson's algorithm
NN_screen_importance <- NeuralNetTools::garson(NN_screen , bar_plot = FALSE) %>% 
  tibble(variables = row.names(.))
screen_plot <- NN_screen_importance %>% 
  # re-order for visualization
  mutate(variables = factor(variables , levels = variables[order(rel_imp)])) %>% 
  # random 
  mutate(class = ifelse(str_detect(variables , "R[123]") , "Random" , "Predictor variables")) %>% 
  # visualization
  ggplot(aes(x = variables , y = rel_imp , fill = class)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_lancet() +
  labs(x = "Variables" , y = "Variable importance" , 
       title = "Screening of relevant predictor variables" , 
       subtitle = "Relative importance of input variables in neural networks \nusing Garson's algorithm") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 4) , legend.position = "bottom")
screen_plot

# variable selection
included_var_garson <- NN_screen_importance %>% 
  filter(rel_imp > NN_screen_importance %>% 
           filter(str_detect(variables , "R[123]")) %>% 
           # the max importance of the random-value variables 
           summarize(rel_imp = max(rel_imp)) %>% 
           unlist) %>% 
  select(variables) %>% 
  filter(!str_detect(variables , "R[123]")) %>% 
  unlist %>% unname

# =====================================
# hyperparameters
# =====================================
hyperparm_vector <- hyper_evaluation %>% 
  arrange(MAE_CV) %>% 
  dplyr::slice(9) %>% 
  select(-contains("_training") , -contains("_CV")) %>% 
  unlist


# =====================================
# model with the full training set
# =====================================
{ # data preparation
  training.data <- data_daily %>% 
    drop_na()
  # make matrix
  predictor_train <- training.data %>% 
    select(all_of(included_var_garson)) %>% 
    as.matrix()
  response_train <- training.data %>% 
    select(NO2) %>% 
    as.matrix()
}

# define model
set_random_seed(1010) # reproducibility for Keras
NN_define(hyperparm_vector = hyperparm_vector , n_var = ncol(predictor_train))
# train model
NN %>% 
  fit(predictor_train , response_train , 
      epoch = hyperparm_vector["epochs"] , 
      batch_size = hyperparm_vector["batch.size"] , 
      validation_split = 0.2 , 
      verbose = TRUE)
# prediction
NN_prediction_training <- training.data %>% 
  select(date , Station_name , NO2 , Type_of_station , X , Y) %>% 
  # prediction
  mutate(predicted = predict(NN , predictor_train)[,1])

# =====================================
# cross validation
# =====================================
# conventional CV
for(k in as.factor(1:k_fold)){
  # data preparation: partition
  training.data <- data_daily %>% 
    filter(CV != k) %>% 
    drop_na()
  testing.data <- data_daily %>% 
    filter(CV == k)
  # data preparation: make matrix
  predictor_train <- training.data %>% 
    select(all_of(included_var_garson)) %>% 
    as.matrix()
  response_train <- training.data %>% 
    select(NO2) %>% 
    as.matrix()
  predictor_test <- testing.data %>% 
    select(all_of(included_var_garson)) %>% 
    as.matrix()
  response_test <- testing.data %>% 
    select(NO2) %>% 
    as.matrix()
  # define model
  set_random_seed(1010) # reproducibility for Keras
  NN_define(hyperparm_vector , n_var = ncol(predictor_train))
  # train model
  NN %>% 
    fit(predictor_train , response_train , 
        epoch = hyperparm_vector["epochs"] , 
        batch_size = hyperparm_vector["batch.size"] , 
        validation_split = 0.2 , 
        verbose = FALSE)
  # prediction
  prediction_test <- testing.data %>% 
    select(date , Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = predict(NN , predictor_test)[,1])
  # prediction data.frame
  if(as.character(k) == "1"){
    NN_prediction_CV <- prediction_test # <-
  }else{ # append
    NN_prediction_CV <- bind_rows(NN_prediction_CV , prediction_test)
  }
  # clean environment
  rm(k , training.data , testing.data , predictor_train , predictor_test , response_train , response_test , prediction_test)
}

# spatial CV
for(k in as.factor(1:k_fold)){
  # data preparation: partition
  training.data <- data_daily %>% 
    filter(spatial_CV != k) %>% 
    drop_na()
  testing.data <- data_daily %>% 
    filter(spatial_CV == k) %>% 
    drop_na()
  # data preparation: make matrix
  predictor_train <- training.data %>% 
    select(all_of(included_var_garson)) %>% 
    as.matrix()
  response_train <- training.data %>% 
    select(NO2) %>% 
    as.matrix()
  predictor_test <- testing.data %>% 
    select(all_of(included_var_garson)) %>% 
    as.matrix()
  response_test <- testing.data %>% 
    select(NO2) %>% 
    as.matrix()
  # define model
  set_random_seed(1010) # reproducibility for Keras
  NN_define(hyperparm_vector , n_var = ncol(predictor_train))
  # train model
  NN %>% 
    fit(predictor_train , response_train , 
        epoch = hyperparm_vector["epochs"] , 
        batch_size = hyperparm_vector["batch.size"] , 
        validation_split = 0.2 , 
        verbose = FALSE)
  # prediction
  prediction_test <- testing.data %>% 
    select(date , Station_name , NO2 , Type_of_station , spatial_CV) %>% 
    # prediction
    mutate(predicted = predict(NN , predictor_test)[,1])
  # prediction data.frame
  if(as.character(k) == "1"){
    NN_prediction_CV_sp <- prediction_test # <-
  }else{ # append
    NN_prediction_CV_sp <- bind_rows(NN_prediction_CV_sp , prediction_test)
  }
  # clean environment
  rm(k , training.data , testing.data , predictor_train , predictor_test , response_train , response_test , prediction_test)
}

# temporal CV
for(k in 1:12){
  # data preparation: partition
  training.data <- data_daily %>% 
    mutate(month = month(date)) %>% 
    filter(month != k) %>% 
    drop_na()
  testing.data <- data_daily %>% 
    mutate(month = month(date)) %>% 
    filter(month == k) %>% 
    drop_na()
  # data preparation: make matrix
  predictor_train <- training.data %>% 
    select(all_of(included_var_garson)) %>% 
    as.matrix()
  response_train <- training.data %>% 
    select(NO2) %>% 
    as.matrix()
  predictor_test <- testing.data %>% 
    select(all_of(included_var_garson)) %>% 
    as.matrix()
  response_test <- testing.data %>% 
    select(NO2) %>% 
    as.matrix()
  # define model
  set_random_seed(1010) # reproducibility for Keras
  NN_define(hyperparm_vector , n_var = ncol(predictor_train))
  # train model
  NN %>% 
    fit(predictor_train , response_train , 
        epoch = hyperparm_vector["epochs"] , 
        batch_size = hyperparm_vector["batch.size"] , 
        validation_split = 0.2 , 
        verbose = FALSE)
  # prediction
  prediction_test <- testing.data %>% 
    select(date , Station_name , NO2 , Type_of_station) %>% 
    # prediction
    mutate(predicted_temporalCV = predict(NN , predictor_test)[,1])
  # prediction data.frame
  if(as.character(k) == "1"){
    NN_prediction_CV_tp <- prediction_test # <-
  }else{ # append
    NN_prediction_CV_tp <- bind_rows(NN_prediction_CV_tp , prediction_test)
  }
  # clean environment
  rm(k , training.data , testing.data , predictor_train , predictor_test , response_train , response_test , prediction_test)
}

# combine the prediction of the three CV
NN_prediction_CV <- NN_prediction_CV %>% 
  left_join(NN_prediction_CV_sp , 
            by = c("date" , "Station_name" , "NO2" , "Type_of_station") , 
            suffix = c("_CV" , "_spatialCV")) %>% 
  left_join(NN_prediction_CV_tp , 
            by = c("date" , "Station_name" , "NO2" , "Type_of_station"))

# //////////////////////////////////////////////////////////////////////////
# model evaluation
# //////////////////////////////////////////////////////////////////////////
# =====================================
# observed, predicted, residuals
# =====================================
NN_prediction <- NN_prediction_training %>% 
  # CV-prediction
  full_join(NN_prediction_CV , 
            by = c("Station_name" , "NO2" , "Type_of_station" , "date")) %>% 
  # residuals
  mutate(residual = NO2 - predicted , 
         residual_CV = NO2 - predicted_CV , 
         residual_spatialCV = NO2 - predicted_spatialCV , 
         residual_temporalCV = NO2 - predicted_temporalCV)

# =====================================
# performance indices
# =====================================
# model performance indices as a data.frame
NN_indices <- NN_prediction %>%   # <-
  eval_performance_indices()

# =====================================
# visualization
# =====================================
out_dirpath_plots <- sprintf("3_results/output-graph/model_daily/%s" , model_abbr)
if(!dir.exists(out_dirpath_plots)) dir.create(out_dirpath_plots)

# predicted <-> observed
plot_obs_pred(NN_prediction , # <-
              sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/obs-pred_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 5 , base_height = 3.5
)

# residual diagnostic plots
plot_resid(NN_prediction , # <-
           title_text = sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/residuals_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 7.8 , base_height = 6
)

# residuals by month
plot_resid_month(NN_prediction , 
                 subtitle_text = sprintf("%s (%s)" , str_to_title(model_name) , SAT_product) , 
                 daily = TRUE)
save_plot(
  sprintf("%s/residuals-month_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 6 , base_height = 3
)

# screening of relevant predictor variables 
save_plot(
  sprintf("%s/%s_screening_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = screen_plot , 
  base_width = 5 , base_height = 9
)

# =====================================
# spatial autocorrelation of the residuals
# =====================================
moran_month_df <- eval_resid_moran(NN_prediction , by_time = TRUE , col_time = "date")
any(moran_month_df$p < 0.05)
moran_mean_df <- eval_resid_moran(NN_prediction %>% drop_na() , by_time = FALSE)

# visualization
plot_resid_map(NN_prediction %>% drop_na() , # <-
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
  out_dirpath_predicted <- "3_results/output-data/model_daily/observed-predicted"
  if(!dir.exists(out_dirpath_predicted)) dir.create(out_dirpath_predicted)
  NN_prediction %>% # <-
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_predicted , model_abbr , SAT_product))
  
  # export the model performance indices
  out_dirpath_indices <- "3_results/output-data/model_daily/indices"
  if(!dir.exists(out_dirpath_indices)) dir.create(out_dirpath_indices)
  NN_indices %>% # <- 
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_indices , model_abbr , SAT_product))
  
  # Moran's I
  out_dirpath_Moran <- "3_results/output-data/model_daily/Moran"
  if(!dir.exists(out_dirpath_Moran)) dir.create(out_dirpath_Moran)
  moran_month_df %>% 
    pivot_longer(cols = -date) %>% 
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
# {
#   out_dirpath_model <- "3_results/output-model/model_monthly"
#   if(!dir.exists(out_dirpath_model)) dir.create(out_dirpath_model)
#   
#   saveRDS(NN , # <-
#           file = sprintf("%s/%s_%s.rds" , out_dirpath_model , model_abbr , SAT_product))
# }

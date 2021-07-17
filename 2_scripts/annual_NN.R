#####################################################################################################
# Masterarbeit
# Modeling
# Annual model: Neural network-- grid search of hyperparameters
# 2021-06-29
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

source("2_scripts/utils_model-eval.R")


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
  inner_join(sites_CV , by = "Station_name") %>% 
  select(-geometry)

# DNN hyperparameters
hyper_evaluation <- read_csv(
  "3_results/output-data/model_annual/NN_grid-search/hyper_evaluation.csv", 
  col_types = cols(layers = col_integer(), 
                   neurons = col_integer(), epochs = col_integer(), 
                   batch.size = col_integer(), regularization = col_integer())
)

# non-predictor columns
columns_nonpredictor <- c("Station_name" , "NO2" , "Type_of_zone" , "Type_of_station" , 
                          "Altitude" , "Canton_ID" , "Canton_name" , "X" , "Y" , 
                          "CV" , "spatial_CV")

# //////////////////////////////////////////////////////////////////////////
# naming the model
# //////////////////////////////////////////////////////////////////////////
model_name <- "Neural network"
model_abbr <- "NN"
SAT_product <- c("OMI" , "TROPOMI")[1]

# subset data: satellite-product
if(SAT_product == "OMI"){
  # for the OMI model: exclude TROPOMI and meteorological variables at 12H
  data_annual <- data_annual_raw %>% 
    select(-TROPOMI_NO2 , -ends_with("_12H")) 
}else if(SAT_product == "TROPOMI"){
  # for the TROPOMI model: exclude OMI and meteorological variables at 15H
  data_annual <- data_annual_raw %>% 
    select(-OMI_NO2 , -ends_with("_15H")) 
}

# //////////////////////////////////////////////////////////////////////////
# model development
# //////////////////////////////////////////////////////////////////////////
# =====================================
# modularized NN model definition
# =====================================
NN_define <- function(hyperparm_vector , n_var){
  # | hyperparm_vector: a named vector with the hyperparameters (layers, neurons, epochs, batch.size)
  # | n_var: number of input variables (for the input layer); ncol(predictor_matrix)
  # define model 
  NN <<- keras_model_sequential()
  # regularization
  if(is.na(hyperparm_vector["regularization"])){
    NN_regularizer <- NULL
  }else if(hyperparm_vector["regularization"] == 1){
    NN_regularizer <- regularizer_l1(l = 0.001)
  }else if(hyperparm_vector["regularization"] == 2) {
    NN_regularizer <- regularizer_l2(l = 0.001)
  }
  # define model layers
  if(hyperparm_vector["layers"] == 1){
    NN %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  input_shape = n_var , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dense(units = 1 , activation = "linear")
  }
  if(hyperparm_vector["layers"] == 2){
    NN %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  input_shape = n_var , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dense(units = 1 , activation = "linear")
  }
  if(hyperparm_vector["layers"] == 3){
    NN %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  input_shape = n_var , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dense(units = 1 , activation = "linear")
  }
  if(hyperparm_vector["layers"] == 4){
    NN %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  input_shape = n_var , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dense(units = 1 , activation = "linear")
  }
  # define loss and optimizer
  NN %>% 
    compile(
      loss = "mae" , 
      optimizer = optimizer_adam() , 
      metrics = list("mean_absolute_error")
    )
}

# =====================================
# hyperparameters
# =====================================
hyperparm_vector <- hyper_evaluation %>% 
  arrange(MAE_CV) %>% 
  dplyr::slice(4) %>% 
  select(-contains("_training") , -contains("_CV")) %>% 
  unlist

# layers        neurons         epochs     batch.size regularization 
#      4             15             50              5              1 

# hyperparm_vector <- c("layers" = 2 ,
#                       "neurons" = 100 ,
#                       "epochs" = 100 ,
#                       "batch.size" = 100 ,
#                       "regularization" = 2)
# =====================================
# model with the full training set
# =====================================
{ # data preparation
  training.data <- data_annual
  # # variable selection
  # included_var <- training.data %>% 
  #   select(-c(spatial_CV , Station_name , Type_of_zone , Type_of_station , 
  #             Altitude , Canton_ID , Canton_name , X , Y)) %>% 
  #   pivot_longer(cols = -NO2) %>% 
  #   group_by(name) %>% 
  #   summarize(cor = cor(NO2 , value)) %>% 
  #   ungroup %>% 
  #   filter(abs(cor) > 0.2) %>% 
  #   select(name) %>% unlist %>% unname
  # make matrix
  predictor_train <- training.data %>% 
    select(-all_of(columns_nonpredictor)) %>% 
    # select(all_of(included_var)) %>% 
    as.matrix()
  response_train <- training.data %>% 
    select(NO2) %>% 
    as.matrix()
}

# define model
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
  select(Station_name , NO2 , Type_of_station , X , Y) %>% 
  # prediction
  mutate(predicted = predict(NN , predictor_train)[,1])

# =====================================
# cross validation
# =====================================
# conventional CV
for(k in as.factor(1:k_fold)){
  # data preparation: partition
  training.data <- data_annual %>% 
    filter(CV != k)
  testing.data <- data_annual %>% 
    filter(CV == k)
  # data preparation: make matrix
  predictor_train <- training.data %>% 
    select(-all_of(columns_nonpredictor)) %>% 
    as.matrix()
  response_train <- training.data %>% 
    select(NO2) %>% 
    as.matrix()
  predictor_test <- testing.data %>% 
    select(-all_of(columns_nonpredictor)) %>% 
    as.matrix()
  response_test <- testing.data %>% 
    select(NO2) %>% 
    as.matrix()
  # define model
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
    select(Station_name , NO2 , Type_of_station , CV) %>% 
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
  training.data <- data_annual %>% 
    filter(spatial_CV != k)
  testing.data <- data_annual %>% 
    filter(spatial_CV == k)
  # data preparation: make matrix
  predictor_train <- training.data %>% 
    select(-all_of(columns_nonpredictor)) %>% 
    as.matrix()
  response_train <- training.data %>% 
    select(NO2) %>% 
    as.matrix()
  predictor_test <- testing.data %>% 
    select(-all_of(columns_nonpredictor)) %>% 
    as.matrix()
  response_test <- testing.data %>% 
    select(NO2) %>% 
    as.matrix()
  # define model
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
    select(Station_name , NO2 , Type_of_station , spatial_CV) %>% 
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

# combine the prediction of the two CV
NN_prediction_CV <- NN_prediction_CV %>% 
  left_join(NN_prediction_CV_sp , 
            by = c("Station_name" , "NO2" , "Type_of_station") , 
            suffix = c("_CV" , "_spatialCV"))

# //////////////////////////////////////////////////////////////////////////
# model evaluation
# //////////////////////////////////////////////////////////////////////////
# =====================================
# observed, predicted, residuals
# =====================================
NN_prediction <- NN_prediction_training %>% 
  # CV-prediction
  full_join(NN_prediction_CV , 
            by = c("Station_name" , "NO2" , "Type_of_station")) %>% 
  # residuals
  mutate(residual = NO2 - predicted , 
         residual_CV = NO2 - predicted_CV , 
         residual_spatialCV = NO2 - predicted_spatialCV)

# =====================================
# performance indices
# =====================================
# model performance indices as a data.frame
NN_indices <- NN_prediction %>%   # <-
  eval_performance_indices()

# =====================================
# visualization
# =====================================
out_dirpath_plots <- sprintf("3_results/output-graph/model_annual/%s" , model_abbr)
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

# =====================================
# spatial autocorrelation of the residuals
# =====================================
moran_df <- eval_resid_moran(NN_prediction)

# visualization
plot_resid_map(NN_prediction , # <-
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
  out_dirpath_predicted <- "3_results/output-data/model_annual/observed-predicted"
  if(!dir.exists(out_dirpath_predicted)) dir.create(out_dirpath_predicted)
  NN_prediction %>% # <-
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_predicted , model_abbr , SAT_product))
  
  # export the model performance indices
  out_dirpath_indices <- "3_results/output-data/model_annual/indices"
  if(!dir.exists(out_dirpath_indices)) dir.create(out_dirpath_indices)
  NN_indices %>% # <- 
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_indices , model_abbr , SAT_product))
  
  # Moran's I
  out_dirpath_Moran <- "3_results/output-data/model_annual/Moran"
  if(!dir.exists(out_dirpath_Moran)) dir.create(out_dirpath_Moran)
  moran_df %>% 
    pivot_longer(cols = everything()) %>% 
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_Moran , model_abbr , SAT_product))
}

# =====================================
# export model
# =====================================
{
  out_dirpath_model <- "3_results/output-model/model_annual"
  if(!dir.exists(out_dirpath_model)) dir.create(out_dirpath_model)
  saveRDS(NN , # <-
          file = sprintf("%s/%s_%s.rds" , out_dirpath_model , model_abbr , SAT_product))
}

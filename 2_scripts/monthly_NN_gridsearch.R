#####################################################################################################
# Masterarbeit
# Modeling
# Monthly model: Neural network-- grid search of hyperparameters
# 2021-07-20
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
library(ranger)
library(keras)

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
# subset data
# //////////////////////////////////////////////////////////////////////////
SAT_product <- "TROPOMI"
# for now I grid-search the model using TROPOMI and use the same hyperparameter set for OMI

# subset data: satellite-product
if(SAT_product == "OMI"){
  # for the OMI model: exclude TROPOMI and meteorological variables at 12H
  data_monthly <- data_monthly_raw %>% 
    select(-TROPOMI_NO2 , -ends_with("_12H")) 
}else if(SAT_product == "TROPOMI"){
  # for the TROPOMI model: exclude OMI and meteorological variables at 15H
  data_monthly <- data_monthly_raw %>% 
    select(-OMI_NO2 , -ends_with("_15H")) 
}

# //////////////////////////////////////////////////////////////////////////
# grid search for best model hyperparameters
# //////////////////////////////////////////////////////////////////////////
# =====================================
# modularized NN model definition
# =====================================
source("2_scripts/utils_define-NN.R")

# =====================================
# feature selection
# =====================================
# Spearman coefficient of correlation
included_var_cor <- data_monthly %>%
  select(-all_of(columns_nonpredictor) , NO2) %>%
  pivot_longer(cols = -NO2) %>%
  drop_na() %>%
  group_by(name) %>%
  summarize(cor = cor(NO2 , value , method = "spearman") ,
            cor_pearson = cor(NO2 , value , method = "pearson")) %>%
  ungroup() %>%
  mutate(cor_abs = abs(cor)) %>%
  arrange(-cor_abs) %>%
  filter(cor_abs > 0.1) %>%
  select(name) %>% unlist %>% unname

# random forest
included_var_RF <- readRDS(sprintf("3_results/output-model/model_monthly/RF_%s.rds" , SAT_product)) %>%
  ranger::importance() %>%
  names()

# no selection
included_var_all <- colnames(data_monthly)[!colnames(data_monthly) %in% columns_nonpredictor]

# =====================================
# create hyperparameter grid
# =====================================
hyper_grid <- expand.grid(
  layers = c(1,2,3,4) ,
  neurons = c(5,10,20,50,100,200) ,
  epochs = c(25,50) , 
  batch.size = c(3,5,10,20,100) , 
  regularization = c(NA , 1 , 2) , # 1 for L1 and 2 for L2
  regularization_factor = 0.001 ,
  selection = c(0 , 1 , 2) # 0 for no selection, 1 for cor, 2 for RF
) 

# =====================================
# grid search
# =====================================
pb <- txtProgressBar(min = 1 , max = nrow(hyper_grid) , style = 3 )
for(i in 1:nrow(hyper_grid)){
  # hyperparameters
  hyper_i <- hyper_grid %>% 
    slice(i) %>% 
    unlist
  # variable selection
  if(hyper_i["selection"] == 0){
    included_var <- included_var_all
  }else if(hyper_i["selection"] == 1){
    included_var <- included_var_cor
  }else if(hyper_i["selection"] == 2){
    included_var <- included_var_RF
  }
  # =====================================
  # full training set
  # =====================================
  training.data <- data_monthly %>% 
    drop_na()
  # make matrix
  predictor_train <- training.data %>% 
    # select(-all_of(columns_nonpredictor)) %>%
    select(one_of(included_var)) %>% 
    as.matrix()
  response_train <- training.data %>% 
    select(NO2) %>% 
    as.matrix()
  # define model
  NN_define(hyper_i , n_var = ncol(predictor_train))
  # train model
  NN %>% 
    fit(predictor_train , response_train , 
        epoch = hyper_i["epochs"] , 
        batch_size = hyper_i["batch.size"] , 
        validation_split = 0.2 , 
        verbose = FALSE)
  # prediction
  prediction_training <- training.data %>% 
    select(month , Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = predict(NN , predictor_train)[,1])
  # clean environment
  rm(training.data , predictor_train , response_train , NN)
  # =====================================
  # cross validation
  # =====================================
  for(k in as.factor(1:k_fold)){
    # data preparation: partition
    training.data <- data_monthly %>% 
      filter(CV != k) %>% 
      drop_na()
    testing.data <- data_monthly %>% 
      filter(CV == k) %>% 
      drop_na()
    # data preparation: make matrix
    predictor_train <- training.data %>% 
      # select(-all_of(columns_nonpredictor)) %>% 
      select(all_of(included_var)) %>% 
      as.matrix()
    response_train <- training.data %>% 
      select(NO2) %>% 
      as.matrix()
    predictor_test <- testing.data %>% 
      # select(-all_of(columns_nonpredictor)) %>% 
      select(all_of(included_var)) %>% 
      as.matrix()
    response_test <- testing.data %>% 
      select(NO2) %>% 
      as.matrix()
    # define model
    NN_define(hyper_i , n_var = ncol(predictor_train))
    # train model
    NN %>% 
      fit(predictor_train , response_train , 
          epoch = hyper_i["epochs"] , 
          batch_size = hyper_i["batch.size"] , 
          validation_split = 0.2 , 
          verbose = FALSE)
    # prediction
    prediction_test <- testing.data %>% 
      select(month , Station_name , NO2 , Type_of_station , CV) %>% 
      # prediction
      mutate(predicted = predict(NN , predictor_test)[,1])
    # prediction data.frame
    if(as.character(k) == "1"){
      prediction_CV <- prediction_test # <-
    }else{ # append
      prediction_CV <- bind_rows(prediction_CV , prediction_test)
    }
    # clean environment
    rm(k , training.data , testing.data , 
       predictor_train , predictor_test , response_train , response_test , 
       prediction_test)
  }
  # =====================================
  # evaluate
  # =====================================
  if(i == 1){ # hyper_evaluation as the output of the grid search
    hyper_evaluation <- hyper_i %>% as.list() %>% as_tibble() %>% 
      bind_cols(
        prediction_training %>% 
          full_join(prediction_CV , 
                    by = c("Station_name" , "NO2" , "Type_of_station" , "CV" , "month") , 
                    suffix = c("" , "_CV")) %>% 
          # calculate the indices from the observed and predicted values 
          summarize(MSE_training = mse(NO2 , predicted) , 
                    MSE_CV = mse(NO2 , predicted_CV) , 
                    MAE_training = mae(NO2 , predicted) ,
                    MAE_CV = mae(NO2 , predicted_CV) ,
                    R2_training = cor(NO2 , predicted)^2 ,
                    R2_CV = cor(NO2 , predicted_CV)^2 )
      )
  }else{ # append
    hyper_evaluation <- hyper_evaluation %>% 
      bind_rows(
        hyper_i %>% as.list() %>% as_tibble() %>% 
          bind_cols(
            prediction_training %>% 
              full_join(prediction_CV , 
                        by = c("Station_name" , "NO2" , "Type_of_station" , "CV" , "month") , 
                        suffix = c("" , "_CV")) %>% 
              # calculate the indices from the observed and predicted values 
              summarize(MSE_training = mse(NO2 , predicted) , 
                        MSE_CV = mse(NO2 , predicted_CV) , 
                        MAE_training = mae(NO2 , predicted) ,
                        MAE_CV = mae(NO2 , predicted_CV) ,
                        R2_training = cor(NO2 , predicted)^2 ,
                        R2_CV = cor(NO2 , predicted_CV)^2 )
          )
      )
  }
  # progress bar
  setTxtProgressBar(pb,i)
  # clean environment
  rm(hyper_i , included_var , NN , prediction_training , prediction_CV)
}
rm(pb,i)

# =====================================
# export grid search results
# =====================================
out_dirpath_grid_search <- "3_results/output-data/model_monthly/NN_grid-search"
if(!dir.exists(out_dirpath_grid_search)) dir.create(out_dirpath_grid_search)
hyper_evaluation %>% 
  write_csv(sprintf("%s/hyper_evaluation.csv" , out_dirpath_grid_search))



# //////////////////////////////////////////////////////////////////////////
# evaluate the grid search result
# //////////////////////////////////////////////////////////////////////////
# =====================================
# hyperparameters with the lowest CV-loss and highest CV-R2
# =====================================
hyper_evaluation %>% 
  arrange(-R2_CV) 
hyper_evaluation %>% 
  arrange(MAE_CV)

# =====================================
# see the effect of neurons, layers, regularizations
# =====================================
# MAE loss
hyper_evaluation %>% 
  pivot_longer(cols = c(ends_with("_training") , ends_with("CV")) , 
               names_to = c("index" , "type") , names_sep = "_") %>% 
  # only look at the loss
  filter(index == "MAE") %>% 
  mutate(regularization = ifelse(is.na(regularization) , "none" , paste0("L" , regularization))) %>% 
  # re-order for visualization
  mutate(type = factor(type , levels = c("training" , "CV"))) %>% 
  # visualization
  ggplot(aes(x = neurons , y = value , color = type)) +
  geom_line(aes(linetype = regularization)) +
  geom_point(aes(shape = regularization)) +
  facet_grid(batch.size + epochs ~ layers , labeller = label_both) +
  labs(x = "Number of neurons in each hidden layer" , y = "MAE loss" , 
       title = "Grid search on DNN hyperparameters") +
  coord_cartesian(ylim = c(2,6)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 8))

# R2
hyper_evaluation %>% 
  pivot_longer(cols = c(ends_with("_training") , ends_with("CV")) , 
               names_to = c("index" , "type") , names_sep = "_") %>% 
  # only look at the loss
  filter(index == "R2") %>% 
  mutate(regularization = ifelse(is.na(regularization) , "none" , paste0("L" , regularization))) %>% 
  # re-order for visualization
  mutate(type = factor(type , levels = c("training" , "CV"))) %>% 
  # visualization
  ggplot(aes(x = neurons , y = value , color = type)) +
  geom_line(aes(linetype = regularization)) +
  geom_point(aes(shape = regularization)) +
  facet_grid(batch.size + epochs ~ layers , labeller = label_both) +
  labs(x = "Number of neurons in each hidden layer" , y = expression("R"^2) , 
       title = "Grid search on DNN hyperparameters") +
  coord_cartesian(ylim = c(0.4,0.88)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 8))

# MSE

# =====================================
# see the effect of layers, neurons, regularizations
# =====================================
# MAE loss
hyper_evaluation %>% 
  pivot_longer(cols = c(ends_with("_training") , ends_with("CV")) , 
               names_to = c("index" , "type") , names_sep = "_") %>% 
  # only look at the loss
  filter(index == "MAE") %>% 
  mutate(regularization = ifelse(is.na(regularization) , "none" , paste0("L" , regularization))) %>% 
  # re-order for visualization
  mutate(type = factor(type , levels = c("training" , "CV"))) %>% 
  # visualization
  ggplot(aes(x = layers , y = value , color = type)) +
  geom_line(aes(linetype = regularization)) +
  geom_point(aes(shape = regularization)) +
  facet_grid(batch.size + epochs ~ neurons , labeller = label_both) +
  labs(x = "Number of hidden layers" , y = "MAE loss" , 
       title = "Grid search on DNN hyperparameters") +
  coord_cartesian(ylim = c(2,6)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 8))

# R2
hyper_evaluation %>% 
  pivot_longer(cols = c(ends_with("_training") , ends_with("CV")) , 
               names_to = c("index" , "type") , names_sep = "_") %>% 
  # only look at the loss
  filter(index == "R2") %>% 
  mutate(regularization = ifelse(is.na(regularization) , "none" , paste0("L" , regularization))) %>% 
  # re-order for visualization
  mutate(type = factor(type , levels = c("training" , "CV"))) %>% 
  # visualization
  ggplot(aes(x = layers , y = value , color = type)) +
  geom_line(aes(linetype = regularization)) +
  geom_point(aes(shape = regularization)) +
  facet_grid(batch.size + epochs ~ neurons , labeller = label_both) +
  labs(x = "Number of hidden layers" , y = expression("R"^2) , 
       title = "Grid search on DNN hyperparameters") +
  coord_cartesian(ylim = c(0.4,0.88)) +
  theme_bw() +
  theme(strip.text.y = element_text(size = 8))

# MSE

# =====================================
# see the effect of epochs
# =====================================
# MAE loss
hyper_evaluation %>% 
  pivot_longer(cols = c(ends_with("_training") , ends_with("CV")) , 
               names_to = c("index" , "type") , names_sep = "_") %>% 
  # only look at the loss
  filter(index == "MAE") %>% 
  mutate(regularization = ifelse(is.na(regularization) , "none" , paste0("L" , regularization))) %>% 
  # re-order for visualization
  mutate(type = factor(type , levels = c("training" , "CV"))) %>% 
  # visualization
  ggplot(aes(x = epochs , y = value , color = type )) +
  geom_line(aes(linetype = regularization)) +
  geom_point(aes(shape = regularization)) +
  facet_grid(batch.size ~ layers + neurons , scales = "free_y" , 
             labeller = label_both) +
  labs(x = "Number of epochs" , y = "MAE loss" , 
       title = "Grid search on DNN hyperparameters") +
  coord_cartesian(ylim = c(2,10)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90 , vjust = 0.5 , hjust = 1) , 
        strip.text.x = element_text(size = 8))

# R2
hyper_evaluation %>% 
  pivot_longer(cols = c(ends_with("_training") , ends_with("CV")) , 
               names_to = c("index" , "type") , names_sep = "_") %>% 
  # only look at R2
  filter(index == "R2") %>% 
  mutate(regularization = ifelse(is.na(regularization) , "none" , paste0("L" , regularization))) %>% 
  # re-order for visualization
  mutate(type = factor(type , levels = c("training" , "CV"))) %>% 
  # visualization
  ggplot(aes(x = epochs , y = value , color = type )) +
  geom_line(aes(linetype = regularization)) +
  geom_point(aes(shape = regularization)) +
  facet_grid(batch.size ~ layers + neurons , scales = "free_y" , 
             labeller = label_both) +
  labs(x = "Number of epochs" , y = expression("R"^2) , 
       title = "Grid search on DNN hyperparameters") +
  coord_cartesian(ylim = c(0.4,0.88)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90 , vjust = 0.5 , hjust = 1) , 
        strip.text.x = element_text(size = 8))

# =====================================
# see the effect of batch sizes
# =====================================
# MAE loss
hyper_evaluation %>% 
  pivot_longer(cols = c(ends_with("_training") , ends_with("CV")) , 
               names_to = c("index" , "type") , names_sep = "_") %>% 
  # only look at the loss
  filter(index == "MAE") %>% 
  mutate(regularization = ifelse(is.na(regularization) , "none" , paste0("L" , regularization))) %>% 
  # re-order for visualization
  mutate(type = factor(type , levels = c("training" , "CV"))) %>% 
  # visualization
  ggplot(aes(x = batch.size , y = value , color = type )) +
  geom_line(aes(linetype = regularization)) +
  geom_point(aes(shape = regularization)) +
  facet_grid(epochs ~ layers + neurons , scales = "free_y" , 
             labeller = label_both) +
  labs(x = "Batch size" , y = "MAE loss" , 
       title = "Grid search on DNN hyperparameters") +
  coord_cartesian(ylim = c(2,10)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90 , vjust = 0.5 , hjust = 1) , 
        strip.text.x = element_text(size = 8))

# R2
hyper_evaluation %>% 
  pivot_longer(cols = c(ends_with("_training") , ends_with("CV")) , 
               names_to = c("index" , "type") , names_sep = "_") %>% 
  # only look at R2
  filter(index == "R2") %>% 
  mutate(regularization = ifelse(is.na(regularization) , "none" , paste0("L" , regularization))) %>% 
  # re-order for visualization
  mutate(type = factor(type , levels = c("training" , "CV"))) %>% 
  # visualization
  ggplot(aes(x = batch.size , y = value , color = type )) +
  geom_line(aes(linetype = regularization)) +
  geom_point(aes(shape = regularization)) +
  facet_grid(epochs ~ layers + neurons , scales = "free_y" , 
             labeller = label_both) +
  labs(x = "Batch size" , y = expression("R"^2) , 
       title = "Grid search on DNN hyperparameters") +
  coord_cartesian(ylim = c(0.4,0.88)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90 , vjust = 0.5 , hjust = 1) , 
        strip.text.x = element_text(size = 8))


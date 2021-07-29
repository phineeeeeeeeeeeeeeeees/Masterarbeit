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
library(ggplot2) ; library(ggsci) ; library(ggthemes)
library(lubridate) ; library(stringr)
library(spdep)
library(Metrics)
library(keras)
library(nnet) ; library(NeuralNetTools)
library(parallel) ; library(pbapply)

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

# non-predictor columns
columns_nonpredictor <- c("Station_name" , "NO2" , "Type_of_zone" , "Type_of_station" , 
                          "Altitude" , "Canton_ID" , "Canton_name" , "X" , "Y" , 
                          "CV" , "spatial_CV")

# //////////////////////////////////////////////////////////////////////////
# subset data
# //////////////////////////////////////////////////////////////////////////
SAT_product <- "TROPOMI"
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
# grid search for best model hyperparameters
# //////////////////////////////////////////////////////////////////////////
# =====================================
# modularized NN model definition
# =====================================
source("2_scripts/utils_define-NN.R")

# =====================================
# feature selection
# =====================================
# single-hidden-layer neural network for screening
set.seed(20210727)
NN_screen <- nnet(
  x = data_annual %>% 
    drop_na() %>% 
    select(-all_of(columns_nonpredictor)) %>% 
    # random-value variables
    mutate(R1 = runif(n()) , 
           R2 = runif(n()) ,
           R3 = runif(n()) ) , 
  y = data_annual %>% 
    drop_na() %>% 
    select(NO2) , 
  size = 10 , linout = TRUE , MaxNWts = 1e4
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
# no selection
included_var_all <- colnames(data_annual)[!colnames(data_annual) %in% columns_nonpredictor]

# =====================================
# create hyperparameter grid
# =====================================
hyper_grid <- expand.grid(
  layers = c(1,2,3,4) ,
  neurons = c(5,10,20,30,50,100) ,
  epochs = c(25,50) , 
  batch.size = c(3,5,10,20) , 
  regularization = c(NA , 1 , 2) , # 1 for L1 and 2 for L2
  regularization_factor = 0.001 , 
  garson_selection = c(NA,1) , # NA for no selection; 1 for selection using Garson importance
  dropout_rate = c(0 , 0.1 , 0.2)
)

# =====================================
# grid search
# =====================================
# parallel computation
cpu.cores <- detectCores()
cl <- makeCluster(cpu.cores)
# clusterExport
# 在使用預設PSOCK cluster時，若想要讓所有的節點可以使用預先設定好的全域變數，必須以clusterExport將變數傳遞至所有節點，之後才能在各個節點中使用
clusterExport(
  cl = cl , 
  varlist = c("data_annual" , "k_fold" , "included_var_all" , "included_var_garson" , "NN_define")
)
# grid search function for "apply"
search_hyper_grid <- function(hyper_grid_row){
  # hyper_grid_row is the hyperparameter (one row of "hyper_grid")
  # load packages for the clusters
  library(dplyr) ; library(tidyr) ; 
  library(keras) ; library(tensorflow)
  library(Metrics)
  # =====================================
  # feature selection
  # =====================================
  if(is.na(hyper_grid_row["garson_selection"])){
    included_var <- included_var_all
  }else if(hyper_grid_row["garson_selection"] == 1){
    included_var <- included_var_garson
  }
  # =====================================
  # full training set
  # =====================================
  training.data <- data_annual %>% 
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
  set_random_seed(1010) # reproducibility for Keras
  NN_define(hyper_grid_row , n_var = ncol(predictor_train))
  # train model
  NN %>% 
    fit(predictor_train , response_train , 
        epoch = hyper_grid_row["epochs"] , 
        batch_size = hyper_grid_row["batch.size"] , 
        validation_split = 0.2 , 
        verbose = FALSE)
  # prediction
  prediction_training <- training.data %>% 
    select(Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = predict(NN , predictor_train)[,1])
  # # clean environment
  # rm(training.data , predictor_train , response_train , NN)
  # =====================================
  # cross validation
  # =====================================
  for(k in as.factor(1:k_fold)){
    # data preparation: partition
    training.data <- data_annual %>% 
      filter(CV != k) %>% 
      drop_na()
    testing.data <- data_annual %>% 
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
    set_random_seed(1010) # reproducibility for Keras
    NN_define(hyper_grid_row , n_var = ncol(predictor_train))
    # train model
    NN %>% 
      fit(predictor_train , response_train , 
          epoch = hyper_grid_row["epochs"] , 
          batch_size = hyper_grid_row["batch.size"] , 
          validation_split = 0.2 , 
          verbose = FALSE)
    # prediction
    prediction_test <- testing.data %>% 
      select(Station_name , NO2 , Type_of_station , CV) %>% 
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
  hyper_evaluation_i <- hyper_grid_row %>% 
    as.list() %>% as_tibble() %>% 
    bind_cols(
      prediction_training %>% 
        full_join(prediction_CV , 
                  by = c("Station_name" , "NO2" , "Type_of_station" , "CV") , 
                  suffix = c("" , "_CV")) %>% 
        # calculate the indices from the observed and predicted values 
        summarize(MSE_training = mse(NO2 , predicted) , 
                  MSE_CV = mse(NO2 , predicted_CV) , 
                  MAE_training = mae(NO2 , predicted) ,
                  MAE_CV = mae(NO2 , predicted_CV) ,
                  R2_training = cor(NO2 , predicted)^2 ,
                  R2_CV = cor(NO2 , predicted_CV)^2 )
    )
  
  # function return
  return(hyper_evaluation_i)
}
# grid search
hyper_evaluation <- hyper_grid %>% 
  pbapply(MARGIN = 1 , FUN = search_hyper_grid , cl = cl) %>% 
  bind_rows()

# turn off cluster
stopCluster(cl)


# =====================================
# export grid search results
# =====================================
out_dirpath_grid_search <- "3_results/output-data/model_annual/NN_grid-search"
if(!dir.exists(out_dirpath_grid_search)) dir.create(out_dirpath_grid_search)
hyper_evaluation %>% 
  write_csv(sprintf("%s/hyper_evaluation.csv" , out_dirpath_grid_search))

cowplot::save_plot(
  sprintf("%s/garson_screening.png" , out_dirpath_grid_search) , 
  plot = screen_plot ,
  base_width = 6 , base_height = 10
)


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


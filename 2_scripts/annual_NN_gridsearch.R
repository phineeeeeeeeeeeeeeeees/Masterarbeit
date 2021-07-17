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
# create hyperparameter grid
# =====================================
hyper_grid <- expand.grid(
  layers = c(1,2,3,4) ,
  neurons = c(5,10,15,20,30,50,100) ,
  epochs = c(30,50) , 
  batch.size = c(3,5,10,20,30) , 
  regularization = c(NA , 1 , 2) # 1 for L1 and 2 for L2
)

# =====================================
# modularized NN model definition
# =====================================
NN_grid_define <- function(hyperparm_vector , n_var){
  # | hyperparm_vector: a named vector with the hyperparameters (layers, neurons, epochs, batch.size)
  # | n_var: number of input variables (for the input layer); ncol(predictor_matrix)
  # define model 
  NN_grid <<- keras_model_sequential()
  # regularization
  if(is.na(hyperparm_vector["regularization"])){
    NN_grid_regularizer <- NULL
  }else if(hyperparm_vector["regularization"] == 1){
    NN_grid_regularizer <- regularizer_l1(l = 0.001)
  }else if(hyperparm_vector["regularization"] == 2) {
    NN_grid_regularizer <- regularizer_l2(l = 0.001)
  }
  # define model layers
  if(hyperparm_vector["layers"] == 1){
    NN_grid %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  input_shape = n_var , 
                  kernel_regularizer = NN_grid_regularizer) %>% 
      layer_dense(units = 1 , activation = "linear")
  }
  if(hyperparm_vector["layers"] == 2){
    NN_grid %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  input_shape = n_var , 
                  kernel_regularizer = NN_grid_regularizer) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_grid_regularizer) %>% 
      layer_dense(units = 1 , activation = "linear")
  }
  if(hyperparm_vector["layers"] == 3){
    NN_grid %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  input_shape = n_var , 
                  kernel_regularizer = NN_grid_regularizer) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_grid_regularizer) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_grid_regularizer) %>% 
      layer_dense(units = 1 , activation = "linear")
  }
  if(hyperparm_vector["layers"] == 4){
    NN_grid %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  input_shape = n_var , 
                  kernel_regularizer = NN_grid_regularizer) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_grid_regularizer) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_grid_regularizer) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_grid_regularizer) %>% 
      layer_dense(units = 1 , activation = "linear")
  }
  # define loss and optimizer
  NN_grid %>% 
    compile(
      loss = "mae" , 
      optimizer = optimizer_adam() , 
      metrics = list("mean_absolute_error")
    )
}

# =====================================
# grid search
# =====================================
pb <- txtProgressBar(min = 1 , max = nrow(hyper_grid) , style = 3 )
for(i in 1:nrow(hyper_grid)){
  # hyperparameters
  hyper_i <- hyper_grid %>% 
    slice(i) %>% 
    unlist
  # =====================================
  # full training set
  # =====================================
  training.data <- data_annual
  # make matrix
  predictor_train <- training.data %>% 
    select(-all_of(columns_nonpredictor)) %>% 
    as.matrix()
  response_train <- training.data %>% 
    select(NO2) %>% 
    as.matrix()
  # define model
  NN_grid_define(hyper_i , n_var = ncol(predictor_train))
  # train model
  NN_grid %>% 
    fit(predictor_train , response_train , 
        epoch = hyper_i["epochs"] , 
        batch_size = hyper_i["batch.size"] , 
        validation_split = 0.2 , 
        verbose = FALSE)
  # prediction
  prediction_training <- training.data %>% 
    select(Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = predict(NN_grid , predictor_train)[,1])
  # clean environment
  rm(training.data , predictor_train , response_train , NN_grid)
  # =====================================
  # cross validation
  # =====================================
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
    NN_grid_define(hyper_i , n_var = ncol(predictor_train))
    # train model
    NN_grid %>% 
      fit(predictor_train , response_train , 
          epoch = hyper_i["epochs"] , 
          batch_size = hyper_i["batch.size"] , 
          validation_split = 0.2 , 
          verbose = FALSE)
    # prediction
    prediction_test <- testing.data %>% 
      select(Station_name , NO2 , Type_of_station , CV) %>% 
      # prediction
      mutate(predicted = predict(NN_grid , predictor_test)[,1])
    # prediction data.frame
    if(as.character(k) == "1"){
      prediction_CV <- prediction_test # <-
    }else{ # append
      prediction_CV <- bind_rows(prediction_CV , prediction_test)
    }
    # clean environment
    rm(k , training.data , testing.data , predictor_train , predictor_test , response_train , response_test , prediction_test)
  }
  # =====================================
  # evaluate
  # =====================================
  if(i == 1){ # hyper_evaluation as the output of the grid search
    hyper_evaluation <- hyper_i %>% as.list() %>% as_tibble() %>% 
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
  }else{ # append
    hyper_evaluation <- hyper_evaluation %>% 
      bind_rows(
        hyper_i %>% as.list() %>% as_tibble() %>% 
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
      )
  }
  # progress bar
  setTxtProgressBar(pb,i)
  # clean environment
  rm(hyper_i , NN_grid , prediction_training , prediction_CV)
}
rm(pb,i)

# =====================================
# export grid search results
# =====================================
out_dirpath_grid_search <- "3_results/output-data/model_annual/NN_grid-search"
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


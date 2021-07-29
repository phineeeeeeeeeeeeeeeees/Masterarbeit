#####################################################################################################
# Masterarbeit
# Modeling
# Monthly model: Neural network-- feature selection
# 2021-07-23
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
library(keras) ; set_random_seed(1010) # reproducibility for Keras

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
# define model 
# //////////////////////////////////////////////////////////////////////////
source("2_scripts/utils_define-NN.R")

# benchmark hyperparameter
hyperparm_benchmark <- c("layers" = 1 , 
                         "neurons" = 100 , 
                         "epochs" = 50 , 
                         "batch.size" = 100 , 
                         "regularization" = NA , 
                         "regularization_factor" = NA , 
                         "dropout_rate" = 0.1)

# //////////////////////////////////////////////////////////////////////////
# different feature selection methods 
# //////////////////////////////////////////////////////////////////////////
# =====================================
# no selection
# =====================================
included_var_all <- colnames(data_monthly)[!colnames(data_monthly) %in% columns_nonpredictor]
# train and evaluate model
{
  included_var <- included_var_all
  # full training set -------------------------------------------
  training.data <- data_monthly %>% drop_na()
  # make matrix
  predictor_train <- training.data %>% select(one_of(included_var)) %>% as.matrix() # <-
  response_train <- training.data %>% select(NO2) %>% as.matrix()
  # define model
  NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
  # train model
  NN %>% 
    fit(predictor_train , response_train , 
        epoch = hyperparm_benchmark["epochs"] , 
        batch_size = hyperparm_benchmark["batch.size"] , 
        validation_split = 0.2 , 
        verbose = TRUE)
  # prediction
  prediction_training <- training.data %>% 
    select(month , Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = predict(NN , predictor_train)[,1])
  # clean environment
  rm(training.data , predictor_train , response_train , NN)
  # cross validation -------------------------------------------
  for(k in as.factor(1:k_fold)){
    # data preparation: partition
    training.data <- data_monthly %>% filter(CV != k) %>% drop_na()
    testing.data <- data_monthly %>% filter(CV == k) %>% drop_na()
    # data preparation: make matrix
    predictor_train <- training.data %>% select(all_of(included_var)) %>% as.matrix() # <-
    response_train <- training.data %>% select(NO2) %>% as.matrix()
    predictor_test <- testing.data %>% select(all_of(included_var)) %>% as.matrix() # <-
    response_test <- testing.data %>% select(NO2) %>% as.matrix()
    # define model
    NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
    # train model
    NN %>% 
      fit(predictor_train , response_train , 
          epoch = hyperparm_benchmark["epochs"] , 
          batch_size = hyperparm_benchmark["batch.size"] , 
          validation_split = 0.2 , 
          verbose = FALSE)
    # prediction
    prediction_test <- testing.data %>% 
      select(month , Station_name , NO2 , Type_of_station , CV) %>% 
      # prediction
      mutate(predicted = predict(NN , predictor_test)[,1])
    # prediction data.frame
    if(as.character(k) == "1"){
      prediction_CV <- prediction_test
    }else{ # append
      prediction_CV <- bind_rows(prediction_CV , prediction_test)
    }
    # clean environment
    rm(k , training.data , testing.data , 
       predictor_train , predictor_test , response_train , response_test , 
       prediction_test)
  }
  # evaluate  -------------------------------------------
  evaluation_all <- prediction_training %>%   # <-
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
}

evaluation_all

# =====================================
# filter: amount variation
# =====================================
# data_monthly %>% 
#   select(-all_of(columns_nonpredictor)) %>% 
#   pivot_longer(cols = everything() , names_to = "variable") %>% 
#   group_by(variable) %>% 
#   summarize(sd = sd(value , na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   # re-order for visualization
#   mutate(variable = factor(variable , levels = variable[order(sd)])) %>% 
#   # visualization
#   ggplot(aes(x = variable , y = sd)) +
#   geom_bar(stat = "identity")  +
#   coord_flip() +
#   labs(title = "Amount variation of the predictor variables" , 
#        subtitle = "Consider dropping the variables that have a very low variation?" , 
#        x = "Predictor variables" , y = "Standard deviation") +
#   theme_bw() +
#   theme(axis.text.y = element_text(size = 6))

# =====================================
# filter: Spearman coefficient of correlation
# =====================================
# not so successful in previous experiments
# Correlation criteria such as R(i) can only detect linear dependencies between variable and target
# Also it leads to the selection of a redundant subset
var_cor <- data_monthly %>%
  select(-all_of(columns_nonpredictor) , NO2) %>%
  pivot_longer(cols = -NO2) %>%
  drop_na() %>%
  group_by(name) %>%
  summarize(cor = cor(NO2 , value , method = "spearman")) %>%
  ungroup() %>%
  mutate(cor_abs = abs(cor))
included_var_cor <- var_cor %>%
  arrange(-cor_abs) %>%
  filter(cor_abs > 0.1) %>%
  select(name) %>% unlist %>% unname

# train and evaluate model
{
  included_var <- included_var_cor
  # full training set -------------------------------------------
  training.data <- data_monthly %>% drop_na()
  # make matrix
  predictor_train <- training.data %>% select(one_of(included_var)) %>% as.matrix() # <-
  response_train <- training.data %>% select(NO2) %>% as.matrix()
  # define model
  NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
  # train model
  NN %>% 
    fit(predictor_train , response_train , 
        epoch = hyperparm_benchmark["epochs"] , 
        batch_size = hyperparm_benchmark["batch.size"] , 
        validation_split = 0.2 , 
        verbose = TRUE)
  # prediction
  prediction_training <- training.data %>% 
    select(month , Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = predict(NN , predictor_train)[,1])
  # clean environment
  rm(training.data , predictor_train , response_train , NN)
  # cross validation -------------------------------------------
  for(k in as.factor(1:k_fold)){
    # data preparation: partition
    training.data <- data_monthly %>% filter(CV != k) %>% drop_na()
    testing.data <- data_monthly %>% filter(CV == k) %>% drop_na()
    # data preparation: make matrix
    predictor_train <- training.data %>% select(all_of(included_var)) %>% as.matrix() # <-
    response_train <- training.data %>% select(NO2) %>% as.matrix()
    predictor_test <- testing.data %>% select(all_of(included_var)) %>% as.matrix() # <-
    response_test <- testing.data %>% select(NO2) %>% as.matrix()
    # define model
    NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
    # train model
    NN %>% 
      fit(predictor_train , response_train , 
          epoch = hyperparm_benchmark["epochs"] , 
          batch_size = hyperparm_benchmark["batch.size"] , 
          validation_split = 0.2 , 
          verbose = FALSE)
    # prediction
    prediction_test <- testing.data %>% 
      select(month , Station_name , NO2 , Type_of_station , CV) %>% 
      # prediction
      mutate(predicted = predict(NN , predictor_test)[,1])
    # prediction data.frame
    if(as.character(k) == "1"){
      prediction_CV <- prediction_test
    }else{ # append
      prediction_CV <- bind_rows(prediction_CV , prediction_test)
    }
    # clean environment
    rm(k , training.data , testing.data , 
       predictor_train , predictor_test , response_train , response_test , 
       prediction_test)
  }
  # evaluate  -------------------------------------------
  evaluation_cor <- prediction_training %>%   # <-
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
}

evaluation_cor

# =====================================
# filter: cluster analysis
# =====================================
var_cluster <- data_monthly %>% 
  select(-all_of(columns_nonpredictor) , -month) %>% 
  # transpose
  t() %>% 
  # distance matix
  dist() %>% 
  # Ward Hierarchical Clustering
  hclust() %>% 
  # cut the trees with the desired number of clusters
  cutree(k = 50) %>% 
  as.list %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything() , names_to = "variable" , values_to = "cluster") %>% 
  # individual correlation
  left_join(var_cor , by = c("variable" = "name")) %>% 
  group_by(cluster) %>% 
  # select the variable in each cluster that has the highest correlation with the response
  group_modify(~ .x %>% slice(which.max(cor_abs))) %>% 
  ungroup

included_var_cluster <- var_cluster %>% 
  select(variable) %>% 
  unlist() %>% unname %>% c(. , "month")

# train and evaluate model
{
  included_var <- included_var_cluster
  # full training set -------------------------------------------
  training.data <- data_monthly %>% drop_na()
  # make matrix
  predictor_train <- training.data %>% select(one_of(included_var)) %>% as.matrix() # <-
  response_train <- training.data %>% select(NO2) %>% as.matrix()
  # define model
  NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
  # train model
  NN %>% 
    fit(predictor_train , response_train , 
        epoch = hyperparm_benchmark["epochs"] , 
        batch_size = hyperparm_benchmark["batch.size"] , 
        validation_split = 0.2 , 
        verbose = TRUE)
  # prediction
  prediction_training <- training.data %>% 
    select(month , Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = predict(NN , predictor_train)[,1])
  # clean environment
  rm(training.data , predictor_train , response_train , NN)
  # cross validation -------------------------------------------
  for(k in as.factor(1:k_fold)){
    # data preparation: partition
    training.data <- data_monthly %>% filter(CV != k) %>% drop_na()
    testing.data <- data_monthly %>% filter(CV == k) %>% drop_na()
    # data preparation: make matrix
    predictor_train <- training.data %>% select(all_of(included_var)) %>% as.matrix() # <-
    response_train <- training.data %>% select(NO2) %>% as.matrix()
    predictor_test <- testing.data %>% select(all_of(included_var)) %>% as.matrix() # <-
    response_test <- testing.data %>% select(NO2) %>% as.matrix()
    # define model
    NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
    # train model
    NN %>% 
      fit(predictor_train , response_train , 
          epoch = hyperparm_benchmark["epochs"] , 
          batch_size = hyperparm_benchmark["batch.size"] , 
          validation_split = 0.2 , 
          verbose = FALSE)
    # prediction
    prediction_test <- testing.data %>% 
      select(month , Station_name , NO2 , Type_of_station , CV) %>% 
      # prediction
      mutate(predicted = predict(NN , predictor_test)[,1])
    # prediction data.frame
    if(as.character(k) == "1"){
      prediction_CV <- prediction_test
    }else{ # append
      prediction_CV <- bind_rows(prediction_CV , prediction_test)
    }
    # clean environment
    rm(k , training.data , testing.data , 
       predictor_train , predictor_test , response_train , response_test , 
       prediction_test)
  }
  # evaluate  -------------------------------------------
  evaluation_cluster <- prediction_training %>%   # <-
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
}

evaluation_cluster

# =====================================
# filtering: Orthogonal greedy algorithm
# =====================================
var_OGA <- Ohit::OGA(
  data_monthly %>% 
    drop_na() %>% 
    select(-all_of(columns_nonpredictor)) %>% 
    as.matrix(), 
  data_monthly %>% 
    drop_na() %>% 
    select(NO2) %>% 
    unlist() %>% unname() , 
  c1 = 3
)

included_var_OGA <- data_monthly %>% 
  drop_na() %>% 
  select(-all_of(columns_nonpredictor)) %>% 
  select(var_OGA$J_OGA) %>% 
  names()

# train and evaluate model
{
  included_var <- included_var_OGA
  # full training set -------------------------------------------
  training.data <- data_monthly %>% drop_na()
  # make matrix
  predictor_train <- training.data %>% select(one_of(included_var)) %>% as.matrix() # <-
  response_train <- training.data %>% select(NO2) %>% as.matrix()
  # define model
  NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
  # train model
  NN %>% 
    fit(predictor_train , response_train , 
        epoch = hyperparm_benchmark["epochs"] , 
        batch_size = hyperparm_benchmark["batch.size"] , 
        validation_split = 0.2 , 
        verbose = TRUE)
  # prediction
  prediction_training <- training.data %>% 
    select(month , Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = predict(NN , predictor_train)[,1])
  # clean environment
  rm(training.data , predictor_train , response_train , NN)
  # cross validation -------------------------------------------
  for(k in as.factor(1:k_fold)){
    # data preparation: partition
    training.data <- data_monthly %>% filter(CV != k) %>% drop_na()
    testing.data <- data_monthly %>% filter(CV == k) %>% drop_na()
    # data preparation: make matrix
    predictor_train <- training.data %>% select(all_of(included_var)) %>% as.matrix() # <-
    response_train <- training.data %>% select(NO2) %>% as.matrix()
    predictor_test <- testing.data %>% select(all_of(included_var)) %>% as.matrix() # <-
    response_test <- testing.data %>% select(NO2) %>% as.matrix()
    # define model
    NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
    # train model
    NN %>% 
      fit(predictor_train , response_train , 
          epoch = hyperparm_benchmark["epochs"] , 
          batch_size = hyperparm_benchmark["batch.size"] , 
          validation_split = 0.2 , 
          verbose = FALSE)
    # prediction
    prediction_test <- testing.data %>% 
      select(month , Station_name , NO2 , Type_of_station , CV) %>% 
      # prediction
      mutate(predicted = predict(NN , predictor_test)[,1])
    # prediction data.frame
    if(as.character(k) == "1"){
      prediction_CV <- prediction_test
    }else{ # append
      prediction_CV <- bind_rows(prediction_CV , prediction_test)
    }
    # clean environment
    rm(k , training.data , testing.data , 
       predictor_train , predictor_test , response_train , response_test , 
       prediction_test)
  }
  # evaluate  -------------------------------------------
  evaluation_OGA <- prediction_training %>%   # <-
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
  }

evaluation_OGA

# =====================================
# filter/transform: PCA
# =====================================
transform_PCA <- data_monthly %>% 
  select(-all_of(columns_nonpredictor) , -month) %>% 
  prcomp()
str(transform_PCA)
transform_PCA$sdev %>% 
  as_tibble() %>% 
  mutate(PC = 1:n()) %>% 
  rename(sd = value) %>% 
  mutate(variance = sd^2) %>% 
  ggplot(aes(x = PC , y = variance)) +
  geom_line() +
  geom_point() +
  coord_cartesian(xlim = c(1,20)) +
  labs(x = "Principle components" , y = "Variance" , 
       title = "Screeplot" ,
       subtitle = "PCA of the predictor variables") +
  theme_bw()

data_monthly_PC <- predict(transform_PCA , data_monthly) %>% 
  as_tibble() %>% 
  select(1:40) %>% 
  bind_cols(
    data_monthly %>% 
      select(all_of(columns_nonpredictor) , month)
  )

# train and evaluate model
{
  # full training set -------------------------------------------
  training.data <- data_monthly_PC %>% drop_na()
  # make matrix
  predictor_train <- training.data %>% select(-all_of(columns_nonpredictor)) %>% as.matrix() # <-
  response_train <- training.data %>% select(NO2) %>% as.matrix()
  # define model
  NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
  # train model
  NN %>% 
    fit(predictor_train , response_train , 
        epoch = hyperparm_benchmark["epochs"] , 
        batch_size = hyperparm_benchmark["batch.size"] , 
        validation_split = 0.2 , 
        verbose = TRUE)
  # prediction
  prediction_training <- training.data %>% 
    select(month , Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = predict(NN , predictor_train)[,1])
  # clean environment
  rm(training.data , predictor_train , response_train , NN)
  # cross validation -------------------------------------------
  for(k in as.factor(1:k_fold)){
    # data preparation: partition
    training.data <- data_monthly_PC %>% filter(CV != k) %>% drop_na()
    testing.data <- data_monthly_PC %>% filter(CV == k) %>% drop_na()
    # data preparation: make matrix
    predictor_train <- training.data %>% select(-all_of(columns_nonpredictor)) %>% as.matrix() # <-
    response_train <- training.data %>% select(NO2) %>% as.matrix()
    predictor_test <- testing.data %>% select(-all_of(columns_nonpredictor)) %>% as.matrix() # <-
    response_test <- testing.data %>% select(NO2) %>% as.matrix()
    # define model
    NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
    # train model
    NN %>% 
      fit(predictor_train , response_train , 
          epoch = hyperparm_benchmark["epochs"] , 
          batch_size = hyperparm_benchmark["batch.size"] , 
          validation_split = 0.2 , 
          verbose = FALSE)
    # prediction
    prediction_test <- testing.data %>% 
      select(month , Station_name , NO2 , Type_of_station , CV) %>% 
      # prediction
      mutate(predicted = predict(NN , predictor_test)[,1])
    # prediction data.frame
    if(as.character(k) == "1"){
      prediction_CV <- prediction_test
    }else{ # append
      prediction_CV <- bind_rows(prediction_CV , prediction_test)
    }
    # clean environment
    rm(k , training.data , testing.data , 
       predictor_train , predictor_test , response_train , response_test , 
       prediction_test)
  }
  # evaluate  -------------------------------------------
  evaluation_PCA <- prediction_training %>%   # <-
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
}

evaluation_PCA

# =====================================
# filter/embedded: random forest
# =====================================
included_var_RF <- readRDS(sprintf("3_results/output-model/model_monthly/RF_%s.rds" , SAT_product)) %>%
  ranger::importance() %>%
  names()

# train and evaluate model
{
  included_var <- included_var_RF
  # full training set -------------------------------------------
  training.data <- data_monthly %>% drop_na()
  # make matrix
  predictor_train <- training.data %>% select(one_of(included_var)) %>% as.matrix() # <-
  response_train <- training.data %>% select(NO2) %>% as.matrix()
  # define model
  NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
  # train model
  NN %>% 
    fit(predictor_train , response_train , 
        epoch = hyperparm_benchmark["epochs"] , 
        batch_size = hyperparm_benchmark["batch.size"] , 
        validation_split = 0.2 , 
        verbose = TRUE)
  # prediction
  prediction_training <- training.data %>% 
    select(month , Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = predict(NN , predictor_train)[,1])
  # clean environment
  rm(training.data , predictor_train , response_train , NN)
  # cross validation -------------------------------------------
  for(k in as.factor(1:k_fold)){
    # data preparation: partition
    training.data <- data_monthly %>% filter(CV != k) %>% drop_na()
    testing.data <- data_monthly %>% filter(CV == k) %>% drop_na()
    # data preparation: make matrix
    predictor_train <- training.data %>% select(all_of(included_var)) %>% as.matrix() # <-
    response_train <- training.data %>% select(NO2) %>% as.matrix()
    predictor_test <- testing.data %>% select(all_of(included_var)) %>% as.matrix() # <-
    response_test <- testing.data %>% select(NO2) %>% as.matrix()
    # define model
    NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
    # train model
    NN %>% 
      fit(predictor_train , response_train , 
          epoch = hyperparm_benchmark["epochs"] , 
          batch_size = hyperparm_benchmark["batch.size"] , 
          validation_split = 0.2 , 
          verbose = FALSE)
    # prediction
    prediction_test <- testing.data %>% 
      select(month , Station_name , NO2 , Type_of_station , CV) %>% 
      # prediction
      mutate(predicted = predict(NN , predictor_test)[,1])
    # prediction data.frame
    if(as.character(k) == "1"){
      prediction_CV <- prediction_test
    }else{ # append
      prediction_CV <- bind_rows(prediction_CV , prediction_test)
    }
    # clean environment
    rm(k , training.data , testing.data , 
       predictor_train , predictor_test , response_train , response_test , 
       prediction_test)
  }
  # evaluate  -------------------------------------------
  evaluation_RF <- prediction_training %>%   # <-
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
}

evaluation_RF

# =====================================
# embedded: regularization
# =====================================
included_var_all <- colnames(data_monthly)[!colnames(data_monthly) %in% columns_nonpredictor]
# train and evaluate model
{
  included_var <- included_var_all
  hyperparm_reglularization <- hyperparm_benchmark
  hyperparm_reglularization["regularization"] <- 1
  hyperparm_reglularization["regularization_factor"] <- 0.001
  # full training set -------------------------------------------
  training.data <- data_monthly %>% drop_na()
  # make matrix
  predictor_train <- training.data %>% select(one_of(included_var)) %>% as.matrix() # <-
  response_train <- training.data %>% select(NO2) %>% as.matrix()
  # define model
  NN_define(hyperparm_reglularization , n_var = ncol(predictor_train))
  # train model
  NN %>% 
    fit(predictor_train , response_train , 
        epoch = hyperparm_reglularization["epochs"] , 
        batch_size = hyperparm_reglularization["batch.size"] , 
        validation_split = 0.2 , 
        verbose = TRUE)
  # prediction
  prediction_training <- training.data %>% 
    select(month , Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = predict(NN , predictor_train)[,1])
  # clean environment
  rm(training.data , predictor_train , response_train , NN)
  # cross validation -------------------------------------------
  for(k in as.factor(1:k_fold)){
    # data preparation: partition
    training.data <- data_monthly %>% filter(CV != k) %>% drop_na()
    testing.data <- data_monthly %>% filter(CV == k) %>% drop_na()
    # data preparation: make matrix
    predictor_train <- training.data %>% select(all_of(included_var)) %>% as.matrix() # <-
    response_train <- training.data %>% select(NO2) %>% as.matrix()
    predictor_test <- testing.data %>% select(all_of(included_var)) %>% as.matrix() # <-
    response_test <- testing.data %>% select(NO2) %>% as.matrix()
    # define model
    NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
    # train model
    NN %>% 
      fit(predictor_train , response_train , 
          epoch = hyperparm_reglularization["epochs"] , 
          batch_size = hyperparm_reglularization["batch.size"] , 
          validation_split = 0.2 , 
          verbose = FALSE)
    # prediction
    prediction_test <- testing.data %>% 
      select(month , Station_name , NO2 , Type_of_station , CV) %>% 
      # prediction
      mutate(predicted = predict(NN , predictor_test)[,1])
    # prediction data.frame
    if(as.character(k) == "1"){
      prediction_CV <- prediction_test
    }else{ # append
      prediction_CV <- bind_rows(prediction_CV , prediction_test)
    }
    # clean environment
    rm(k , training.data , testing.data , 
       predictor_train , predictor_test , response_train , response_test , 
       prediction_test)
  }
  # evaluate  -------------------------------------------
  evaluation_regularization <- prediction_training %>%   # <-
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
}

evaluation_regularization

# =====================================
# embedded: LIME (Local Interpretable Model-agnostic Explanations)
# =====================================
# library(lime)
# 
# # run lime() on training set -------------------------------------------
# {
#   included_var <- included_var_all
#   training.data <- data_monthly %>% drop_na()
#   # make matrix
#   predictor_train <- training.data %>% select(one_of(included_var)) %>% as.matrix() # <-
#   response_train <- training.data %>% select(NO2) %>% as.matrix()
#   # define model
#   NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
#   # train model
#   NN %>% 
#     fit(predictor_train , response_train , 
#         epoch = hyperparm_benchmark["epochs"] , 
#         batch_size = hyperparm_benchmark["batch.size"] , 
#         validation_split = 0.2 , 
#         verbose = TRUE)
# }
# explainer <- lime(x = training.data , model = NN , bin_continuous = 5)
# 
# # run explainer() on explainer
# explanation <- explain(x = training.data , explainer = explainer , n_features = 10)
# plot_features(explanation)

# =====================================
# embedded: deconstructing weights
# =====================================
library(nnet)
library(NeuralNetTools)

# full training set -------------------------------------------
training.data <- data_monthly %>% drop_na()
# make matrix
set.seed(20210727)
predictor_train <- training.data %>% 
  select(-all_of(columns_nonpredictor)) %>% 
  # random-value variables
  mutate(R1 = runif(n()) , 
         R2 = runif(n()) ,
         R3 = runif(n()) ) %>% 
  as.matrix()
response_train <- training.data %>% 
  select(NO2) %>% 
  as.matrix()
# model
NN_nnet <- nnet(
  x = predictor_train , 
  y = response_train , 
  size = 20 , linout = TRUE , MaxNWts = 1e4
)

# cor(NN_nnet$fitted.values , response_train)^2
# mae(NN_nnet$fitted.values , response_train)

# the functoin for determing relative importance
NN_nnet_varimp <- NeuralNetTools::garson(NN_nnet , bar_plot = FALSE) %>% 
  tibble(variables = row.names(.))
NN_nnet_varimp %>% 
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
       subtitle = "Relative importance of input variables in neural networks\n using Garson's algorithm") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6) , legend.position = "bottom")

included_var_garson <- NN_nnet_varimp %>% 
  filter(rel_imp > NN_nnet_varimp %>% 
           filter(str_detect(variables , "R[123]")) %>% 
           # the max importance of the random-value variables 
           summarize(rel_imp = max(rel_imp)) %>% 
           unlist) %>% 
  select(variables) %>% 
  filter(!str_detect(variables , "R[123]")) %>% 
  unlist %>% unname

# train and evaluate model
{
  included_var <- included_var_garson
  # full training set -------------------------------------------
  training.data <- data_monthly %>% drop_na()
  # make matrix
  predictor_train <- training.data %>% select(one_of(included_var)) %>% as.matrix() # <-
  response_train <- training.data %>% select(NO2) %>% as.matrix()
  # define model
  NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
  # train model
  NN %>% 
    fit(predictor_train , response_train , 
        epoch = hyperparm_benchmark["epochs"] , 
        batch_size = hyperparm_benchmark["batch.size"] , 
        validation_split = 0.2 , 
        verbose = TRUE)
  # prediction
  prediction_training <- training.data %>% 
    select(month , Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = predict(NN , predictor_train)[,1])
  # clean environment
  rm(training.data , predictor_train , response_train , NN)
  # cross validation -------------------------------------------
  for(k in as.factor(1:k_fold)){
    # data preparation: partition
    training.data <- data_monthly %>% filter(CV != k) %>% drop_na()
    testing.data <- data_monthly %>% filter(CV == k) %>% drop_na()
    # data preparation: make matrix
    predictor_train <- training.data %>% select(all_of(included_var)) %>% as.matrix() # <-
    response_train <- training.data %>% select(NO2) %>% as.matrix()
    predictor_test <- testing.data %>% select(all_of(included_var)) %>% as.matrix() # <-
    response_test <- testing.data %>% select(NO2) %>% as.matrix()
    # define model
    NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
    # train model
    NN %>% 
      fit(predictor_train , response_train , 
          epoch = hyperparm_benchmark["epochs"] , 
          batch_size = hyperparm_benchmark["batch.size"] , 
          validation_split = 0.2 , 
          verbose = FALSE)
    # prediction
    prediction_test <- testing.data %>% 
      select(month , Station_name , NO2 , Type_of_station , CV) %>% 
      # prediction
      mutate(predicted = predict(NN , predictor_test)[,1])
    # prediction data.frame
    if(as.character(k) == "1"){
      prediction_CV <- prediction_test
    }else{ # append
      prediction_CV <- bind_rows(prediction_CV , prediction_test)
    }
    # clean environment
    rm(k , training.data , testing.data , 
       predictor_train , predictor_test , response_train , response_test , 
       prediction_test)
  }
  # evaluate  -------------------------------------------
  evaluation_garson <- prediction_training %>%   # <-
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
}

evaluation_garson

# =====================================
# embedded: LassoNET
# =====================================
library(reticulate)
# reticulate::py_install("lassonet" , pip = TRUE)

lassonet <- import("lassonet")

# full training set -------------------------------------------
training.data <- data_monthly %>% drop_na()
# make matrix
set.seed(20210727)
predictor_train <- training.data %>% 
  select(-all_of(columns_nonpredictor)) %>% 
  # # random-value variables
  # mutate(R1 = runif(n()) , 
  #        R2 = runif(n()) ,
  #        R3 = runif(n()) ) %>% 
  as.matrix()
response_train <- training.data %>% 
  select(NO2) %>% 
  as.matrix()
# LassoNET model
NN_Lasso <- lassonet$LassoNetRegressor(hidden_dims = tuple(as.integer(20)) , 
                                       verbose = TRUE , 
                                       patience = c(100,5))
NN_Lasso_path <- NN_Lasso$path(X = predictor_train , y = response_train)

NN_Lasso_df <- tibble(n_selected = NA_real_ , mae = NA_real_)
for(i in 1:length(NN_Lasso_path)){
  NN_Lasso$load(NN_Lasso_path[[i]]$state_dict)
  y_pred <- NN_Lasso$predict(predictor_train)
  if(i == 1){
    NN_Lasso_df <- tibble(
      i = i , 
      n_selected = NN_Lasso_path[[i]]$selected$numpy() %>% sum() , 
      mae = mae(response_train , y_pred) , 
      lambda = NN_Lasso_path[[i]]$lambda_
    )
  }else{ # append
    NN_Lasso_df <- tibble(
      i = i , 
      n_selected = NN_Lasso_path[[i]]$selected$numpy() %>% sum() , 
      mae = mae(response_train , y_pred) , 
      lambda = NN_Lasso_path[[i]]$lambda_
    ) %>% 
      bind_rows(NN_Lasso_df)
  }
}

NN_Lasso_df %>% 
  ggplot(aes(x = lambda , y = mae)) +
  geom_point()
i_best <- NN_Lasso_df %>% 
  filter(n_selected < 100) %>% 
  arrange(mae) %>% 
  slice(1) %>% 
  select(i) %>% 
  unlist %>% unname

included_var_LassoNet <- predictor_train %>% 
  colnames() %>% 
  .[NN_Lasso_path[[i_best]]$selected$numpy()]

# train and evaluate model
{
  included_var <- included_var_LassoNet
  # full training set -------------------------------------------
  training.data <- data_monthly %>% drop_na()
  # make matrix
  predictor_train <- training.data %>% select(one_of(included_var)) %>% as.matrix() # <-
  response_train <- training.data %>% select(NO2) %>% as.matrix()
  # define model
  NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
  # train model
  NN %>% 
    fit(predictor_train , response_train , 
        epoch = hyperparm_benchmark["epochs"] , 
        batch_size = hyperparm_benchmark["batch.size"] , 
        validation_split = 0.2 , 
        verbose = TRUE)
  # prediction
  prediction_training <- training.data %>% 
    select(month , Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = predict(NN , predictor_train)[,1])
  # clean environment
  rm(training.data , predictor_train , response_train , NN)
  # cross validation -------------------------------------------
  for(k in as.factor(1:k_fold)){
    # data preparation: partition
    training.data <- data_monthly %>% filter(CV != k) %>% drop_na()
    testing.data <- data_monthly %>% filter(CV == k) %>% drop_na()
    # data preparation: make matrix
    predictor_train <- training.data %>% select(all_of(included_var)) %>% as.matrix() # <-
    response_train <- training.data %>% select(NO2) %>% as.matrix()
    predictor_test <- testing.data %>% select(all_of(included_var)) %>% as.matrix() # <-
    response_test <- testing.data %>% select(NO2) %>% as.matrix()
    # define model
    NN_define(hyperparm_benchmark , n_var = ncol(predictor_train))
    # train model
    NN %>% 
      fit(predictor_train , response_train , 
          epoch = hyperparm_benchmark["epochs"] , 
          batch_size = hyperparm_benchmark["batch.size"] , 
          validation_split = 0.2 , 
          verbose = FALSE)
    # prediction
    prediction_test <- testing.data %>% 
      select(month , Station_name , NO2 , Type_of_station , CV) %>% 
      # prediction
      mutate(predicted = predict(NN , predictor_test)[,1])
    # prediction data.frame
    if(as.character(k) == "1"){
      prediction_CV <- prediction_test
    }else{ # append
      prediction_CV <- bind_rows(prediction_CV , prediction_test)
    }
    # clean environment
    rm(k , training.data , testing.data , 
       predictor_train , predictor_test , response_train , response_test , 
       prediction_test)
  }
  # evaluate  -------------------------------------------
  evaluation_LassoNet <- prediction_training %>%   # <-
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
}

evaluation_LassoNet

# //////////////////////////////////////////////////////////////////////////
# evaluation of the different feature selection methods 
# //////////////////////////////////////////////////////////////////////////
mget(ls(pattern = "^evaluation")) %>% 
  bind_rows() %>% 
  mutate(method = ls(pattern = "^evaluation" , envir=.GlobalEnv) %>% 
           str_remove("evaluation_")) %>% 
  arrange(MAE_CV)


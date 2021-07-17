#####################################################################################################
# Masterarbeit
# Modeling
# Monthly model: Random forest model
# 2021-07-07
#####################################################################################################

# =====================================
# required packages
# =====================================
library(readr) ; library(vroom)
library(sf) 
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(ggsci) ; library(ggthemes)
library(lubridate) ; library(stringr)
library(spdep)
library(Metrics)
library(ranger)

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
columns_nonpredictor <- c("Station_name" , "Type_of_zone" , "Type_of_station" , 
                          "Altitude" , "Canton_ID" , "Canton_name" , "X" , "Y" , 
                          "CV" , "spatial_CV")

# //////////////////////////////////////////////////////////////////////////
# model fitting
# //////////////////////////////////////////////////////////////////////////
model_name <- "Random forest"
model_abbr <- "RF"
SAT_product <- c("OMI" , "TROPOMI")[1]

# random forest hyperparameters
hyper_grid <- read_csv("3_results/output-data/model_monthly/RF_grid-search/hyper_evaluation.csv")
hyperparm_final <- hyper_grid %>% 
  arrange(-CV_R2) %>% 
  slice(3) %>% 
  as.list() 
# mtry = 3 , N.trees = 1000

# =====================================
# subset data: satellite-product
# =====================================
if(SAT_product == "OMI"){
  # for the OMI model: exclude TROPOMI and meteorological variables at 12H
  data_monthly <- data_monthly_raw %>% 
    select(-TROPOMI_NO2 , -ends_with("_12H")) 
}else if(SAT_product == "TROPOMI"){
  # for the TROPOMI model: exclude OMI and meteorological variables at 15H
  data_monthly <- data_monthly_raw %>% 
    select(-OMI_NO2 , -ends_with("_15H")) 
}

# =====================================
# screening of important variables
# =====================================
set.seed(1010)
rf_screen <- ranger(NO2 ~ . , 
                    data = data_monthly %>% 
                      # drop non-predictor columns
                      select(-all_of(columns_nonpredictor)) %>%
                      drop_na() %>% 
                      # random-value variables
                      mutate(R1 = runif(n()) , 
                             R2 = runif(n()) ,
                             R3 = runif(n()) ) , 
                    importance = "impurity" , 
                    keep.inbag = TRUE , 
                    num.trees = 5000 , 
                    mtry = 30 )
rf_screen_importance <- rf_screen$variable.importance %>% 
  sort(decreasing = TRUE) %>% 
  as.list() %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything() , names_to = "variable" , values_to = "importance")

# exclude not-important variables 
included_var <- rf_screen_importance %>% 
  filter(importance > rf_screen_importance %>% 
           filter(str_detect(variable , "R[123]")) %>% 
           # the max importance of the random-value variables 
           summarize(importance = max(importance)) %>% 
           unlist)

# =====================================
# random forest model with the selected variables 
# =====================================
formula_rf_final <- included_var %>% 
  select(variable) %>% 
  unlist %>% unname() %>% 
  paste(collapse = " + ") %>% 
  sprintf("NO2 ~ %s" , .)

# fit model
set.seed(1021)
rf_final <- ranger(formula_rf_final , 
                   data = data_monthly %>% 
                     drop_na() , 
                   importance = "impurity" , 
                   keep.inbag = TRUE , 
                   num.trees = hyperparm_final$N.trees , 
                   mtry = hyperparm_final$mtry)

# =====================================
# cross validation
# =====================================
# calculated and save the CV-prediction
# conventional CV
for(k in as.factor(1:k_fold)){
  # partition
  training.data <- data_monthly %>% 
    filter(CV != k)
  testing.data <- data_monthly %>% 
    filter(CV == k)
  # train model
  set.seed(123)
  model_train <- ranger(
    formula = formula_rf_final ,  # <-
    data = training.data %>% 
      drop_na() , 
    importance = "impurity" , 
    keep.inbag = TRUE , 
    num.trees = hyperparm_final$N.trees , 
    mtry = hyperparm_final$mtry
  )
  # prediction
  prediction_test <- testing.data %>% 
    select(month , Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = predict(model_train , data = testing.data)$predictions)
  # prediction data.frame
  if(as.character(k) == "1"){
    rf_final_prediction_CV <- prediction_test # <-
  }else{ # append
    rf_final_prediction_CV <- bind_rows(rf_final_prediction_CV , prediction_test) # <-
  }
  # clean environment
  rm(training.data , testing.data , model_train , prediction_test , k)
}

# spatial CV
for(k in as.factor(1:k_fold)){
  # partition
  training.data <- data_monthly %>% 
    filter(spatial_CV != k)
  testing.data <- data_monthly %>% 
    filter(spatial_CV == k)
  # train model
  set.seed(123)
  model_train <- ranger(
    formula = formula_rf_final ,  # <-
    data = training.data %>% 
      drop_na() , 
    importance = "impurity" , 
    keep.inbag = TRUE , 
    num.trees = hyperparm_final$N.trees , 
    mtry = hyperparm_final$mtry
  )
  # prediction
  prediction_test <- testing.data %>% 
    select(month , Station_name , NO2 , Type_of_station , spatial_CV) %>% 
    # prediction
    mutate(predicted = predict(model_train , data = testing.data)$predictions)
  # prediction data.frame
  if(as.character(k) == "1"){
    rf_final_prediction_CV_sp <- prediction_test # <-
  }else{ # append
    rf_final_prediction_CV_sp <- bind_rows(rf_final_prediction_CV_sp , prediction_test) # <-
  }
  # clean environment
  rm(training.data , testing.data , model_train , prediction_test , k)
}

# temporal cross validation
for(k in unique(data_monthly$month)){
  # partition
  training.data <- data_monthly %>% 
    filter(month != k)
  testing.data <- data_monthly %>%
    filter(month == k)
  # train model
  set.seed(123)
  model_train <- ranger(
    formula = formula_rf_final ,  # <-
    data = training.data %>% 
      drop_na() , 
    importance = "impurity" , 
    keep.inbag = TRUE , 
    num.trees = hyperparm_final$N.trees , 
    mtry = hyperparm_final$mtry
  )
  # prediction
  prediction_test <- testing.data %>% 
    select(month , Station_name , NO2 , Type_of_station) %>% 
    # prediction
    mutate(predicted_temporalCV = predict(model_train , data = testing.data)$predictions) # <-
  # prediction data.frame
  if(as.character(k) == "1"){
    rf_final_prediction_CV_tp <- prediction_test # <-
  }else{ # append
    rf_final_prediction_CV_tp <- bind_rows(rf_final_prediction_CV_tp , prediction_test) # <-
  }
  # clean environment
  rm(training.data , testing.data , model_train , prediction_test , k)
}

# combine the prediction of the three CV
rf_final_prediction_CV <- rf_final_prediction_CV %>% 
  left_join(rf_final_prediction_CV_sp , 
            by = c("month" , "Station_name" , "NO2" , "Type_of_station") , 
            suffix = c("_CV" , "_spatialCV")) %>% 
  left_join(rf_final_prediction_CV_tp , 
            by = c("month" , "Station_name" , "NO2" , "Type_of_station"))


# //////////////////////////////////////////////////////////////////////////
# model evaluation
# //////////////////////////////////////////////////////////////////////////
# =====================================
# model summary
# =====================================
rf_final

# =====================================
# observed, predicted, residuals
# =====================================
# OOB-prediction
rf_final_prediction <- data_monthly %>% 
  drop_na() %>% 
  select(month , Station_name , NO2 , Type_of_station , X , Y) %>% 
  # OOB-prediction
  mutate(predicted = rf_final$predictions) %>% 
  # CV-prediction
  full_join(rf_final_prediction_CV , 
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
rf_final_indices <- rf_final_prediction %>%   
  eval_performance_indices()

# =====================================
# visualization
# =====================================
out_dirpath_plots <- sprintf("3_results/output-graph/model_monthly/%s" , model_abbr)
if(!dir.exists(out_dirpath_plots)) dir.create(out_dirpath_plots , recursive = TRUE)

# predicted <-> observed
plot_obs_pred(rf_final_prediction , # <-
              sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/obs-pred_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 5 , base_height = 3.5
)

# residual diagnostic plots
plot_resid(rf_final_prediction , # <-
           title_text = sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/residuals_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 7 , base_height = 7
)

# residuals by month
plot_resid_month(rf_final_prediction , 
                 subtitle_text = sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/residuals-month_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 6 , base_height = 3
)

# variable importance screening
rf_screen_importance %>% 
  mutate(class = ifelse(str_detect(variable , "R[123]") , "Random" , "Predictor variables")) %>% 
  # reorder for visualization
  mutate(variable = factor(variable , levels = variable[order(importance)])) %>% 
  # visualization
  ggplot(aes(x = variable , y = importance , fill = class)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_lancet() +
  labs(x = "Variables" , y = "Variable importance" , fill = "" , 
       title = "Screening of relevant predictor variables" , 
       subtitle = sprintf("The variable importance of the random forest model (%s)" , SAT_product)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 4) , legend.position = "bottom")
save_plot(
  sprintf("%s/importance-screening_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 6 , base_height = 9
)
included_var %>% 
  # reorder for visualization
  mutate(variable = factor(variable , levels = variable[order(importance)])) %>% 
  ggplot(aes(x = variable , y = importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variables" , y = "Variable importance" , 
       title = sprintf("Included predictor variables (%s)" , SAT_product)) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 8) , legend.position = "bottom")
save_plot(
  sprintf("%s/importance-included_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 5 , base_height = 6
)


# =====================================
# spatial autocorrelation of the residuals
# =====================================
moran_month_df <- eval_resid_moran(rf_final_prediction , by_time = TRUE , col_time = "month")
moran_mean_df <- eval_resid_moran(rf_final_prediction %>% drop_na() , 
                                  by_time = FALSE)
# visualization
plot_resid_map(rf_final_prediction %>% drop_na() , # <-
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
  if(!dir.exists(out_dirpath_predicted)) dir.create(out_dirpath_predicted , recursive = TRUE)
  rf_final_prediction %>% # <-
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_predicted , model_abbr , SAT_product))
  
  # export the model performance indices
  out_dirpath_indices <- "3_results/output-data/model_monthly/indices"
  if(!dir.exists(out_dirpath_indices)) dir.create(out_dirpath_indices , recursive = TRUE)
  rf_final_indices %>% # <- 
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_indices , model_abbr , SAT_product))
  
  # Moran's I
  out_dirpath_Moran <- "3_results/output-data/model_monthly/Moran"
  if(!dir.exists(out_dirpath_Moran)) dir.create(out_dirpath_Moran , recursive = TRUE)
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
  if(!dir.exists(out_dirpath_model)) dir.create(out_dirpath_model , recursive = TRUE)
  saveRDS(rf_final , # <-
          file = sprintf("%s/%s_%s.rds" , out_dirpath_model , model_abbr , SAT_product))
}


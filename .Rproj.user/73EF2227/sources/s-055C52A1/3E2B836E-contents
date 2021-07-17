#####################################################################################################
# Masterarbeit
# Modeling
# Annual model: Random forest-- grid search of hyperparameters
# 2021-06-22
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
  inner_join(sites_CV , by = "Station_name") %>% # this also excludes the 3 sites that are outside Switzerland and was not counted in the CV groups
  select(-geometry)

# non-predictor columns
columns_nonpredictor <- c("Station_name" , "Type_of_zone" , "Type_of_station" , 
                          "Altitude" , "Canton_ID" , "Canton_name" , "X" , "Y" , 
                          "CV" , "spatial_CV")

# //////////////////////////////////////////////////////////////////////////
# naming the model
# //////////////////////////////////////////////////////////////////////////
SAT_product <- "TROPOMI"
# for now I grid-search the model using TROPOMI and use the same hyperparameter set for OMI

# //////////////////////////////////////////////////////////////////////////
# data preparation for random forest
# //////////////////////////////////////////////////////////////////////////
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

# =====================================
# screening of important variables
# =====================================
set.seed(1010)
rf_screen <- ranger(NO2 ~ . , 
                    data = data_annual %>% 
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
# visualization
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

# exclude not-important variables 
included_var <- rf_screen_importance %>% 
  filter(importance > rf_screen_importance %>% 
           filter(str_detect(variable , "R[123]")) %>% 
           # the max importance of the random-value variables 
           summarize(importance = max(importance)) %>% 
           unlist)
formula_rf_final <- included_var %>% 
  select(variable) %>% 
  unlist %>% unname() %>% 
  paste(collapse = " + ") %>% 
  sprintf("NO2 ~ %s" , .)

# //////////////////////////////////////////////////////////////////////////
# grid search for best model hyperparameters
# //////////////////////////////////////////////////////////////////////////
# =====================================
# create hyperparameter grid
# =====================================
# create hyperparameter grid
hyper_grid <- expand.grid(
  # Number of variables to possibly split at in each node. 
  # Default is the (rounded down) square root of the number variables
  mtry = c(3,5,8,10,20,30,40) , 
  # Number of trees.
  N.trees = c(50,100,150,250,300,500,1000,1500,2000,3000,5000) ,
  # a place to dump results
  OOB_MSE = NA,  
  OOB_R2 = NA ,
  CV_R2 = NA  
)

# =====================================
# grid search 
# =====================================
pb <- txtProgressBar(min = 1 , max = nrow(hyper_grid) , style = 3 )
for(i in 1:nrow(hyper_grid)) {
  # reproducibility
  set.seed(123)
  # train model
  rf_grid <- ranger(formula_rf_final , 
                    data = data_annual %>% 
                      drop_na(), 
                    importance = "impurity" , 
                    keep.inbag = TRUE , 
                    num.trees = hyper_grid$N.trees[i] , 
                    mtry = hyper_grid$mtry[i] )
  # conventional CV
  for(k in as.factor(1:k_fold)){
    # partition
    training.data <- data_annual %>% 
      filter(CV != k)
    testing.data <- data_annual %>% 
      filter(CV == k)
    # train model
    set.seed(123)
    rf_grid_cv <- ranger(
      formula = formula_rf_final  ,  # <-
      data = training.data %>% 
        drop_na() , 
      importance = "impurity" , 
      keep.inbag = TRUE , 
      num.trees = hyper_grid$N.trees[i] , 
      mtry = hyper_grid$mtry[i]
    )
    # prediction
    prediction_test <- testing.data %>% 
      select(Station_name , NO2 , Type_of_station , CV) %>% 
      # prediction
      mutate(predicted = predict(rf_grid_cv , data = testing.data)$predictions)
    # prediction data.frame
    if(as.character(k) == "1"){
      rf_grid_prediction_CV <- prediction_test # <-
    }else{ # append
      rf_grid_prediction_CV <- bind_rows(rf_grid_prediction_CV , prediction_test) # <-
    }
    # clean environment
    rm(training.data , testing.data , rf_grid_cv , prediction_test , k)
  }
  # evaluation (add to hyper_grid data.frame)
  # OOB-RMSE
  hyper_grid$OOB_MSE[i] <- rf_grid$prediction.error
  # OOB-R2
  hyper_grid$OOB_R2[i] <- rf_grid$r.squared
  # CV-R2
  hyper_grid$CV_R2[i] <- cor(rf_grid_prediction_CV$NO2 , rf_grid_prediction_CV$predicted , 
                             use = "na.or.complete")^2
  # progress bar
  setTxtProgressBar(pb,i)
  # clean environment
  rm(rf_grid_prediction_CV , rf_grid)
}
rm(pb,i)


# =====================================
# export grid search results
# =====================================
out_dirpath_hypergrid <- "3_results/output-data/model_annual/RF_grid-search"
if(!dir.exists(out_dirpath_hypergrid)) dir.create(out_dirpath_hypergrid , recursive = TRUE)
hyper_grid %>%
  write_csv(sprintf("%s/hyper_evaluation.csv" , out_dirpath_hypergrid))

# =====================================
# visualization: best hyperparameter
# =====================================
hyper_grid %>% 
  pivot_longer(cols = ends_with("R2") , 
               names_to = c("type" , "indices") , names_sep = "_" , 
               values_to = "R2") %>% 
  mutate(mtry = factor(mtry) ,
         type = factor(type , levels = c("OOB" , "CV"))) %>% 
  # visualization
  ggplot(aes(x = N.trees , y = R2 , color = mtry)) +
  geom_line() +
  geom_point() +
  facet_grid(type~. , scales = "free_y") +
  scale_color_manual(values = RColorBrewer::brewer.pal(length(unique(hyper_grid$mtry)),"Spectral")) +
  labs(x = "Number of trees" , y = expression(R^2) , 
       title = "Grid search of random forest hyperparameters") +
  theme_bw()
ggsave(sprintf("%s/hyper_evaluation.png" , out_dirpath_hypergrid) , width = 5 , height = 5)

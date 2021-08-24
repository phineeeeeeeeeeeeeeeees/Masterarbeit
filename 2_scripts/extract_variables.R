library(dplyr) 
library(lme4)
library(xgboost) 

model <- readRDS("3_results/output-model/model_daily/XGB_OMI.rds")

# lm
model$coefficients %>% 
  names %>% 
  paste(collapse = ", ")

# lmer
model %>% 
  fixef() %>% 
  names() %>% 
  paste(collapse = ", ")

# ranger
model$variable.importance %>% 
  names %>% 
  paste(collapse = ", ")

# xgboost
model$feature_names %>% 
  paste(collapse = ", ")

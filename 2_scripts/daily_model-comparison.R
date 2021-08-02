#####################################################################################################
# Masterarbeit
# Modeling
# Comparison of the daily models
# 2021-08-02
#####################################################################################################

# =====================================
# required packages
# =====================================
library(readr) 
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(ggsci) ; library(ggthemes)
library(lubridate) ; library(stringr)

# =====================================
# load data
# =====================================
# model performance indices 
in_filepath_indices <- list.files("3_results/output-data/model_daily/indices" , pattern = ".csv$" , full.names = TRUE)
model_indices <- in_filepath_indices %>% 
  lapply(read_csv) %>% 
  bind_rows()
# model observed versus predicted values
model_prediction <- list.files("3_results/output-data/model_daily/observed-predicted" , pattern = ".csv$" , full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows()
# model residual Moran's I
model_moran <- list.files("3_results/output-data/model_daily/moran" , pattern = "mean_[[:alpha:]]+_[[:alpha:]]+.csv$" , full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows()

# distance from the monitoring sites to the nearest road
sites_road <- read_csv("1_data/processed/cleaned/extracted/site-road-distance.csv")

# =====================================
# model performance indices
# tidy table for presentation and paper
# =====================================
model_compare_tidy <- model_indices %>% 
  # round values
  mutate(across(c(value , min , max) , round , 3)) %>% 
  # value format: with ranges
  mutate(across(c(value , min , max) , format , trim = TRUE , digits = 3)) %>% 
  mutate(value = glue::glue_data(. , "{value} ({min}~{max})") %>% 
           str_remove_all("\\(NA~NA\\)") %>% str_trim()) %>% 
  # re-order columns
  select(model , product , type , name , value) %>% 
  pivot_wider(names_from = c(type , name) , names_sep = "_" , values_from = value) %>% 
  # re-order rows
  mutate(model = factor(model , levels = c("SLR" , "SLMER" , "RF" , "GBM" , "NN")) , 
         product = factor(product , levels = c("OMI" , "TROPOMI"))) %>% 
  arrange(model , product) %>% 
  # re-order columns
  select(model , product , 
         training_R2 , training_RMSE , training_slope , training_intercept , 
         CV_R2 , CV_RMSE , CV_slope , CV_intercept , 
         spatialCV_R2 , spatialCV_RMSE , spatialCV_slope , spatialCV_intercept , 
         temporalCV_R2 , temporalCV_RMSE , temporalCV_slope , temporalCV_intercept) %>% 
  # Moran's I
  left_join(
    model_moran %>% 
      pivot_wider(names_from = name , values_from = value) %>% 
      rename(MoransI = Moran_I_statistic , 
             MoransI.p = p) %>% 
      select(-Expectation , -Variance) , 
    by = c("model" , "product")
  ) %>% 
  # round values (Morans's I)
  mutate(across(starts_with("MoransI") , round , 3))

# export the tidy table
model_compare_tidy %>% 
  write_csv("3_results/output-data/model_daily/model_daily_indices.csv")
# tidy table with R2, RMSE, Moran's I
model_compare_tidy %>% 
  select(model , product , 
         training_R2 , training_RMSE , 
         CV_R2 , CV_RMSE , 
         spatialCV_R2 , spatialCV_RMSE , 
         temporalCV_R2 , temporalCV_RMSE, starts_with("MoransI")) %>% 
  write_csv("3_results/output-data/model_daily/model_daily_indices_1.csv")
# tidy table with slope and intercept
model_compare_tidy %>% 
  select(model , product , 
         training_slope , training_intercept , 
         CV_slope , CV_intercept , 
         spatialCV_slope , spatialCV_intercept , 
         temporalCV_slope , temporalCV_intercept) %>% 
  write_csv("3_results/output-data/model_daily/model_daily_indices_2.csv")

# =====================================
# residual diagnostics
# residual <-> distance to nearest roads
# =====================================
model_prediction_road <- model_prediction %>% 
  # distance to nearest roads
  left_join(
    sites_road %>% 
      select(Station_name , starts_with("dist_") , starts_with("DTV")), 
    by = "Station_name"
  ) %>% 
  # binary: <100m to nearest major road
  mutate(near = ifelse(dist_nearest_mainroad_bysite < 100 , "1" , "0") %>% 
           factor(levels = c("1" , "0"))) %>% 
  # re-order for visualization
  mutate(model = factor(model , levels = c("SLR" , "SLMER" , "RF" , "GBM" , "NN")) , 
         product = factor(product , levels = c("OMI" , "TROPOMI")))

model_prediction_road %>% 
  # visualization
  ggplot(aes(x = dist_nearest_mainroad_bysite , y = residual_CV)) +
  geom_point(shape = 1) +
  geom_smooth() +
  scale_x_log10() +
  facet_wrap(~model + product , ncol = 4) +
  labs(title = "The distance from the monitoring sites to the nearest major road" , 
       subtitle = "Daily models" ,
       x = "meter" ,  y = "Cross-validation residual") +
  theme_bw()

model_prediction_road %>% 
  ggplot(aes(x = near , y = residual_CV)) +
  ggdist::stat_halfeye() +
  facet_wrap(~model + product , ncol = 4) +
  scale_x_discrete(labels = c("<100m" , "≥100m") , name = "Distance to the nearest main road") +
  labs(y = "Cross-validation residual") +
  theme_bw()

model_residual_mainroad_test <- model_prediction_road %>% 
  select(model , product , near , residual_CV) %>% 
  group_by(model , product) %>% 
  group_modify(
    ~ t.test(residual_CV ~ near , data = .x , alternative = "greater") %>% 
      broom::tidy()
  ) %>% 
  ungroup() %>% 
  mutate(significance = p.value < 0.05)

model_residual_mainroad_test %>% 
  select(model , product , statistic , p.value , conf.low , conf.high , alternative , significance)


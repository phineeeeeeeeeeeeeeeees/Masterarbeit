#####################################################################################################
# Masterarbeit
# Modeling
# Comparison of the monthly models
# 2021-08-02
#####################################################################################################

# =====================================
# required packages
# =====================================
library(readr) 
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(ggsci) ; library(ggthemes) ; library(cowplot)
library(lubridate) ; library(stringr)

tempres <- "monthly"

# =====================================
# load data
# =====================================
# model performance indices 
model_indices <- list.files(sprintf("3_results/output-data/model_%s/indices" , tempres) , 
                                  pattern = ".csv$" , full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows() %>% 
  mutate(model = factor(model , levels = c("SLR" , "SLMER" , "RF" , "XGB" , "LGB" , "NN")) , 
         product = factor(product , levels = c("OMI" , "TROPOMI")))

# model observed versus predicted values
model_prediction <- list.files(sprintf("3_results/output-data/model_%s/observed-predicted" , tempres) , 
                               pattern = ".csv$" , full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows() %>% 
  mutate(model = factor(model , levels = c("SLR" , "SLMER" , "RF" , "XGB" , "LGB" , "NN")) , 
         product = factor(product , levels = c("OMI" , "TROPOMI")))

# model residual Moran's I
model_moran <- list.files(sprintf("3_results/output-data/model_%s/moran" , tempres) , 
                          pattern = "mean_[[:alpha:]]+_[[:alpha:]]+.csv$" , full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows() %>% 
  mutate(model = factor(model , levels = c("SLR" , "SLMER" , "RF" , "XGB" , "LGB" , "NN")) , 
         product = factor(product , levels = c("OMI" , "TROPOMI")))

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
  write_csv(sprintf("3_results/output-data/model_%s/model_%s_indices.csv" , tempres , tempres))
# tidy table with R2, RMSE, Moran's I
model_compare_tidy %>% 
  select(model , product , 
         training_R2 , training_RMSE , 
         CV_R2 , CV_RMSE , 
         spatialCV_R2 , spatialCV_RMSE , 
         temporalCV_R2 , temporalCV_RMSE, starts_with("MoransI")) %>% 
  write_csv(sprintf("3_results/output-data/model_%s/model_%s_indices_1.csv" , tempres , tempres))
# tidy table with slope and intercept
model_compare_tidy %>% 
  select(model , product , 
         training_slope , training_intercept , 
         CV_slope , CV_intercept , 
         spatialCV_slope , spatialCV_intercept , 
         temporalCV_slope , temporalCV_intercept) %>% 
  write_csv(sprintf("3_results/output-data/model_%s/model_%s_indices_2.csv" , tempres , tempres))


# =====================================
# spatial CV residual diagnostics
# =====================================
model_prediction %>% 
  filter(!if_any(everything() , is.na)) %>% 
  # re-order for visualization
  mutate(spatial_CV = factor(spatial_CV)) %>% 
  # fold-specific RMSE
  group_by(model, product , spatial_CV) %>% 
  summarize(RMSE_spatialCV = Metrics::rmse(NO2 , predicted_spatialCV)) %>% 
  ungroup() %>% 
  # visualization
  ggplot(aes(x = spatial_CV , y = RMSE_spatialCV , fill = spatial_CV)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = format(RMSE_spatialCV , digits = 3)) , 
            angle = 90 , hjust = 1.3 , color = "white" , size = 3) +
  facet_grid(product ~ model) +
  scale_fill_jco() +
  labs(title = "RMSE of the spatially-blocked cross validations" , 
       x = "Spatially-blocked CV fold" , y = "Spatially-blocked CV-RMSE") +
  theme_bw() +
  theme(legend.position = "none")

save_plot(
  sprintf("3_results/output-graph/model_%s/compare_spatial_CV_RMSE.png" , tempres) , 
  plot = last_plot() , 
  base_width = 8 , base_height = 5
)

# =====================================
# temporal CV residual diagnostics
# =====================================
model_prediction %>% 
  filter(!if_any(everything() , is.na)) %>% 
  # re-order for visualization
  mutate(month = factor(month)) %>% 
  # fold-specific RMSE
  group_by(model, product , month) %>% 
  summarize(RMSE_temporalCV = Metrics::rmse(NO2 , predicted_temporalCV)) %>% 
  ungroup() %>% 
  # visualization
  ggplot(aes(x = month , y = RMSE_temporalCV)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = format(RMSE_temporalCV , digits = 3)) , 
            angle = 90 , hjust = 1.2 , color = "white" , size = 2) +
  facet_grid(product ~ model) +
  labs(title = "RMSE of the temporally-blocked cross validations" , 
       x = "Month (Temporally-blocked CV fold)" , y = "Temporally-blocked CV-RMSE") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90 , vjust = 0.5, hjust = 1))

save_plot(
  sprintf("3_results/output-graph/model_%s/compare_temporal_CV_RMSE.png" , tempres) , 
  plot = last_plot() , 
  base_width = 10 , base_height = 6
)

# =====================================
# correlations between different model CV residuals
# =====================================
model_prediction %>% 
  filter(!if_any(everything() , is.na)) %>% 
  select(model , product , residual_CV , month , Station_name) %>% 
  pivot_wider(id_cols = c(month , Station_name , product) , names_from = model , values_from = residual_CV) %>% 
  # correlation matrix plot
  group_by(product) %>% 
  group_map(
    ~ .x %>% 
      GGally::ggpairs(columns = c("SLR" , "SLMER" , "RF" , "XGB" , "LGB" , "NN")) +
      labs(subtitle = .y$product , 
           x = "CV residuals" , y = "CV residuals") +
      theme_bw() +
      theme(axis.text = element_text(size = 7))
  ) %>% 
  # multiple plots
  lapply(GGally::ggmatrix_gtable) %>% 
  plot_grid(plotlist = . , align = "h" , axis = "tb" , nrow = 1) %>% 
  plot_grid(
    ggdraw() + 
      draw_label("Correlation matrix plot of the residuals of the different models" , 
                 x = 0, hjust = 0) +
      theme(plot.margin = margin(0, 0, 0, 15)) , 
    . , 
    ncol = 1,
    rel_heights = c(0.05, 1)
  )

save_plot(
  sprintf("3_results/output-graph/model_%s/compare_residual_correlation.png" , tempres) , 
  plot = last_plot() , 
  base_width = 10 , base_height = 5
)

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
  facet_wrap(~model + product) +
  labs(title = "The distance from the monitoring sites to the nearest major road" , 
       subtitle = "Monthly models" ,
       x = "meter" ,  y = "Cross-validation residual") +
  theme_bw()

model_prediction_road %>% 
  ggplot(aes(x = near , y = residual_CV)) +
  ggdist::stat_halfeye() +
  facet_wrap(~model + product) +
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


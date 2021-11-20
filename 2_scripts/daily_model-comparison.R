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
library(ggplot2) ; library(ggsci) ; library(ggthemes) ; library(cowplot)
library(lubridate) ; library(stringr)
library(astsa)

source("2_scripts/utils_model-eval.R")

tempres <- "daily"

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

# annual data
data_annual_raw <- read_csv("1_data/processed/cleaned/extracted/annual_scaled.csv")

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
# temporal autocorrelation of model residuals 
# =====================================
# daily mean residuals (averaged acrossed monitoring sites)
model_resid_daily <- model_prediction %>% 
  group_by(model , product , date) %>% 
  summarize(mean_residual = mean(residual_CV , na.rm = TRUE)) %>% 
  ungroup()

Sys.setlocale(locale = "en_US.UTF-8")
# residual time series plot
model_resid_daily %>% 
  ggplot(aes(x = date , y = mean_residual)) +
  geom_line(size = 0.3) +
  geom_smooth(method = "loess" , span = 0.5) +
  facet_wrap(~ model+product , scales = "free_y" , nrow = 2 , dir = "v") +
  scale_x_date(breaks = "2 months" , date_labels = "%b") +
  labs(x = "Date" , y = "Daily mean residuals" , title = "Daily mean CV-residual time series (averaged acrossed sites)") +
  theme_bw()
save_plot(
  sprintf("3_results/output-graph/model_%s/residual_time_series.png" , tempres) , 
  plot = last_plot() , 
  base_width = 13 , base_height = 4
)

# spectral analysis (fast Fourier transformation)
model_resid_spectrum <- model_resid_daily %>% 
  group_by(model , product) %>% 
  # spectral analysis
  group_modify(
    ~ .x %>% 
      select(mean_residual) %>% 
      unlist() %>% unname() %>% 
      diff() %>% 
      ts() %>% 
      mvspec(spans = 7 , detrend = FALSE , plot = FALSE) %>% 
      .$details %>% 
      as_tibble()
  ) %>% 
  ungroup()

# spectral density graphs
model_resid_spectrum %>% 
  ggplot(aes(x = frequency , y = spectrum)) +
  geom_line() +
  facet_wrap(~ model+product , scales = "free_y" , nrow = 2 , dir = "v") +
  labs(x = "Frequency (1/day)" , y = "Spectral density" , 
       title = "Spectral density of the daily mean CV-residual time series (averaged acrossed sites)" , 
       subtitle = "(First-differenced series; Fast Fourier Transformation; spectral density smoothing bandwidth = 7)") +
  theme_bw()
save_plot(
  sprintf("3_results/output-graph/model_%s/residual_time_series_spectral_density.png" , tempres) , 
  plot = last_plot() , 
  base_width = 13 , base_height = 4
)

# ACF/PACF plots with S=7
model_resid_daily %>% 
  group_by(model , product) %>% 
  group_modify(
    ~ .x %>% 
      select(mean_residual) %>% 
      unlist() %>% unname() %>% 
      diff(7) %>% 
      ts() %>% 
      acf2(plot = FALSE) %>% 
      as_tibble() %>% 
      mutate(lag = 1:n())
  ) %>% 
  ungroup() %>% 
  pivot_longer(cols = c("ACF" , "PACF")) %>% 
  ggplot(aes(x = lag , y = value)) +
  geom_vline(xintercept = c(7,14,21,28) , linetype = "dotted" , color = "azure4" , size = 0.4) +
  geom_bar(stat = "identity") +
  facet_grid(name ~ model + product) +
  labs(x = "Lag" , y = "" , 
       title = "ACF/PACF plots of the daily mean CV-residual time series (averaged acrossed sites)" , 
       subtitle = "(lag-7-differenced)") +
  theme_bw()
save_plot(
  sprintf("3_results/output-graph/model_%s/residual_time_series_ACF2.png" , tempres) , 
  plot = last_plot() , 
  base_width = 13 , base_height = 4
)
  
# =====================================
# correlations between different model CV residuals
# =====================================
model_prediction %>% 
  filter(!if_any(everything() , is.na)) %>% 
  select(model , product , residual_CV , date , Station_name) %>% 
  pivot_wider(id_cols = c(date , Station_name , product) , names_from = model , values_from = residual_CV) %>% 
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
# aggragate daily estimation to annual mean
# =====================================
# aggregate daily to annual mean
data_annual_aggregated <- model_prediction %>% 
  select(model , product , date , Station_name , CV , spatial_CV , predicted , predicted_CV , predicted_spatialCV , predicted_temporalCV) %>% 
  group_by(model , product , Station_name , CV , spatial_CV) %>% 
  summarise(across(starts_with("predicted") , mean , na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(
    data_annual_raw %>% 
      select(Station_name , NO2) , 
    by = "Station_name"
  )
# model performace indices on annual average
model_indices_annual_aggregated <- data_annual_aggregated %>% 
  # filter(model == "SLR" & product == "OMI") %>% 
  group_by(model , product) %>% 
  group_modify( ~ eval_performance_indices(.x)) %>% 
  ungroup() %>% 
  mutate(min = ifelse(type == "temporalCV" , NA , min) , 
         max = ifelse(type == "temporalCV" , NA , max))

# tidy table
model_compare_tidy_aggregated <- model_indices_annual_aggregated %>% 
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
         temporalCV_R2 , temporalCV_RMSE , temporalCV_slope , temporalCV_intercept)
# export the tidy table
model_compare_tidy_aggregated %>% 
  write_csv(sprintf("3_results/output-data/model_%s/model_%s_aggregated_indices.csv" , tempres , tempres))
# tidy table with R2, RMSE, Moran's I
model_compare_tidy_aggregated %>% 
  select(model , product , 
         training_R2 , training_RMSE , 
         CV_R2 , CV_RMSE , 
         spatialCV_R2 , spatialCV_RMSE) %>% 
  write_csv(sprintf("3_results/output-data/model_%s/model_%s_aggregated_indices_1.csv" , tempres , tempres))
# tidy table with slope and intercept
model_compare_tidy_aggregated %>% 
  select(model , product , 
         training_slope , training_intercept , 
         CV_slope , CV_intercept , 
         spatialCV_slope , spatialCV_intercept) %>% 
  write_csv(sprintf("3_results/output-data/model_%s/model_%s_aggregated_indices_2.csv" , tempres , tempres))

# =====================================
# comparison: variable selection
# =====================================
model_indices_without_selection <- list.files(sprintf("3_results/output-data/model_%s/indices_without_variable_selection" , tempres) , 
                                              pattern = ".csv$" , full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows() %>% 
  mutate(model = factor(model , levels = c("SLR" , "SLMER" , "RF" , "XGB" , "LGB" , "NN")) , 
         product = factor(product , levels = c("OMI" , "TROPOMI")))

indices_selection <- model_indices_without_selection %>% 
  mutate(selection = "without") %>% 
  bind_rows(
    model_indices %>% 
      filter(model %in% c("RF" , "XGB", "LGB" , "NN") & product == "TROPOMI") %>% 
      mutate(selection = "with")
  ) %>% 
  mutate(type = factor(type , levels = c("training" , "CV" , "spatialCV" , "temporalCV")))

indices_selection %>% 
  filter(name == "R2" & type == "CV") %>% 
  ggplot(aes(x = model , y = value , group = selection)) +
  geom_bar(aes(fill = selection) , stat = "identity" , width = 0.5 , position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width=0.6)) +
  geom_errorbar(aes(ymin = min , ymax = max) , 
                color = "black" , position = position_dodge(width=0.6) , width = 0.3) +
  coord_cartesian(ylim = c(0.2,1)) +
  scale_fill_solarized() +
  labs(x = "Model" , y = expression("CV-R"^2) , 
       title = "Comparison between land-use machine-learning models \nwith and without variable selection" , 
       subtitle = "Daily models; with TROPOMI" , fill = "Variable selection") +
  theme_bw() +
  theme(legend.position = "bottom")
  
save_plot(
  sprintf("3_results/output-graph/model_%s/compare_variable_selection.png" , tempres) , 
  plot = last_plot() , 
  base_width = 5.5 , base_height = 4
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


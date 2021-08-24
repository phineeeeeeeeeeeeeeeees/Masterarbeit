#####################################################################################################
# Masterarbeit
# Modeling
# Annual model: Geographically weighted regression -- OMI and TROPOMI
# 2021-06-10
#####################################################################################################

# =====================================
# required packages
# =====================================
library(readr) 
library(sf) ; library(stars)
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(ggsci) ; library(ggthemes) ; library(cowplot)
library(lubridate) ; library(stringr)
library(spdep)
library(Metrics)
library(GWmodel)

source("2_scripts/utils_model-eval.R")

# =====================================
# load datasets
# =====================================
# training data
data_annual <- read_csv("1_data/processed/cleaned/extracted/annual_scaled.csv") 

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
data_annual <- data_annual %>% 
  inner_join(sites_CV , by = "Station_name") %>% # this also excludes the 3 sites that are outside Switzerland and was not counted in the CV groups
  select(-geometry)

# Switzerland shapefile
CH <- st_read("1_data/raw/Switzerland_shapefile/CHE_adm0.shp") %>% 
  st_transform(st_crs(2056))

# //////////////////////////////////////////////////////////////////////////
# model fitting
# //////////////////////////////////////////////////////////////////////////
model_name <- "Geographically weighted regression"
model_abbr <- "GWR"
SAT_product <- c("spatial" , "OMI" , "TROPOMI")[1]

# =====================================
# SLR model formulas
# =====================================
# using the variables from the SLR results (SLR summary table)
formula_SLR_gwr <- read_csv(sprintf("3_results/output-data/model_annual/SLR_summary/%s.csv" , SAT_product)) %>% 
  select(term) %>% 
  filter(term != "(Intercept)") %>% 
  unlist %>% 
  unname() %>% 
  paste(collapse = "+") %>% 
  sprintf("NO2~%s" , .) %>% 
  formula() 

# a vector of the included variables 
included_var <- labels(terms(formula_SLR_gwr)) 

# =====================================
# GWR
# =====================================
bandwidth = 1000
gwr_SLR <- gwr.basic(formula_SLR_gwr , 
                     data = data_annual %>% 
                       st_as_sf(coords = c("X" , "Y") , crs = st_crs(2056)) %>% 
                       as_Spatial() , 
                     bw = bandwidth , 
                     adaptive = TRUE)

# =====================================
# cross validation
# =====================================
# conventional CV
for(k in as.factor(1:k_fold)){
  # partition
  training.data <- data_annual %>% filter(CV != k)
  testing.data <- data_annual %>% filter(CV == k)
  # train model
  model_train <- gwr.predict(formula_SLR_gwr , 
                             data = training.data %>% 
                               st_as_sf(coords = c("X" , "Y") , crs = st_crs(2056)) %>% 
                               as_Spatial() , 
                             predictdata = testing.data %>% 
                               st_as_sf(coords = c("X" , "Y") , crs = st_crs(2056)) %>% 
                               as_Spatial()  , 
                             bw = bandwidth , 
                             adaptive = TRUE)
  # prediction
  prediction_test <- testing.data %>% 
    select(Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = model_train$SDF$prediction)
  # prediction data.frame
  if(as.character(k) == "1"){
    gwr_SLR_prediction_CV <- prediction_test # <-
  }else{ # append
    gwr_SLR_prediction_CV <- bind_rows(gwr_SLR_prediction_CV , prediction_test) # <-
  }
  # clean environment
  rm(training.data , testing.data , model_train , prediction_test , k)
}

# spatial CV
for(k in as.factor(1:k_fold)){
  # partition
  training.data <- data_annual %>% filter(spatial_CV != k)
  testing.data <- data_annual %>% filter(spatial_CV == k)
  # train model
  model_train <- gwr.predict(formula_SLR_gwr , 
                             data = training.data %>% 
                               st_as_sf(coords = c("X" , "Y") , crs = st_crs(2056)) %>% 
                               as_Spatial() , 
                             predictdata = testing.data %>% 
                               st_as_sf(coords = c("X" , "Y") , crs = st_crs(2056)) %>% 
                               as_Spatial()  , 
                             bw = bandwidth , 
                             adaptive = TRUE)
  # prediction
  prediction_test <- testing.data %>% 
    select(Station_name , NO2 , Type_of_station , spatial_CV) %>% 
    # prediction
    mutate(predicted = model_train$SDF$prediction)
  # prediction data.frame
  if(as.character(k) == "1"){
    gwr_SLR_prediction_CV_sp <- prediction_test # <-
  }else{ # append
    gwr_SLR_prediction_CV_sp <- bind_rows(gwr_SLR_prediction_CV_sp , prediction_test) # <-
  }
  # clean environment
  rm(training.data , testing.data , model_train , prediction_test , k)
}

# combine the prediction of the two CV
gwr_SLR_prediction_CV <- gwr_SLR_prediction_CV %>% 
  left_join(gwr_SLR_prediction_CV_sp , 
            by = c("Station_name" , "NO2" , "Type_of_station") , 
            suffix = c("_CV" , "_spatialCV"))



# //////////////////////////////////////////////////////////////////////////
# model evaluation
# //////////////////////////////////////////////////////////////////////////
# =====================================
# model summary
# =====================================
gwr_SLR

# =====================================
# coefficient in space
# =====================================
gwr_SLR_coef_sf <- gwr_SLR$SDF %>% 
  st_as_sf %>% 
  as.data.frame() %>% 
  # coefficients
  select(geometry , all_of(included_var)) %>% 
  pivot_longer(cols = -geometry , names_to = "variable" , values_to = "coef") %>% 
  # coefficient standard error
  left_join(
    gwr_SLR$SDF %>% 
      st_as_sf %>% 
      as.data.frame() %>% 
      # standard error
      select(geometry , all_of(paste0(included_var , "_SE"))) %>% 
      pivot_longer(cols = -geometry , names_to = "variable" , values_to = "SE") %>% 
      mutate(variable = str_replace(variable , "_SE$" , "")), 
    by = c("variable" , "geometry")
  ) %>% 
  st_as_sf()

# =====================================
# observed, predicted, residuals
# =====================================
gwr_SLR_prediction <- data_annual %>% 
  select(Station_name , NO2 , Type_of_station , X , Y) %>% 
  # prediction
  mutate(predicted = gwr_SLR$SDF$yhat) %>% 
  # CV-prediction
  full_join(gwr_SLR_prediction_CV , 
            by = c("Station_name" , "NO2" , "Type_of_station")) %>% 
  # residuals
  mutate(residual = NO2 - predicted , 
         residual_CV = NO2 - predicted_CV , 
         residual_spatialCV = NO2 - predicted_spatialCV)


# =====================================
# performance indices
# =====================================
# model performance indices as a data.frame
gwr_SLR_indices <- gwr_SLR_prediction %>%   # <-
  eval_performance_indices()

# =====================================
# visualization 
# =====================================
out_dirpath_plots <- sprintf("3_results/output-graph/model_annual/%s" , model_abbr)
if(!dir.exists(out_dirpath_plots)) dir.create(out_dirpath_plots)

# predicted <-> observed
plot_obs_pred(gwr_SLR_prediction , 
              sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/obs-pred_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 5 , base_height = 3.5
)

# residual diagnostic plots
plot_resid(gwr_SLR_prediction , 
           title_text = sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/residuals_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 7.8 , base_height = 6
)

# coefficients in space
gwr_SLR_coef_sf %>% 
  group_by(variable) %>% 
  group_map(
    ~ggplot(.x) +
      geom_sf(data = CH) +
      geom_sf(aes(color = coef)) +
      scale_color_viridis_c() +
      labs(title = .x$variable , color = expression(beta*"(u"[i]*",v"[i]*")")) +
      theme_bw() , 
    .keep = TRUE
  ) %>% 
  cowplot::plot_grid(plotlist = . , align = 'hv', ncol = 3) %>% 
  plot_grid(
    ggdraw() + 
      draw_label(sprintf("%s (%s)" , str_to_title(model_name) , SAT_product) , x = 0, hjust = 0) +
      theme(plot.margin = margin(0, 0, 0, 15)) , 
    . , 
    ncol = 1,
    rel_heights = c(0.05, 1)
  )
save_plot(
  sprintf("%s/coef-space_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 15 , base_height = 7
)

# gwr_SLR_coef_sf %>% 
#   gather("par" , "value" , coef , SE) %>% 
#   group_by(variable , par) %>% 
#   group_map(
#     ~ggplot(.x) +
#       geom_sf(data = st_simplify(CH , dTolerance = 1000)) +
#       geom_sf(aes(color = value)) +
#       scale_color_viridis_c() +
#       labs(title = .x$variable , color = expression(beta*"(u"[i]*",v"[i]*")")) +
#       theme_bw() , 
#     .keep = TRUE
#   ) %>% 
#   cowplot::plot_grid(plotlist = . , align = 'hv', ncol = length(included_var) , byrow = FALSE)

# =====================================
# spatial autocorrelation of residuals
# =====================================
moran_df <- eval_resid_moran(gwr_SLR_prediction)

# visualization
plot_resid_map(gwr_SLR_prediction , 
               sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/residual-map_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 6 , base_height = 4
)

# =====================================
# export datasets
# =====================================
{
  # export the model coefficients in space
  out_dirpath_coef_sf <- "3_results/output-data/model_annual/GWR_coef"
  if(!dir.exists(out_dirpath_coef_sf)) dir.create(out_dirpath_coef_sf)
  gwr_SLR_coef_sf %>%
    st_write(sprintf("%s/%s.shp" , out_dirpath_coef_sf , SAT_product))
  
  # export the predicted values
  out_dirpath_predicted <- "3_results/output-data/model_annual/observed-predicted"
  if(!dir.exists(out_dirpath_predicted)) dir.create(out_dirpath_predicted)
  gwr_SLR_prediction %>% # <-
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_predicted , model_abbr , SAT_product))
  
  # export the model performance indices
  out_dirpath_indices <- "3_results/output-data/model_annual/indices"
  if(!dir.exists(out_dirpath_indices)) dir.create(out_dirpath_indices)
  gwr_SLR_indices %>% # <-
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_indices , model_abbr , SAT_product))
  
  # Moran's I
  out_dirpath_Moran <- "3_results/output-data/model_annual/Moran"
  if(!dir.exists(out_dirpath_Moran)) dir.create(out_dirpath_Moran)
  moran_df %>% 
    pivot_longer(cols = everything()) %>% 
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_Moran , model_abbr , SAT_product))
}

# =====================================
# export model
# =====================================
{
  out_dirpath_model <- "3_results/output-model/model_annual"
  if(!dir.exists(out_dirpath_model)) dir.create(out_dirpath_model)
  saveRDS(gwr_SLR , 
          file = sprintf("%s/%s_%s.rds" , out_dirpath_model , model_abbr , SAT_product))
}


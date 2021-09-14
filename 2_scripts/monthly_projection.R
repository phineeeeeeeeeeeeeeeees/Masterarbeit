#####################################################################################################
# Masterarbeit
# Modeling
# Monthly models: projection
# 2021-09-14
#####################################################################################################

# =====================================
# required packages
# =====================================
library(sf) ; library(stars)
library(readr) ; library(vroom) ; library(data.table)
library(dtplyr) ; library(multidplyr)
library(dplyr , warn.conflicts = FALSE) ; library(tidyr) 
library(ggplot2) ; library(ggsci) ; library(ggthemes) ; library(cowplot)
library(lubridate) ; library(stringr)
library(lme4) ; library(ranger) ; library(xgboost) ; library(lightgbm) ; library(keras)

# =====================================
# load models
# =====================================
in_dirpath_models <- "3_results/output-model/model_monthly"

# models saved as .rds files 
in_filepath_RDS <- list.files(in_dirpath_models , full.names = TRUE , pattern = ".rds$")
for(RDSfile in in_filepath_RDS){
  model_temp <- readRDS(RDSfile)
  assign(
    basename(RDSfile) %>% str_remove(".rds") %>% str_c("model_" , . ) , 
    model_temp
  )
  rm(model_temp , RDSfile)
}
# models saved as .txt files (LightGBM)
in_filepath_LGB <- list.files(in_dirpath_models , full.names = TRUE , pattern = ".txt$")
for(txtfile in in_filepath_LGB){
  model_temp <- lgb.load(txtfile)
  assign(
    basename(txtfile) %>% str_remove(".txt") %>% str_c("model_" , . ) , 
    model_temp
  )
  rm(model_temp , txtfile)
}
# models saved as HDF files (keras)
in_filepath_HDF <- list.files(in_dirpath_models , full.names = TRUE , pattern = ".hdf$")
for(HDFfile in in_filepath_HDF){
  model_temp <- load_model_hdf5(HDFfile)
  assign(
    basename(HDFfile) %>% str_remove(".hdf") %>% str_c("model_" , . ) , 
    model_temp
  )
  rm(model_temp , HDFfile)
}
in_filepath_csv <- list.files(in_dirpath_models , full.names = TRUE , pattern = "csv$") # the variables of NN
for(CSVfile in in_filepath_csv){
  df_temp <- read_csv(CSVfile) %>% 
    select(value) %>% unlist %>% unname()
  assign(
    basename(CSVfile) %>% str_remove(".csv") %>% str_c("model_" , . ) , 
    df_temp
  )
  rm(df_temp , CSVfile)
}

# =====================================
# load data
# =====================================
# # import full dataset
# spatial_df <- fread("1_data/processed/cleaned/data-frame/spatial_df.csv") %>%
#   mutate(across(c(x,y) , as.integer)) %>%
#   as.data.table()
# spatialtemporal_df <- fread("1_data/processed/cleaned/data-frame/spatialtemporal_monthly_df.csv") %>%
#   mutate(across(c(x,y) , as.integer)) %>%
#   as.data.table()
# st_NDVI_df <- fread("1_data/processed/cleaned/data-frame/spatialtemporal_NDVI_monthly_df.csv") %>%
#   mutate(across(c(x,y) , as.integer)) %>%
#   as.data.table()
# lookup_1000m <- fread("1_data/processed/cleaned/data-frame/AOI_spatial-lookup-table_1000to100m.csv") %>%
#   mutate(across(everything() , as.integer)) %>%
#   as.data.table()
# lookup_250m <- fread("1_data/processed/cleaned/data-frame/AOI_spatial-lookup-table_250to100m.csv") %>%
#   mutate(across(everything() , as.integer)) %>%
#   as.data.table()

# Switzerland
# CH <- read_sf("1_data/raw/Switzerland_shapefile/CHE_adm0.shp") %>% 
#   st_transform(st_crs(2056))

# =====================================
# merging data
# =====================================
# full_df <- spatial_df %>%
#   # 1000m lookup-table
#   left_join(lookup_1000m , by = c("x" = "x_100" , "y" = "y_100")) %>%
#   # 1000m dataset
#   left_join(spatialtemporal_df , by = c("x_1000" = "x" , "y_1000" = "y")) %>%
#   # 250m loop-up table
#   left_join(lookup_250m , by = c("x" = "x_100" , "y" = "y_100")) %>%
#   # 250m dataset
#   left_join(st_NDVI_df , by = c("x_250" = "x" , "y_250" = "y" , "month" = "month")) %>%
#   # access results
#   as.data.table()

# # full_df of Zurich area (sub-region)
# Zurich <- read_csv("1_data/processed/cleaned/data-frame/Zurich_area.csv",
#                    col_types = cols(x = col_integer(), y = col_integer()))
# Zurich_full_df <- Zurich %>%
#   left_join(spatial_df , by = c("x" , "y")) %>% 
#   # 1000m lookup-table
#   left_join(lookup_1000m , by = c("x" = "x_100" , "y" = "y_100")) %>%
#   # 1000m dataset
#   left_join(spatialtemporal_df , by = c("x_1000" = "x" , "y_1000" = "y")) %>%
#   # 250m loop-up table
#   left_join(lookup_250m , by = c("x" = "x_100" , "y" = "y_100")) %>%
#   # 250m dataset
#   left_join(st_NDVI_df , by = c("x_250" = "x" , "y_250" = "y" , "month" = "month")) %>%
#   # access results
#   as.data.table()
# write_csv(Zurich_full_df , "1_data/processed/cleaned/data-frame/monthly_full_df_Zurich.csv")

Zurich_full_df <- read_csv(
  "1_data/processed/cleaned/data-frame/monthly_full_df_Zurich.csv", 
  col_types = cols(x = col_integer(), y = col_integer(), 
                   x_250 = col_integer(), y_250 = col_integer(), 
                   x_1000 = col_integer(), y_1000 = col_integer() , 
                   month = col_integer())
)
# only mapping the Zurich area
full_df <- Zurich_full_df
rm(Zurich_full_df) ; gc()

# =====================================
# model projections
# =====================================
# SLR ------------------------------------------------------------
projection_SLR <- full_df %>%
  # transformation of month (fit cosine wave)
  mutate(month_trans = cos(2 * pi * month/12 + -0.5593252)) %>% 
  # prediction
  mutate(pred_SLR_OMI = predict(model_SLR_OMI , newdata = .) %>% unname , 
         pred_SLR_TROPOMI = predict(model_SLR_TROPOMI , newdata = .) %>% unname) %>% 
  # the results
  select(x,y,month,starts_with("pred_")) %>% 
  as.data.table()

# SLMER ------------------------------------------------------------
projection_SLMER <- full_df %>%
  # prediction (BLUPs)
  mutate(pred_SLMER_OMI = predict(model_SLMER_OMI , newdata = . , re.form = NULL) %>% unname , 
         pred_SLMER_TROPOMI = predict(model_SLMER_TROPOMI , newdata = . , re.form = NULL) %>% unname) %>% 
  # the results
  select(x,y,month,starts_with("pred_")) %>% 
  as.data.table()

# RF ------------------------------------------------------------
projection_RF <- full_df %>% 
  as_tibble() %>% 
  # subset the columns of the predictor variables
  select(x,y,
         one_of(names(model_RF_OMI$variable.importance)) , 
         one_of(names(model_RF_TROPOMI$variable.importance))) %>% 
  # remove rows with missing values (ranger.prediction)
  drop_na() %>% 
  # prediction
  mutate(
    pred_RF_OMI = predict(model_RF_OMI , data = .)$predictions , 
    pred_RF_TROPOMI = predict(model_RF_TROPOMI , data = .)$predictions
  ) %>% 
  # the results
  select(x,y,month,starts_with("pred_")) %>% 
  as.data.table()

# XGBoost ------------------------------------------------------------
projection_XGB_OMI <- full_df %>% 
  # subset the columns of the predictor variables
  select(x,y,one_of(model_XGB_OMI$feature_names)) %>% 
  # remove rows with missing values
  drop_na() %>%
  # prediction
  mutate(
    pred_XGB_OMI = predict(
      model_XGB_OMI , 
      newdata = full_df %>% 
        as_tibble() %>% 
        select(all_of(model_XGB_OMI$feature_names)) %>% 
        drop_na() %>% 
        as.matrix()
    ) , 
  ) %>% 
  # the results
  select(x,y,month,starts_with("pred_")) %>% 
  as.data.table()
projection_XGB_TROPOMI <- full_df %>% 
  # subset the columns of the predictor variables
  select(x,y,one_of(model_XGB_TROPOMI$feature_names)) %>% 
  # remove rows with missing values
  drop_na() %>%
  # prediction
  mutate(
    pred_XGB_TROPOMI = predict(
      model_XGB_TROPOMI , 
      newdata = full_df %>% 
        as_tibble() %>% 
        select(all_of(model_XGB_TROPOMI$feature_names)) %>% 
        drop_na() %>% 
        as.matrix()
    ) , 
  ) %>% 
  # the results
  select(x,y,month,starts_with("pred_")) %>% 
  as.data.table()

# LightGBM ------------------------------------------------------------
projection_LGB_OMI <- full_df %>% 
  # subset the columns of the predictor variables
  select(x,y,one_of(model_LGB_OMI %>% lgb.importance() %>% as_tibble() %>% select(Feature) %>% unlist %>% unname())) %>% 
  # remove rows with missing values
  drop_na() %>%
  # prediction
  mutate(
    pred_LGB_OMI = predict(
      model_LGB_OMI , 
      data = full_df %>% 
        as_tibble() %>% 
        select(all_of(model_LGB_OMI %>% lgb.importance() %>% as_tibble() %>% select(Feature) %>% unlist %>% unname())) %>% 
        drop_na() %>% 
        as.matrix()
    ) , 
  ) %>% 
  # the results
  select(x,y,month,starts_with("pred_")) %>% 
  as.data.table()
projection_LGB_TROPOMI <- full_df %>% 
  # subset the columns of the predictor variables
  select(x,y,one_of(model_LGB_TROPOMI %>% lgb.importance() %>% as_tibble() %>% select(Feature) %>% unlist %>% unname())) %>% 
  # remove rows with missing values
  drop_na() %>%
  # prediction
  mutate(
    pred_LGB_TROPOMI = predict(
      model_LGB_TROPOMI , 
      data = full_df %>% 
        as_tibble() %>% 
        select(all_of(model_LGB_TROPOMI %>% lgb.importance() %>% as_tibble() %>% select(Feature) %>% unlist %>% unname())) %>% 
        drop_na() %>% 
        as.matrix()
    ) , 
  ) %>% 
  # the results
  select(x,y,month,starts_with("pred_")) %>% 
  as.data.table()

# Neural network ------------------------------------------------------------
projection_NN_OMI <- full_df %>% 
  # subset the columns of the predictor variables
  select(x,y,one_of(model_NN_OMI_variables)) %>% 
  # remove rows with missing values
  drop_na() %>% 
  # prediction
  mutate(
    pred_NN_OMI = predict(
      model_NN_OMI , 
      full_df %>% 
        as_tibble() %>% 
        select(all_of(model_NN_OMI_variables)) %>% 
        drop_na() %>% 
        as.matrix()
    )[,1] , 
  ) %>% 
  # the results
  select(x,y,month,starts_with("pred_")) %>% 
  as.data.table()
projection_NN_TROPOMI <- full_df %>% 
  # subset the columns of the predictor variables
  select(x,y,one_of(model_NN_TROPOMI_variables)) %>% 
  # remove rows with missing values
  drop_na() %>% 
  # prediction
  mutate(
    pred_NN_TROPOMI = predict(
      model_NN_TROPOMI , 
      full_df %>% 
        as_tibble() %>% 
        select(all_of(model_NN_TROPOMI_variables)) %>% 
        drop_na() %>% 
        as.matrix()
    )[,1] , 
  ) %>% 
  # the results
  select(x,y,month,starts_with("pred_")) %>% 
  as.data.table()

# =====================================
# joining
# =====================================
projection <- projection_SLR %>% 
  full_join(projection_SLMER , by = c("x" , "y" , "month")) %>% 
  full_join(projection_RF , by = c("x" , "y" , "month")) %>% 
  full_join(projection_XGB_OMI , by = c("x" , "y" , "month")) %>% 
  full_join(projection_XGB_TROPOMI, by = c("x" , "y" , "month")) %>% 
  full_join(projection_LGB_OMI , by = c("x" , "y" , "month")) %>% 
  full_join(projection_LGB_TROPOMI, by = c("x" , "y" , "month")) %>% 
  full_join(projection_NN_OMI , by = c("x" , "y" , "month")) %>% 
  full_join(projection_NN_TROPOMI, by = c("x" , "y" , "month")) %>% 
  as.data.table()

# export
out_dirpath_map <- "3_results/output-data/model_monthly/projection_monthly_Zurich"
if(!dir.exists(out_dirpath_map)) dir.create(out_dirpath_map , recursive = TRUE)
projection %>% 
  fwrite(sprintf("%s/projection_monthly_Zurich.csv" , out_dirpath_map))
# =====================================
# convert the prediction maps to stars
# =====================================
projection_stars <- projection %>%
  setNames(str_remove(colnames(.) , "pred_")) %>% 
  st_as_stars(dim = c("x" , "y" , "month") , coords = 1:3) %>%
  select(-month) %>% 
  st_set_crs(st_crs(2056)) %>% 
  merge()

# visualization
# ggplot() +
#   geom_stars(data = projection_stars) +
#   #geom_sf(data = CH , fill = NA , color = "white") +
#   coord_sf(crs = st_crs(2056) , expand = FALSE) +
#   facet_grid(attributes~month) +
#   scale_fill_gradientn(colors = RColorBrewer::brewer.pal(8,"RdYlGn")[8:1] ) +
#   labs(x = "" , y = "" , 
#        title = expression("Model-predicted ground-level NO"[2] * " concentration") ,
#        fill = expression("NO"[2] * " (µg/m"^3 * ")")) +
#   theme_bw() +
#   theme(axis.text = element_text(size = 6) , 
#         axis.text.x = element_text(angle = 90 , vjust = 0.5 , hjust = 1))

# export the projected NO2
models <- st_get_dimension_values(projection_stars , 4)
for(i in 1:length(models)){
  projection_stars[,,,,i] %>% 
    split(4) %>%
    write_stars(sprintf("%s/%s.tif" , out_dirpath_map , models[i]))
}
rm(i)


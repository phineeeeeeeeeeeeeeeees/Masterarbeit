#####################################################################################################
# Masterarbeit
# Modeling
# Annual models: mapping
# 2021-08-31
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
library(GWmodel) ; library(ranger) ; library(xgboost) ; library(lightgbm) ; library(keras)

# =====================================
# load models
# =====================================
in_dirpath_models <- "3_results/output-model/model_annual"
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

# =====================================
# load data
# =====================================
# import full dataset
spatial_df <- fread("1_data/processed/cleaned/data-frame/spatial_df.csv")
# spatial_df <- vroom("1_data/processed/cleaned/data-frame/spatial_df.csv" ,
#                     col_select = c(x,y,one_of(included_var))) %>%
#   mutate(across(c(x,y) , as.integer))
spatialtemporal_df <- fread("1_data/processed/cleaned/data-frame/spatialtemporal_annual_df.csv")
# spatialtemporal_df <- vroom("1_data/processed/cleaned/data-frame/spatialtemporal_annual_df.csv" ,
#                             col_select = c(x,y,one_of(included_var))) %>%
#   mutate(across(c(x,y) , as.integer))
st_NDVI_df <- fread("1_data/processed/cleaned/data-frame/spatialtemporal_NDVI_annual_df.csv")
# st_NDVI_df <- vroom("1_data/processed/cleaned/data-frame/spatialtemporal_NDVI_annual_df.csv" ,
#                     col_select = c(x,y,one_of(included_var))) %>%
#   mutate(across(c(x,y) , as.integer))
lookup_1000m <- fread("1_data/processed/cleaned/data-frame/AOI_spatial-lookup-table_1000to100m.csv")
# lookup_1000m <- vroom("1_data/processed/cleaned/data-frame/AOI_spatial-lookup-table_1000to100m.csv") %>%
#   mutate(across(everything() , as.integer))
lookup_250m <- fread("1_data/processed/cleaned/data-frame/AOI_spatial-lookup-table_250to100m.csv") 
# lookup_250m <- vroom("1_data/processed/cleaned/data-frame/AOI_spatial-lookup-table_250to100m.csv") %>%
#   mutate(across(everything() , as.integer))

# =====================================
# merging data
# =====================================
full_df <- spatial_df %>%
  # 1000m lookup-table
  left_join(lookup_1000m , by = c("x" = "x_100" , "y" = "y_100")) %>%
  # 1000m dataset
  left_join(spatialtemporal_df , by = c("x_1000" = "x" , "y_1000" = "y")) %>%
  # 250m loop-up table
  left_join(lookup_250m , by = c("x" = "x_100" , "y" = "y_100")) %>%
  # 250m dataset
  left_join(st_NDVI_df , by = c("x_250" = "x" , "y_250" = "y")) %>% 
  # access results
  as.data.table()


# =====================================
# model projections
# =====================================
projection_SLR <- full_df %>%
  mutate(pred_SLR_OMI = predict(lm_SLR , newdata = .) %>% unname)

# convert the NO2 map to stars
projection_SLR_stars <- projection_SLR %>%
  select(x,y,NO2_pred) %>%
  st_as_stars(dim = c("x" , "y")) %>%
  st_set_crs(st_crs(2056))


# visualization
ggplot() +
  geom_stars(data = projection_SLR_stars) +
  geom_sf(data = AOI , fill = NA , color = "white") +
  coord_sf(crs = st_crs(2056) , expand = FALSE) +
  scale_fill_viridis_c() +
  labs(title = expression("SLR-predicted ground-level NO"[2] * " concentration") ,
       fill = expression("NO"[2] * " (µg/m"^3 * ")")) +
  theme_bw()

# zoom to a Canton
zoomed.area <- st_read("1_data/raw/Switzerland_shapefile/CHE_adm1.shp") %>%
  st_transform(st_crs(2056)) %>%
  filter(str_detect(NAME_1 , "Zürich"))
ggplot() +
  geom_stars(data = projection_SLR_stars[zoomed.area]) +
  coord_sf(crs = st_crs(2056) , expand = FALSE) +
  scale_fill_viridis_c() +
  labs(title = expression("SLR-predicted ground-level NO"[2] * " concentration") ,
       fill = expression("NO"[2] * " (µg/m"^3 * ")")) +
  theme_bw()

# export the projected NO2
projection_SLR_stars %>%
  write_stars("3_results/output-data/model_annual/projection_SLR_OMI.tif")



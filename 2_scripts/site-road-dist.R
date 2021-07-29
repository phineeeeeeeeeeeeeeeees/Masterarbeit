#####################################################################################################
# Masterarbeit
# Modeling
# Model diagnostic: distance to roads
# 2021-07-28
#####################################################################################################
# CRS: 2056

# =====================================
# required packages
# =====================================
library(readr)
library(sf)
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(ggsci) ; library(ggthemes) ; library(cowplot)
library(lubridate) ; library(stringr)

# =====================================
# load datasets
# =====================================
# monitoring sites
sites <- st_read("1_data/processed/cleaned/extracted//NO2-sites_5-fold-CV.shp") %>% 
  rename(Station_name = Station)

# road network
roadnet <- st_read("1_data/raw/Roads/Roads_SonBase_sel.shp") %>% 
  st_transform(crs = st_crs(sites)) 

# =====================================
# main roads
# =====================================
# Main roads were extracted from the sonBASE traffic database, linked to the VECTOR25 road network, by selecting roads with an Annual Average Daily Traffic (AADT) greater than 5000
# 1.	de Hoogh, K.; Héritier, H.; Stafoggia, M.; Künzli, N.; Kloog, I., Modelling daily PM2. 5 concentrations at high spatio-temporal resolution across Switzerland. Environmental Pollution 2018, 233, 1147-1154.
roadnet_main <- roadnet %>% 
  filter(DTV > 5000)

# =====================================
# calculate the distance from each monitoring sites to the roads
# =====================================
# all roads
# indexing: nearest road of each monitoring site
nearest_road_bysite <- st_nearest_feature(sites , roadnet)
# distance
dist_nearest_road_bysite <- roadnet %>% 
  # the roads that are nearest to each monitoring site (by the index of nearest feature)
  slice(nearest_road_bysite) %>% 
  # the distance between the monitoring sites and the nearest road
  st_distance(sites , by_element = TRUE)
# road information
info_nearest_road_bysite <- roadnet %>% 
  slice(nearest_road_bysite) %>% 
  select(OBJECTID , DTV , StreetType) %>% 
  rename_with(~str_c(. , "_all") , -geometry)

# main roads
# indexing: nearest main road of each monitoring site
nearest_mainroad_bysite <- st_nearest_feature(sites , roadnet_main)
# distance
dist_nearest_mainroad_bysite <- roadnet_main %>% 
  # the main roads that are nearest to each monitoring site (by the index of nearest feature)
  slice(nearest_mainroad_bysite) %>% 
  # the distance between the monitoring sites and the nearest main road
  st_distance(sites , by_element = TRUE)
# road information
info_nearest_mainroad_bysite <- roadnet_main %>% 
  slice(nearest_mainroad_bysite) %>% 
  select(OBJECTID , DTV , StreetType) %>% 
  rename_with(~str_c(. , "_main") , -geometry)

# =====================================
# combine information
# =====================================
site_road <- sites %>% 
  mutate(dist_nearest_road_bysite = dist_nearest_road_bysite , 
         dist_nearest_mainroad_bysite = dist_nearest_mainroad_bysite) %>% 
  bind_cols(
    info_nearest_road_bysite %>% 
      as.data.frame() %>% 
      select(-geometry)
  ) %>% 
  bind_cols(
    info_nearest_mainroad_bysite %>% 
      as.data.frame() %>% 
      select(-geometry)
  )

# =====================================
# export 
# =====================================
site_road %>% 
  as_tibble() %>% 
  write_csv("1_data/processed/cleaned/extracted/site-road-distance.csv")


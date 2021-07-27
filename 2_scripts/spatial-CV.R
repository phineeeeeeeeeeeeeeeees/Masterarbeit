#####################################################################################################
# Masterarbeit
# Modeling
# Spatially blocked cross validation
# 2021-06-10
#####################################################################################################

library(readr) ; library(sf)
library(stars)
library(dplyr) ; library(tidyr) ; library(stringr)
library(ggplot2); library(ggsci)

# =====================================
# load datasets
# =====================================
# monitoring data
data_daily <- read_csv("1_data/processed/cleaned/extracted/daily_scaled.csv")

# AOI: national boundary
CH <- st_read("1_data/raw/Switzerland_shapefile/CHE_adm0.shp") %>% 
  select(ID_0) %>% # drop unnecessary columns
  st_transform(st_crs(2056)) 

# monitoring sites
NO2_sites <- read_csv("1_data/raw/NO2-monitoring/metadaten_idbluft_supplemented.csv") %>% 
  # clean column names
  setNames(str_replace_all(colnames(.) , " " , "_")) %>% 
  setNames(str_replace_all(colnames(.) , "-" , "_")) %>% 
  # convert to spatial points
  st_as_sf(coords = c("x" , "y") , crs = st_crs(2056))

# DEM (for visualization)
DEM <- read_stars("1_data/raw/EU-DEM-1.1/eu_dem_v11_E40N20_AOI.tif") %>% 
  st_warp(cellsize = c(2000,2000) , crs = st_crs(CH)) %>% 
  .[CH]

# =====================================
# filtering sites: missing values
# only select the sites with 30 or more daily measurements
# =====================================
NO2_sites_valid <- data_daily %>% 
  select(date , NO2 , Station_name) %>% 
  group_by(Station_name) %>% 
  summarize(missing_n = is.na(NO2) %>% sum) %>% 
  ungroup() %>% 
  mutate(missing_percent = missing_n / 365) %>% 
  arrange(-missing_n) %>% 
  # only select the sites with 30 or more daily measurements
  filter(missing_n < 365-30)

# =====================================
# k-fold cross validation
# =====================================
k_fold <- 5

# spatially-blocked cross validation
# set.seed(10142)
# set.seed(27)
set.seed(30)
CH_CV <- CH %>%
  # make grids
  st_make_grid(n = c(4,3)) %>% 
  st_as_sf() %>% 
  st_intersection(CH %>% st_geometry) %>% 
  # randomly assign cross validation index
  mutate(spatial_CV = sample(rep(1:k_fold, length.out = n()) , replace = FALSE) %>% as.factor())
NO2_sites_CV <- NO2_sites %>% 
  select(Station_name) %>% 
  # CV grids
  st_join(CH_CV , left = FALSE) %>% 
  # the stations that (1) exist in the monitoring data and (2) have >30 daily measurements
  filter(Station_name %in% NO2_sites_valid$Station_name)

# conventional cross validation
set.seed(77)
NO2_sites_CV <- NO2_sites_CV %>% 
  mutate(CV = sample(rep(1:k_fold , length.out = n() , replace = FALSE)) %>% as.factor())

# =====================================
# summary
# =====================================
# number of sites in each CV-group
table(NO2_sites_CV$spatial_CV)
table(NO2_sites_CV$CV)
# visualization
NO2_sites_CV %>% 
  # pivot_longer
  gather(key = "type" , value = "fold" , ends_with("CV")) %>% 
  # rename
  mutate(type = ifelse(str_detect(type , "spatial") , "spatially-blocked CV" , "random CV")) %>% 
  # visualization
  group_by(type) %>% 
  group_map(
    ~ ggplot() +
      geom_sf(data = CH_CV , color = "azure3") +
      geom_sf(data = .x , aes(color = fold)) +
      scale_color_jco(labels = paste0(1:10 , " (n=" , table(.x$fold) , ")")) +
      labs(title = sprintf("%s-fold %s cross validation" , k_fold , str_remove(.y$type , " CV")) , 
           color = "CV-group") +
      theme_light() +
      theme(legend.position = "bottom")
  ) %>% 
  cowplot::plot_grid(plotlist = . , align = "h" , axis = "tb" , nrow = 1)
  
# cross table: CV group and station type
NO2_sites_CV %>%
  st_join(NO2_sites , left = TRUE) %>%
  xtabs(~Type_of_station + CV  , data = .)
NO2_sites_CV %>%
  st_join(NO2_sites , left = TRUE) %>%
  xtabs(~Type_of_station + spatial_CV  , data = .)

# visualization
NO2_sites_CV %>% 
  st_join(NO2_sites %>% select(Type_of_station) , left = TRUE) %>% 
  ggplot() +
  geom_stars(data = DEM , alpha = 0.5) +
  geom_sf(aes(color = Type_of_station)) +
  scale_fill_gradientn(colors = terrain.colors(5) , na.value = NA) +
  labs(x = "Longitude" , y = "Latitude" , 
       fill = "Altitude (m)" , color = "Type of \nmonitoring \nsites" , 
       title = expression("NO"[2] *" monitoring sites used in the study")) +
  theme_bw()

# =====================================
# export the sites with CV
# =====================================
NO2_sites_CV %>% 
  rename(Station = Station_name) %>% 
  st_write(sprintf("1_data/processed/cleaned/extracted/NO2-sites_%s-fold-CV.shp" , k_fold) , append = FALSE)


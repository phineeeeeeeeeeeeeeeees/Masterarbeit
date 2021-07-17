#####################################################################################################
# Masterarbeit
# Prepare training data (1/3): resampling
# (only thee spatial predictors here in this script)
# 2021-06-04
#####################################################################################################
# coordinate system to work with: EPSG: 2056 (CH1903+/ LV95)

# =====================================
# required packages
# =====================================
library(stars) ; library(sf) 
library(dplyr) ; library(tidyr)
library(ggplot2)
library(lubridate) ; library(stringr)
library(readr)

# =====================================
# set output directory
# =====================================
out_dirpath_cleaned <- "1_data/processed/cleaned"
out_dirpath_cleaned_sp <- paste0(out_dirpath_cleaned , "/spatial")
if(!dir.exists(out_dirpath_cleaned)) dir.create(out_dirpath_cleaned)
if(!dir.exists(out_dirpath_cleaned_sp)) dir.create(out_dirpath_cleaned_sp)

# //////////////////////////////////////////////////////////////////////////
# importing data
# //////////////////////////////////////////////////////////////////////////
# =====================================
# load area of interest boundary
# =====================================
# AOI: national boundary + 5km buffer
AOI <- st_read("1_data/raw/Switzerland_shapefile/CHE_adm0.shp") %>% 
  select(ID_0) %>% # drop unnecessary columns
  st_transform(st_crs(2056)) %>% 
  st_buffer(10000)

# =====================================
# load spatial predictors
# =====================================
# elevation ----------------------------------------------
# 25m*25m
DEM_raw <- read_stars("1_data/raw/EU-DEM-1.1/eu_dem_v11_E40N20_AOI.tif")

# population ----------------------------------------------
# 100m*100m
population_raw <- read_stars("1_data/raw/GHS-population/GHS_POP_SOURCE_EUROPE_R2016A_3035_100_v1_0_AOI.tif")

# nighttime light ----------------------------------------------
# 0.004˚*0.004˚ (~500m at equator)
light_raw <- read_stars("1_data/raw/VIIRS-nighttime-light/VNL_v2_npp_2019_global_vcmslcfg_c202101211500.average_AOI.tif")

# NOx emissions ----------------------------------------------
# 200m*200m
emission_raw <- read_stars(list.files("1_data/raw/Emissions/NOx_2015" , 
                                      pattern = "enox" , full.names = TRUE) , 
                           along = list(src = c("agfo" , "hoco" , "indu" , "traf" , "wood")) , 
                           proxy = FALSE) %>% 
  st_set_crs(st_crs(21781)) %>% 
  setNames("enox")

# traffic intensity ----------------------------------------------
# 100m*100m
traffint_raw <- read_stars("1_data/raw/Traffic/traffint") %>% 
  st_set_crs(st_crs(21781))

# major road density ----------------------------------------------
# 100m*100m
mjdens_raw <- read_stars("1_data/raw/Roads/mjdens.tif")

# all road density ----------------------------------------------
# 100m*100m
rdens_raw <- read_stars("1_data/raw/Roads/rdens.tif")

# number of intersections ----------------------------------------------
# 100m*100m
intersections_raw <- read_stars("1_data/raw/Intersections/int100m.tif")

# distance to nearest major roads ----------------------------------------------
# 100m*100m
nearmjrd_raw <- read_stars("1_data/raw/Roads/nearmjrd.tif")

# NDVI ----------------------------------------------
# 100m*100m
NDVI_sp_raw <- read_stars("1_data/raw/NDVI_30m/ndvi_100m.tif")

# land cover ----------------------------------------------
# 100m*100m
landcover_raw <- read_stars("1_data/raw/CORINE-land-cover/U2018_CLC2018_V2020_20u1_AOI.tif") %>% 
  setNames("lc_code")
# metadata (coding book of land cover types)
landcover_metadata <- read_table2("1_data/raw/CORINE-land-cover/Legend/clc_legend.txt", 
                                  col_names = FALSE, 
                                  col_types = cols(X6 = col_character() , X7 = col_skip())) %>% 
  setNames(c("lc_code" , "R" , "G" , "B" , "alpha" , "lc_ID" , "name1" , "name2" , "name3")) %>% 
  unite(col = "name" , starts_with("name") , sep = "\ ") %>% 
  mutate(name = str_replace_all(name , "NA" , "") %>% str_trim) %>% 
  mutate(color = rgb(R/255,G/255,B/255,alpha/255))
# grouping table for land cover types 
landcover_groups <- data.frame(
  group_code = c(1:6) , 
  group_name = c("residential" , "industry and commercial" , "urban green" , "total built up" , "algriculture" , "semi-natural and forest") , 
  group_abbr = c("RES" , "IND" , "URBGR" , "BUILT" , "AGR" , "NAT") , 
  lc_codes = c("1,2" , "3" , "10" , paste(1:9, collapse = ",") , paste(12:22, collapse = ",") , paste(23:41, collapse = ",")) , 
  stringsAsFactors = FALSE
) %>% 
  separate(lc_codes , 
           into = paste0("x" , 1:19) , 
           sep = ",") %>% 
  pivot_longer(cols = starts_with("x") , values_to = "lc_code") %>% 
  select(-name) %>% 
  filter(!is.na(lc_code)) %>% 
  mutate(lc_code = as.integer(lc_code))

# ggplot() +
#   geom_stars(data = landcover_raw %>% 
#                mutate(lc_code = as.factor(lc_code))) +
#   coord_sf(crs = st_crs(landcover_raw)) +
#   scale_fill_manual(values = landcover_metadata$color %>% 
#                       setNames(landcover_metadata$lc_code) , 
#                     labels = landcover_metadata$name) +
#   theme_bw() +
#   theme(legend.position = "bottom")

# //////////////////////////////////////////////////////////////////////////
# resampling (and some variable-specific preprocessing)
# //////////////////////////////////////////////////////////////////////////
# =====================================
# template target grids 
# =====================================
# 100*100m template grid
AOI_grid100 <- AOI %>% 
  st_rasterize(crs = st_crs(2056) , dx = 100 , dy = 100) %>% 
  mutate(ID_0 = as.integer(ID_0))

# =====================================
# resample to target grid: elevation 
# =====================================
# bilinear-interpolation upscaling 25m to 100m
DEM_rs100 <- DEM_raw %>% 
  st_warp(AOI_grid100 , 
          use_gdal = TRUE , method = "bilinear") %>% 
  setNames("altitude")

# =====================================
# resample to target grid: population 
# =====================================
# nearest-neighbor resampling (100m to 100m, reprojection)
population_rs100 <- population_raw %>% 
  st_warp(AOI_grid100) %>% 
  setNames("population")

# =====================================
# resample to target grid: nighttime light 
# =====================================
# nearest-neighbor downscaling to 100*100m
light_rs100 <- light_raw %>% 
  st_warp(AOI_grid100) %>% 
  setNames("nighttime_light")

# =====================================
# resample to target grid: emission
# =====================================
# nearest-neighbor downscaling 200m to 100m
emission_rs100 <- emission_raw %>% 
  st_warp(AOI_grid100) 

# =====================================
# resample to target grid: traffic intensity
# =====================================
# nearest-neighbor resampling (100m to 100m, reprojection)
traffint_rs100 <- traffint_raw %>% 
  st_warp(AOI_grid100)

# =====================================
# resample to target grid: major road density
# =====================================
# nearest-neighbor resampling (100m to 100m, reprojection)
mjdens_rs100 <- mjdens_raw %>% 
  st_warp(AOI_grid100) %>% 
  setNames("mjroad_density")

# =====================================
# resample to target grid: all road density
# =====================================
# nearest-neighbor resampling (100m to 100m, reprojection)
rdens_rs100 <- rdens_raw %>% 
  st_warp(AOI_grid100) %>% 
  setNames("allroad_density")

# =====================================
# resample to target grid: number of intersections
# =====================================
# nearest-neighbor resampling (100m to 100m, reprojection)
intersection_rs100 <- intersections_raw %>% 
  st_warp(AOI_grid100) %>% 
  setNames("n_intersection")

# =====================================
# resample to target grid: distance to nearest major road
# =====================================
# nearest-neighbor resampling (100m to 100m, reprojection)
nearmjrd_rs100 <- nearmjrd_raw %>% 
  st_warp(AOI_grid100) %>% 
  setNames("nearmjrd")

# =====================================
# resample to target grid: NDVI
# =====================================
# nearest-neighbor resampling (100m to 100m, reprojection)
NDVI_sp_rs100 <- NDVI_sp_raw %>% 
  st_warp(AOI_grid100) %>% 
  setNames("NDVI_sp")

# =====================================
# resample to target grid and grouping: land cover 
# =====================================
# nearest-neighbor resampling (100m to 100m, reprojection)
landcover_rs100 <- landcover_raw %>% 
  st_warp(AOI_grid100) %>% 
  mutate(lc_code = as.integer(lc_code))
# grouping
# create binary (1/0) raster for each group
for(i in unique(landcover_groups$group_code)){
  # abbreviation for naming the objects
  TEMPGROUP_abbr <- unique(landcover_groups$group_abbr[landcover_groups$group_code == i])
  landcover_TEMPGROUP_rs100 <- landcover_rs100 %>% 
    # value 1 for the pixels whose lc_code belongs to that group
    transmute(TEMPGROUP = ifelse(lc_code %in% landcover_groups$lc_code[landcover_groups$group_code == i] , 1 , 0) *
                # mask out NA pixels
                ifelse(is.na(lc_code) , NA , 1)) %>% 
    # rename attribute as group abbreviation
    setNames(TEMPGROUP_abbr)
  assign(sprintf("landcover_%s_rs100" , TEMPGROUP_abbr) , landcover_TEMPGROUP_rs100)
  # clean
  rm(TEMPGROUP_abbr , landcover_TEMPGROUP_rs100)
}
landcover_GROUP_rs100 <- c(landcover_RES_rs100 , landcover_IND_rs100 , 
                           landcover_URBGR_rs100 , landcover_BUILT_rs100 , 
                           landcover_AGR_rs100 , landcover_NAT_rs100) %>% 
  merge() %>% 
  setNames("landcover")
# grouping: visualization
# ggplot() +
#   geom_stars(data = landcover_GROUP_rs100 %>% 
#                mutate(landcover = as.factor(landcover))) +
#   geom_sf(data = Swiss %>% 
#             st_simplify(preserveTopology = TRUE , dTolerance = 1000) , 
#           color = "white" , fill = NA) +
#   scale_fill_manual(values = c("0" = "azure3" , "1" = "deeppink3") , labels = c("Absence", "Presence")) +
#   facet_wrap(~attributes) +
#   labs(x = "Longtitude" , y = "Latitude" , fill = "Land cover") +
#   theme_bw()



# //////////////////////////////////////////////////////////////////////////
# export resampled data
# //////////////////////////////////////////////////////////////////////////
# each variable as a file in `1_data/processed/cleaned`

# =====================================
# spatial
# =====================================
# elevation
DEM_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_cleaned_sp , "/elevation_100m.tif"))
rm(list = ls(pattern = "^DEM")) ; gc()

# population
population_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_cleaned_sp , "/population_100m.tif"))
rm(list = ls(pattern = "^population")) ; gc()

# nighttime light
light_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_cleaned_sp , "/nighttime-light_100m.tif"))
rm(list = ls(pattern = "^light")) ; gc()

# emission
emission_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_cleaned_sp , "/emissions-nox_100m.tif"))
rm(list = ls(pattern = "^emission")) ; gc()

# traffic intensity
traffint_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_cleaned_sp , "/traffic-intensity_100m.tif"))
rm(list = ls(pattern = "^traffint")) ; gc()

# major road density
mjdens_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_cleaned_sp , "/major-road-density_100m.tif"))
rm(list = ls(pattern = "^mjdens")) ; gc()

# all road density
rdens_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_cleaned_sp , "/all-road-density_100m.tif"))
rm(list = ls(pattern = "^rdens")) ; gc()

# number of intersections
intersection_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_cleaned_sp , "/n-intersections_100m.tif"))
rm(list = ls(pattern = "^intersection")) ; gc()

# distance to nearest major road
nearmjrd_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_cleaned_sp , "/dist-near-major-road_100m.tif"))
rm(list = ls(pattern = "^nearmjrd")) ; gc()

# land cover
landcover_rs100 %>% # original coding
  write_stars(dsn = paste0(out_dirpath_cleaned_sp , "/landcover-code_100m.tif"))
landcover_GROUP_rs100 %>%  # binary land cover coding for 6 groups
  write_stars(dsn = paste0(out_dirpath_cleaned_sp , "/landcover-group_binary_100m.tif"))
rm(list = ls(pattern = "^landcover")) ; gc()

# NDVI
NDVI_sp_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_cleaned_sp , "/NDVI_sp_100m.tif"))
rm(list = ls(pattern = "^NDVI_sp")) ; gc()




#####################################################################################################
# Masterarbeit
# Prepare training data (1/3): resampling
# 2021-05-18
#####################################################################################################
# coordinate system to work with: EPSG: 2056 (CH1903+/ LV95)

# =====================================
# required packages
# =====================================
library(readr)
library(stars) ; library(sf) ; library(raster)
library(dplyr) ; library(tidyr) 
library(ggplot2)
library(lubridate) ; library(stringr)

# //////////////////////////////////////////////////////////////////////////
# importing data
# //////////////////////////////////////////////////////////////////////////

# =====================================
# load area of interest boundary
# =====================================
# national boundary
Swiss <- st_read("1_data/raw/Switzerland_shapefile/CHE_adm0.shp") %>% 
  st_transform(st_crs(2056)) %>% 
  st_geometry() 
# adm1
Swiss_adm1 <- st_read("1_data/raw/Switzerland_shapefile/CHE_adm1.shp") %>% 
  st_transform(st_crs(2056))
# AOI (national boundary + 50km buffer)
AOI <- st_read("1_data/raw/Switzerland_shapefile/AOI_4326.shp") %>% 
  st_transform(st_crs(2056))


# =====================================
# load spatial-temporal predictors
# =====================================
# OMI NO2 ----------------------------------------------
OMI_raw <- read_stars("1_data/processed/OMI_imputed/OMI-Aura_L3-OMI_MINDS_NO2d_2019_daily_imputed_AOI.nc") %>% 
  # 365 attributes -> 1 attribute + 365 days (date dimension)
  merge() %>% 
  # set date dimension values 
  st_set_dimensions(3 , 
                    values = st_get_dimension_values(. , 3) %>% 
                      gsub("Band" , "" , .) %>% 
                      as.numeric() %>% 
                      as_date(origin = "2018-12-31") , 
                    names = "date") %>% 
  setNames("OMI_NO2")

# TROPOMI NO2 ----------------------------------------------
TROPOMI_raw <- read_stars("1_data/processed/TROPOMI_imputed/S5P_OFFL_L2__NO2____2019_daily_imputed_AOI.nc") %>% 
  # 365 attributes -> 1 attribute + 365 days (date dimension)
  merge() %>% 
  # set date dimension values 
  st_set_dimensions(3 , 
                    values = st_get_dimension_values(. , 3) %>% 
                      gsub("Band" , "" , .) %>% 
                      as.numeric() %>% 
                      as_date(origin = "2018-12-31") , 
                    names = "date") %>% 
  setNames("TROPOMI_NO2")

# ERA5 meteorological variables ----------------------------------------------
ERA_raw <- read_stars("1_data/raw/ECMWF-ERA5/ERA5-meteorology.nc") %>% 
  st_set_crs(st_crs(4326)) # long-lat coordinates

# NDVI  ----------------------------------------------
in_filepath_NDVI <- list.files("1_data/raw/MODIS-vegetation/NDVI-GTiff" , 
                               pattern = "_AOI.tif$" , full.names = TRUE)
# 0.25˚*0.25˚
NDVI_raw <- read_stars(in_filepath_NDVI , 
                       along = list(
                         date = in_filepath_NDVI %>% 
                           basename() %>% 
                           str_extract("\\d{8}") %>% 
                           as_date()
                       ) , proxy = FALSE) %>% 
  setNames("NDVI")
# crop the extent of the datacube to AOI (too much blank pixels)
NDVI_raw <- NDVI_raw[st_transform(AOI,st_crs(NDVI_raw))]

# ggplot() +
#   geom_stars(data = NDVI_raw) +
#   geom_sf(data = Swiss %>% 
#             st_transform(st_crs(NDVI_raw)) %>% 
#             st_simplify(), 
#           fill = NA , color = "white") +
#   facet_wrap(~date) +
#   coord_sf(expand = FALSE) +
#   scale_fill_viridis_c()

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
# target grids 
# =====================================
# spatial (2D)
# 100*100m
AOI_grid100 <- AOI %>% 
  st_rasterize(crs = st_crs(2056) , dx = 100 , dy = 100) %>% 
  mutate(FID = as.integer(FID)) # integer consumes less space to store data
# 1*1km (for OMI, TROPOMI and ERA5)
AOI_grid1000 <- AOI %>% 
  st_rasterize(crs = st_crs(2056) , dx = 1000 , dy = 1000) %>% 
  mutate(FID = as.integer(FID)) # integer consumes less space to store data


# =====================================
# resample to target grid: OMI
# =====================================
# bilinear-interpolation to 1*1km
# st_warp(use_gdal = TRUE , method = "bilinear")
# resample each day separately (to have the same 2D dimension)
{
  # resample each day to the target grids
  pb <- txtProgressBar(min = 1 , max = 365 , style = 3)
  for(i in 1:dim(OMI_raw)[3]){
    if(i == 1){  # the first day: assign the new stars object `OMI_rs1000` for the resampled data
      OMI_rs1000 <- OMI_raw[,,,i] %>% 
        st_warp(dest = AOI_grid1000  , 
                use_gdal = TRUE , method = "bilinear")
    }else{ # the other days: append the resampled data of the day to the created stars object
      OMI_rs1000 <- c(
        OMI_rs1000 , 
        OMI_raw[,,,i] %>% 
          st_warp(dest = AOI_grid1000  , 
                  use_gdal = TRUE , method = "bilinear")
      )
    }
    setTxtProgressBar(pb , i)
  }
  rm(i,pb)
  # reshape dimensions to 3D
  OMI_rs1000 <- OMI_rs1000 %>% 
    merge() %>% 
    st_set_dimensions(3 , values = st_get_dimension_values(OMI_raw , 3) , names = "date") %>% 
    setNames(names(OMI_raw))
}

# cowplot::plot_grid(
#   ggplot()+
#     geom_stars(data = OMI_raw[,,,10:14] %>% 
#                  st_transform(st_crs(Swiss))) +
#     geom_sf(data = Swiss %>% 
#               st_simplify(preserveTopology = TRUE , dTolerance = 1000) , 
#             color = "white" , fill = NA) +
#     facet_grid(~date) +
#     scale_fill_viridis_c() , 
#   ggplot() +
#     geom_stars(data = OMI_rs1000[,,,10:14]) +
#     geom_sf(data = Swiss %>% 
#               st_simplify(preserveTopology = TRUE , dTolerance = 1000) , 
#             color = "white" , fill = NA) +
#     facet_grid(~date) +
#     scale_fill_viridis_c() , 
#   ncol = 1
# )

# nearest-neighbor-downscaling to 100*100m
OMI_rs100 <- OMI_rs1000 %>% 
  st_warp(AOI_grid100)

# =====================================
# resample to target grid: TROPOMI
# =====================================
# bilinear-interpolation to 1*1km
# st_warp(use_gdal = TRUE , method = "bilinear")
# resample each day separately (to have the same 2D dimension)
{
  # resample each day to the target grids
  pb <- txtProgressBar(min = 1 , max = 365 , style = 3)
  for(i in 1:dim(TROPOMI_raw)[3]){
    if(i == 1){  # the first day: assign the new stars object `TROPOMI_rs1000` for the resampled data
      TROPOMI_rs1000 <- TROPOMI_raw[,,,i] %>% 
        st_warp(dest = AOI_grid1000  , 
                use_gdal = TRUE , method = "bilinear")
    }else{ # the other days: append the resampled data of the day to the created stars object
      TROPOMI_rs1000 <- c(
        TROPOMI_rs1000 , 
        TROPOMI_raw[,,,i] %>% 
          st_warp(dest = AOI_grid1000  , 
                  use_gdal = TRUE , method = "bilinear")
      )
    }
    setTxtProgressBar(pb , i)
  }
  rm(i,pb)
  # reshape dimensions to 3D
  TROPOMI_rs1000 <- TROPOMI_rs1000 %>% 
    merge() %>% 
    st_set_dimensions(3 , values = st_get_dimension_values(TROPOMI_raw , 3) , names = "date") %>% 
    setNames(names(TROPOMI_raw))
}

# cowplot::plot_grid(
#   ggplot()+
#     geom_stars(data = TROPOMI_raw %>% 
#                  filter(date %within% interval("2019-02-01" , "2019-02-10")) %>% 
#                  st_transform(st_crs(Swiss))) +
#     geom_sf(data = Swiss %>% 
#               st_simplify(preserveTopology = TRUE , dTolerance = 1000) , 
#             color = "white" , fill = NA) +
#     facet_grid(~date) +
#     scale_fill_viridis_c(limits = c(0,1e16)) , 
#   ggplot() +
#     geom_stars(data = TROPOMI_rs1000 %>% 
#                  filter(date %within% interval("2019-02-01" , "2019-02-10"))) +
#     geom_sf(data = Swiss %>% 
#               st_simplify(preserveTopology = TRUE , dTolerance = 1000) , 
#             color = "white" , fill = NA) +
#     facet_grid(~date) +
#     scale_fill_viridis_c(limits = c(0,1e16)) , 
#   ncol = 1
# )

# nearest-neighbor-downscaling to 100*100m
TROPOMI_rs100 <- TROPOMI_rs1000 %>% 
  st_warp(AOI_grid100)


# =====================================
# resample to target grid: ERA5
# =====================================
# bilinear-interpolation to 1*1km
# for 12H and 15H, for each variable, for each day separately
{
  hours_ERA <- c(12,15)
  vars_ERA <- names(ERA_raw)
  # for 12H and 15H
  for(H in hours_ERA){
    ERA_raw_H <- ERA_raw %>% 
      filter(hour(time) == H)
    # for each meteorological variable
    for(v in vars_ERA){
      # for every day
      for(i in 1:dim(ERA_raw_H)[3]){
        if(i == 1){ # the first day: assign the new stars object `ERA_HH_var_rs1000_temp` for the resampled data
          ERA_HH_var_rs1000_temp <- ERA_raw_H[v,,,i] %>% 
            st_warp(dest = AOI_grid1000  , 
                    use_gdal = TRUE , method = "bilinear")
        }else{ # the other days: append the resampled data of the day to the created stars object
          ERA_HH_var_rs1000_temp <- c(
            ERA_HH_var_rs1000_temp , 
            # append
            ERA_raw_H[v,,,i] %>% 
              st_warp(dest = AOI_grid1000  , 
                      use_gdal = TRUE , method = "bilinear")
          ) 
        }
      } # end of i loop
      # reshape dimensions to 3D
      ERA_HH_var_rs1000_temp <- ERA_HH_var_rs1000_temp %>% 
        merge() %>% 
        st_set_dimensions(3 , values = st_get_dimension_values(ERA_raw_H , 3) , names = "date") %>% 
        setNames(v)
      # rename object to `{var}_{HH}_rs1000`
      assign(sprintf("%s_%sH_rs1000" , v , H) , ERA_HH_var_rs1000_temp)
      # clean
      rm(ERA_HH_var_rs1000_temp , i)
    } # end of v loop
  } # end of H loop
  rm(hours_ERA , vars_ERA , H , v)
}


# calculate ws, wd
ws_12H_rs1000 <- c(u10_12H_rs1000 , v10_12H_rs1000) %>% 
  transmute(ws = sqrt(u10^2 + v10^2))
ws_15H_rs1000 <- c(u10_15H_rs1000 , v10_15H_rs1000) %>% 
  transmute(ws = sqrt(u10^2 + v10^2))
wd_12H_rs1000 <- c(u10_12H_rs1000 , v10_12H_rs1000) %>% 
  transmute(ws = atan2(-u10 , -v10) )
wd_15H_rs1000 <- c(u10_15H_rs1000 , v10_15H_rs1000) %>% 
  transmute(ws = atan2(-u10 , -v10) )
# remove u10 and v10 objects to save space
rm(u10_12H_rs1000 , u10_15H_rs1000 , v10_12H_rs1000 , v10_15H_rs1000)

# nearest-neighbor-downscaling to 100*100m
# for each hour, each variable
{
  hours_ERA <- c(12,15)
  vars_ERA <- c("ws" , "wd" , "t2m" , "blh" , "sp" , "tcc" , "tp")
  for(H in hours_ERA){
    for(v in vars_ERA){
      # get stars object {var}_{HH}_rs1000
      var_HH_rs1000_temp <- get(sprintf("%s_%sH_rs1000" , v , H))
      # nearest-neighbor resampling
      var_HH_rs100_temp <- var_HH_rs1000_temp %>% 
        st_warp(AOI_grid100)
      # rename stars object to {var}_{HH}_rs100
      assign(sprintf("%s_%sH_rs100" , v , H) , var_HH_rs100_temp)
      # clean
      rm(var_HH_rs1000_temp , var_HH_rs100_temp) ; gc()
    }
  }
  # clean 
  rm(H,v)
}

# =====================================
# temporal interpolation + resample to target grid: NDVI
# =====================================
# convert stars into data.frame for temporal interpolation
NDVI_raw_df <- NDVI_raw %>% 
  as.data.frame() %>% 
  mutate(x = as.character(x) , 
         y = as.character(y))
# temporal interpolation (and convert back to stars)
NDVI_daily <- NDVI_raw_df %>% 
  # a data.frame of pixel centroid coordinates
  select(x,y) %>% 
  distinct() %>% 
  # 365 rows for each pixel 
  mutate(rows_each = 365) %>% 
  tidyr::uncount(rows_each) %>% # make (expand) 365 rows for each pixel (x-y combination)
  group_by(x,y) %>% 
  # full 365-day time-series for each pixel
  mutate(date = seq(as_date("2019-01-01") , as_date("2019-12-31") , "1 day")) %>% 
  ungroup() %>% 
  # join the NDVI values
  full_join(NDVI_raw_df , by = c("x" , "y" , "date")) %>% 
  arrange(x,y,date) %>% 
  group_by(x,y) %>% 
  # temporal interpolation
  mutate(NDVI_interpolated = zoo::na.fill(NDVI , "extend")) %>% 
  ungroup() %>% 
  # only dates within 2019-01-01 and 2019-12-31
  filter(date %within% interval("2019-01-01" , "2019-12-31")) %>% 
  # convert data.frame to stars
  st_as_stars(dim = c("x","y","date")) %>% 
  st_set_crs(st_crs(NDVI_raw))

# nearest-neighbor downscaling to 100*100m
NDVI_daily_rs100 <- NDVI_daily %>% 
  st_warp(AOI_grid100)


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
  st_set_bbox(st_bbox(AOI_grid100)) %>% 
  st_warp(AOI_grid100) 

# =====================================
# resample to target grid: traffic intensity
# =====================================
# nearest-neighbor resampling (100m to 100m, reprojection)
traffint_rs100 <- traffint_raw %>% 
  st_set_bbox(st_bbox(AOI_grid100)) %>% 
  st_warp(AOI_grid100)

# =====================================
# resample to target grid: major road density
# =====================================
# nearest-neighbor resampling (100m to 100m, reprojection)
mjdens_rs100 <- mjdens_raw %>% 
  st_set_bbox(st_bbox(AOI_grid100)) %>% 
  st_warp(AOI_grid100) %>% 
  setNames("mjroad_density")

# =====================================
# resample to target grid: all road density
# =====================================
# nearest-neighbor resampling (100m to 100m, reprojection)
rdens_rs100 <- rdens_raw %>% 
  st_set_bbox(st_bbox(AOI_grid100)) %>% 
  st_warp(AOI_grid100) %>% 
  setNames("allroad_density")

# =====================================
# resample to target grid: number of intersections
# =====================================
# nearest-neighbor resampling (100m to 100m, reprojection)
intersection_rs100 <- intersections_raw %>% 
  st_set_bbox(st_bbox(AOI_grid100)) %>% 
  st_warp(AOI_grid100) %>% 
  setNames("n_intersection")

# =====================================
# resample to target grid: distance to nearest major road
# =====================================
# nearest-neighbor resampling (100m to 100m, reprojection)
nearmjrd_rs100 <- nearmjrd_raw %>% 
  st_set_bbox(st_bbox(AOI_grid100)) %>% 
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
# each variable as a file in `1_data/processed/resampled_100m`
out_dirpath_resampled <- "1_data/processed/resampled_100m"
if(!dir.exists(out_dirpath_resampled)) dir.create(out_dirpath_resampled)

# =====================================
# spatialtemporal
# =====================================
out_dirpath_resampled_st <- paste0(out_dirpath_resampled , "/spatialtemporal")
if(!dir.exists(out_dirpath_resampled_st)) dir.create(out_dirpath_resampled_st)

# OMI
OMI_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_resampled_st , "/OMI_daily_100m.tif"))

# TROPOMI
TROPOMI_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_resampled_st , "/TROPOMI_daily_100m.tif"))

# meteorology
# {var}_{HH}_rs100
ls(pattern = "_\\d{2}H_rs100$") %>% 
  sapply(
    function(obj_name){
      obj_write <- get(obj_name)
      obj_write %>% 
        write_stars(dsn = paste0(out_dirpath_resampled_st , "/" , 
                                 str_replace(out_objects_meteo , "_rs100" , "_daily_100m") , 
                                 ".tif"))
    }
  )


# NDVI
NDVI_daily_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_resampled_st , "/NDVI_daily_100m.tif"))

# =====================================
# spatial
# =====================================
out_dirpath_resampled_sp <- paste0(out_dirpath_resampled , "/spatial")
if(!dir.exists(out_dirpath_resampled_sp)) dir.create(out_dirpath_resampled_sp)

# elevation
DEM_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_resampled_sp , "/elevation_100m.tif"))

# population
population_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_resampled_sp , "/population_100m.tif"))

# nighttime light
light_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_resampled_sp , "/nighttime-light_100m.tif"))

# emission
emission_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_resampled_sp , "/emissions-nox_100m.tif"))

# traffic intensity
traffint_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_resampled_sp , "/traffic-intensity_100m.tif"))

# major road density
mjdens_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_resampled_sp , "/major-road-density_100m.tif"))

# all road density
rdens_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_resampled_sp , "/all-road-density_100m.tif"))

# number of intersections
intersection_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_resampled_sp , "/n-intersections_100m.tif"))

# distance to nearest major road
nearmjrd_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_resampled_sp , "/dist-near-major-road_100m.tif"))

# land cover
landcover_rs100 %>% # original coding
  write_stars(dsn = paste0(out_dirpath_resampled_sp , "/landcover-code_100m.tif"))
landcover_GROUP_rs100 %>%  # binary land cover coding for 6 groups
  write_stars(dsn = paste0(out_dirpath_resampled_sp , "/landcover-group_binary_100m.tif"))

# NDVI
NDVI_sp_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_resampled_sp , "/NDVI_sp_100m.tif"))


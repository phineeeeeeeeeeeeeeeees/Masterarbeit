#####################################################################################################
# Masterarbeit
# Prepare training data (1/3): resampling
# (only the spatial-temporal predictors here in this script)
# 2021-06-04
#####################################################################################################
# coordinate system to work with: EPSG: 2056 (CH1903+/ LV95)

# =====================================
# required packages
# =====================================
library(stars) ; library(sf)
library(dplyr) ; library(tidyr)
library(data.table) ; library(dtplyr)
library(ggplot2)
library(lubridate) ; library(stringr)

# =====================================
# set output directory
# =====================================
out_dirpath_cleaned <- "1_data/processed/cleaned"
out_dirpath_cleaned_st <- paste0(out_dirpath_cleaned , "/spatialtemporal")
if(!dir.exists(out_dirpath_cleaned)) dir.create(out_dirpath_cleaned)
if(!dir.exists(out_dirpath_cleaned_st)) dir.create(out_dirpath_cleaned_st)


# =====================================
# load area of interest boundary
# =====================================
# AOI: national boundary + 5km buffer
AOI <- st_read("1_data/raw/Switzerland_shapefile/CHE_adm0.shp") %>% 
  select(ID_0) %>% # drop unnecessary columns
  st_transform(st_crs(2056)) %>% 
  st_buffer(10000)

# =====================================
# template target grids 
# =====================================
# 1*1km
AOI_grid1000 <- AOI %>% 
  st_rasterize(crs = st_crs(2056) , dx = 1000 , dy = 1000) %>% 
  mutate(ID_0 = as.integer(ID_0)) # integer consumes less space to store data

# =====================================
# make the spatial look-up table (link the 1*1km pixels to 100*100m pixels)
# =====================================
if(!file.exists(paste0(out_dirpath_cleaned_st , "/AOI_spatial-lookup-table_1000to100m.csv"))){
  # 100*100m template grid
  AOI_grid100 <- AOI %>% 
    st_rasterize(crs = st_crs(2056) , dx = 100 , dy = 100) %>% 
    mutate(ID_0 = as.integer(ID_0))
  # assign unique IDs to each 1*1km pixel
  AOI_grid1000_ID <- AOI_grid1000 %>% 
    transmute(pixelID = 1:(dim(.)[1]*dim(.)[2]))
  # make the spatial look-up table
  AOI_lookup_table_1000 <- AOI_grid1000_ID %>% 
    # resample this pixel ID to 100m 
    st_warp(AOI_grid100) %>% # so I know which 100m cell each 1km cell locates
    as.data.frame() %>% 
    arrange(pixelID) %>% 
    # include the 1km-resolution x and y coordinates
    left_join(as.data.frame(AOI_grid1000_ID) , by = "pixelID" , suffix = c("_100" , "_1000")) %>% 
    select(pixelID , starts_with("x") , starts_with("y")) 
  # AOI_lookup_table_250: 8,361,297 * 5
  # save the look-up table (589 MB)
  data.table::fwrite(AOI_lookup_table_1000 , 
                     paste0(out_dirpath_cleaned_st , "/AOI_spatial-lookup-table_1000to100m.csv") , 
                     row.names = FALSE)
  # clean environment
  rm(AOI_grid1000_ID , AOI_lookup_table_1000)
}

# //////////////////////////////////////////////////////////////////////////
# OMI
# //////////////////////////////////////////////////////////////////////////
# =====================================
# import
# =====================================
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

# =====================================
# resample to target grid
# =====================================
# bilinear-interpolation to 1*1km
# st_warp(use_gdal = TRUE , method = "bilinear")
# resample each day separately (to have the same 2D dimension)
{
  # resample each day to the target grids
  pb <- txtProgressBar(min = 1 , max = dim(OMI_raw)[3] , style = 3)
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

# =====================================
# export
# =====================================
OMI_rs1000 %>% 
  write_stars(dsn = paste0(out_dirpath_cleaned_st , "/OMI_daily_1000m.tif")) # 117MB

# clean environment
rm(list = ls(pattern = "^OMI"))
gc() ; gc()


# //////////////////////////////////////////////////////////////////////////
# TROPOMI
# //////////////////////////////////////////////////////////////////////////
# =====================================
# import
# =====================================
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

# =====================================
# resample to target grid
# =====================================
# bilinear-interpolation to 1*1km
# st_warp(use_gdal = TRUE , method = "bilinear")
# resample each day separately (to have the same 2D dimension)
{
  # resample each day to the target grids
  pb <- txtProgressBar(min = 1 , max = dim(TROPOMI_raw)[3] , style = 3)
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

# =====================================
# export
# =====================================
TROPOMI_rs1000 %>% 
  write_stars(dsn = paste0(out_dirpath_cleaned_st , "/TROPOMI_daily_1000m.tif")) # 117MB

# clean environment
rm(list = ls(pattern = "^TROPOMI"))
gc() ; gc()


# //////////////////////////////////////////////////////////////////////////
# meteorological variables
# //////////////////////////////////////////////////////////////////////////
# =====================================
# import
# =====================================
ERA_raw <- read_stars("1_data/raw/ECMWF-ERA5/ERA5-meteorology.nc") %>% 
  st_set_crs(st_crs(4326)) # long-lat coordinates

# =====================================
# resample to target grid
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
    rm(ERA_raw_H)
  } # end of H loop
  rm(hours_ERA , vars_ERA , H , v)
}

# =====================================
# calculate ws, wd
# =====================================
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

# =====================================
# export
# =====================================
# for each hour, each variable
{
  hours_ERA <- c(12,15)
  vars_ERA <- c("ws" , "wd" , "t2m" , "blh" , "sp" , "tcc" , "tp")
  for(H in hours_ERA){
    for(v in vars_ERA){
      # get stars object {var}_{HH}_rs1000
      var_HH_rs1000_temp <- get(sprintf("%s_%sH_rs1000" , v , H))
      # export
      var_HH_rs1000_temp %>% 
        write_stars(paste0(out_dirpath_cleaned_st , "/" , v , "_" , H , "H_daily_1000m.tif"))
      # clean environment
      rm(list = c("var_HH_rs1000_temp" , sprintf("%s_%sH_rs1000" , v , H))) ; gc()
    }
  }
  # clean environment
  rm(H,v)
}

# clean environment
rm(list = ls(pattern = "ERA"))
gc() ; gc()


# //////////////////////////////////////////////////////////////////////////
# 16-day NDVI
# //////////////////////////////////////////////////////////////////////////
# =====================================
# import
# =====================================
in_filepath_NDVI <- list.files("1_data/raw/MODIS-vegetation/NDVI-GTiff" , 
                               pattern = "_AOI.tif$" , full.names = TRUE)
# 250*250m, 4GB
NDVI_raw <- read_stars(in_filepath_NDVI , 
                       along = list(
                         date = in_filepath_NDVI %>% 
                           basename() %>% 
                           str_extract("\\d{8}") %>% 
                           as_date()
                       ) , proxy = FALSE) %>% 
  setNames("NDVI")
# crop the extent of the datacube to AOI (too much blank pixels): 294MB
NDVI_raw <- NDVI_raw[st_transform(AOI,st_crs(NDVI_raw))]

# =====================================
# temporal interpolation
# =====================================
NDVI_raw_df <- NDVI_raw %>% 
  as.data.frame() %>% 
  # some pixels are outside of AOI (between AOI boundary and bounding box)
  # exclude these rows
  left_join(
    y = AOI_grid1000 %>% 
      st_warp(dest = NDVI_raw[,,,1] %>% split()) %>% 
      transmute(AOI_mask = !is.na(ID_0)) %>% 
      as.data.frame() , 
    by = c("x" , "y")
  ) %>% 
  # mask out the rows that are outside AOI
  filter(AOI_mask) %>% 
  select(x,y,date,NDVI)
# 708MB 

NDVI_daily <- NDVI_raw_df %>%
  # a data.frame of pixel centroid coordinates
  select(x,y) %>%
  distinct() %>% 
  # 365 rows for each pixel
  slice(rep(1:n(), each = 365)) %>% 
  mutate(date = rep(seq(as_date("2019-01-01") , as_date("2019-12-31") , "1 day") , length.out = n())) %>% 
  # # join the NDVI values
  left_join(NDVI_raw_df , by = c("x" , "y" , "date")) %>%
  arrange(x,y,date) %>%
  group_by(x,y) %>%
  # temporal interpolation  # need at least two non-NA values to interpolate
  mutate(NDVI_interpolated = ifelse(sum(!is.na(NDVI))>1 , zoo::na.fill(NDVI , "extend") , NA) ) %>%
  ungroup() %>%
  # only dates within 2019-01-01 and 2019-12-31
  filter(date %within% interval("2019-01-01" , "2019-12-31")) %>%
  # convert data.frame to stars
  st_as_stars(dim = c("x","y","date") , coords = 1:3) %>%
  st_set_crs(st_crs(NDVI_raw)) %>% 
  select(NDVI_interpolated)

rm(NDVI_raw , NDVI_raw_df)
gc(reset = TRUE)

NDVI_daily %>% 
  write_stars(dsn = paste0(out_dirpath_cleaned_st , "/NDVI_daily_EPSG9001.tif")) # 2.1 GB

# =====================================
# resample (re-project) to EPSG: 2056 (CH1903+/ LV95)
# and use a spatial look-up table to link to 100*100m
# =====================================
# template target grids 
# 250*250m
AOI_grid250 <- AOI %>% 
  st_rasterize(crs = st_crs(2056) , dx = 250 , dy = 250) %>% 
  mutate(ID_0 = as.integer(ID_0)) # integer consumes less space to store data

# make the spatial look-up table (link the 250*250m pixels to 100*100m pixels)
if(!file.exists(paste0(out_dirpath_cleaned_st , "/AOI_spatial-lookup-table_250to100m.csv"))){
  # 100*100m template grid
  AOI_grid100 <- AOI %>% 
    st_rasterize(crs = st_crs(2056) , dx = 100 , dy = 100) %>% 
    mutate(ID_0 = as.integer(ID_0))
  # assign unique IDs to each 250*250m pixel
  AOI_grid250_ID <- AOI_grid250 %>% 
    transmute(pixelID = 1:(dim(.)[1]*dim(.)[2]))
  # make the spatial look-up table
  AOI_lookup_table_250 <- AOI_grid250_ID %>% 
    # resample this pixel ID to 100m 
    st_warp(AOI_grid100) %>% # so I know which 100m cell each 1km cell locates
    as.data.frame() %>% 
    arrange(pixelID) %>% 
    # include the 1km-resolution x and y coordinates
    left_join(as.data.frame(AOI_grid250_ID) , by = "pixelID" , suffix = c("_100" , "_250")) %>% 
    select(pixelID , starts_with("x") , starts_with("y")) 
  # AOI_lookup_table_250: 8,361,297 * 5
  # save the look-up table (600 MB)
  data.table::fwrite(AOI_lookup_table_250 , 
                     paste0(out_dirpath_cleaned_st , "/AOI_spatial-lookup-table_250to100m.csv") , 
                     row.names = FALSE)
  # clean environment
  rm(AOI_grid250_ID , AOI_lookup_table_250)
}

# resample (re-project) NDVI_daily to the target grid (projection system)
NDVI_daily_rs250 <- NDVI_daily %>% 
  st_warp(dest = AOI_grid250)

# =====================================
# export
# =====================================
NDVI_daily_rs250 %>% 
  write_stars(dsn = paste0(out_dirpath_cleaned_st , "/NDVI_daily_250m.tif")) # 1.8GB

# clean environment
rm(list = ls(pattern = "^NDVI"))
gc() ; gc()


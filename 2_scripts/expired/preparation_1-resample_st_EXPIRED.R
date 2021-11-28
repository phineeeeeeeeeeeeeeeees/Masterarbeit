#####################################################################################################
# Masterarbeit
# Prepare training data (1/3): resampling
# (only spatial-temporal predictors here in this script)
# 2021-05-31
#####################################################################################################
# coordinate system to work with: EPSG: 2056 (CH1903+/ LV95)

# =====================================
# required packages
# =====================================
library(readr)
library(stars) ; library(sf) ; library(raster)
library(dplyr) ; library(tidyr) ; library(data.table)
library(ggplot2)
library(lubridate) ; library(stringr)

# =====================================
# load area of interest boundary
# =====================================
# AOI (national boundary + 50km buffer)
AOI <- st_read("1_data/raw/Switzerland_shapefile/AOI_4326.shp") %>% 
  st_transform(st_crs(2056))

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

cat("\nOMI_rs1000 resampled ")

# =====================================
# nearest-neighbor-downscaling to 100*100m
# =====================================
OMI_rs100 <- OMI_rs1000 %>% 
  st_warp(AOI_grid100)

cat("\nOMI_rs100 resampled")

rm(OMI_raw , OMI_rs1000)


# =====================================
# export
# =====================================
# each variable as a file in `1_data/processed/resampled_100m`
out_dirpath_resampled <- "1_data/processed/resampled_100m"
if(!dir.exists(out_dirpath_resampled)) dir.create(out_dirpath_resampled)
out_dirpath_resampled_st <- paste0(out_dirpath_resampled , "/spatialtemporal")
if(!dir.exists(out_dirpath_resampled_st)) dir.create(out_dirpath_resampled_st)

OMI_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_resampled_st , "/OMI_daily_100m.tif"))


# =====================================
# clean environment
# =====================================
rm(OMI_rs100)
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

cat("\nTROPOMI_rs1000 resampled")

# =====================================
# nearest-neighbor-downscaling to 100*100m
# =====================================
TROPOMI_rs100 <- TROPOMI_rs1000 %>% 
  st_warp(AOI_grid100)

cat("\nTROPOMI_rs100 resampled")

rm(TROPOMI_raw , TROPOMI_1000)


# =====================================
# export
# =====================================
TROPOMI_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_resampled_st , "/TROPOMI_daily_100m.tif"))

# =====================================
# clean environment
# =====================================
clean(TROPOMI_rs100)
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
  } # end of H loop
  rm(hours_ERA , vars_ERA , H , v)
}

cat("\n{var}_{HH}_rs1000 resampled")

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
cat("\nws/wd calculated")
# remove u10 and v10 objects to save space
rm(u10_12H_rs1000 , u10_15H_rs1000 , v10_12H_rs1000 , v10_15H_rs1000)


# =====================================
# nearest-neighbor-downscaling to 100*100m
# =====================================
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
      # clean
      rm(list =  sprintf("%s_%sH_rs1000" , v , H))
      # =====================================
      # export
      # =====================================
      var_HH_rs100_temp %>% 
        write_stars(paste0(out_dirpath_resampled_st , "/" , v , "_" , H , "H_daily_100m.tif"))
      # clean
      rm(var_HH_rs1000_temp , var_HH_rs100_temp) ; gc()
    }
  }
  # clean 
  rm(H,v)
}

cat("\n{var}_{HH}_rs100 resampled and exported")



# =====================================
# clean environment
# =====================================
rm(ERA_raw , list = ls(pattern = "_rs100"))
gc() ; gc()


# ////////////////////////////////////////////////////////////////////////// 
# NDVI
# //////////////////////////////////////////////////////////////////////////
# =====================================
# import
# =====================================
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


# =====================================
# temporal interpolation + resample to target grid: NDVI
# =====================================
NDVI_raw_df <- NDVI_raw %>% 
  as.data.table() %>% 
  .[ , x := as.character(x)] %>% 
  .[ , y := as.character(y)]
# temporal interpolation (and convert back to stars)
NDVI_daily <- NDVI_raw_df %>% 
  # a data.frame of pixel centroid coordinates
  .[,.(x,y)] %>% 
  unique(.) %>% 
  # 365 rows for each pixel 
  .[, rows_each:= c(365) ] %>% 
  tidyr::uncount(rows_each) %>% # make (expand) 365 rows for each pixel (x-y combination)
  # full 365-day time-series for each pixel
  .[ , date:=seq(as_date("2019-01-01"),as_date("2019-12-31"),"1 day") , by=.(x,y) ] %>% 
  # join the NDVI values
  merge(. , NDVI_raw_df , all = TRUE , by = c("x" , "y" , "date")) %>% 
  setorder(. , x,y,date) %>%  
  # temporal interpolation
  .[ , NDVI_interpolated := zoo::na.fill(NDVI,"extend")] %>%  
  # only dates within 2019-01-01 and 2019-12-31
  .[date %within% interval("2019-01-01" , "2019-12-31") , ] %>%  
  .[ , .(x,y,date,NDVI_interpolated)] #%>% 
  # convert data.frame to stars
  st_as_stars(dim = c("x","y","date") , coords = 1:3) %>% 
  st_set_crs(st_crs(NDVI_raw)) %>% 
  select(NDVI_interpolated)
# # convert stars into data.frame for temporal interpolation
# NDVI_raw_df <- NDVI_raw %>% 
#   as.data.table() %>% 
#   mutate(x = as.character(x) , 
#          y = as.character(y))
# # temporal interpolation (and convert back to stars)
# NDVI_daily <- NDVI_raw_df %>% 
#   # a data.frame of pixel centroid coordinates
#   .[,.(x,y)] %>%  #select(x,y) %>% 
#   distinct() %>% 
#   # 365 rows for each pixel 
#   mutate(rows_each = 365) %>% 
#   tidyr::uncount(rows_each) %>% # make (expand) 365 rows for each pixel (x-y combination)
#   group_by(x,y) %>% 
#   # full 365-day time-series for each pixel
#   mutate(date = seq(as_date("2019-01-01") , as_date("2019-12-31") , "1 day")) %>% 
#   ungroup() %>% 
#   # join the NDVI values
#   full_join(NDVI_raw_df , by = c("x" , "y" , "date")) %>% 
#   arrange(x,y,date) %>% 
#   group_by(x,y) %>% 
#   # temporal interpolation
#   mutate(NDVI_interpolated = zoo::na.fill(NDVI , "extend")) %>% 
#   ungroup() %>% 
#   # only dates within 2019-01-01 and 2019-12-31
#   filter(date %within% interval("2019-01-01" , "2019-12-31")) %>% 
#   # convert data.frame to stars
#   st_as_stars(dim = c("x","y","date")) %>% 
#   st_set_crs(st_crs(NDVI_raw))

cat("\nNDVI interpolated to daily")


# =====================================
# nearest-neighbor-downscaling to 100*100m
# =====================================
NDVI_daily_rs100 <- NDVI_daily %>% 
  st_warp(AOI_grid100)

cat("\nNDVI_daily_rs100 resampled")

# =====================================
# export
# =====================================
NDVI_daily_rs100 %>% 
  write_stars(dsn = paste0(out_dirpath_resampled_st , "/NDVI_daily_100m.tif"))

# =====================================
# clean environment
# =====================================
rm(list = ls(pattern = "NDVI"))
gc() ; gc()



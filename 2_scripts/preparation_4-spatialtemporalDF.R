#####################################################################################################
# Masterarbeit
# Prepare data : reshape the spatial-temporal predictor variables into data.frames for later modeling prediction
# 2021-06-16
#####################################################################################################
# coordinate system to work with: EPSG: 2056 (CH1903+/ LV95)

# =====================================
# required packages
# =====================================
library(readr)
library(sf) ; library(stars)
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(ggsci) ; library(ggthemes)
library(lubridate) ; library(stringr)

# =====================================
# load spatialtemporal predictors
# =====================================
in_filepath_st <- list.files("1_data/processed/cleaned/spatialtemporal" , 
                             full.names = TRUE , pattern = ".tif$")

# OMI ----------------------------------------------
# annual
OMI_annual <- read_stars(grep("/OMI_annual" , in_filepath_st , value = TRUE)) %>% 
  setNames("OMI_NO2")
# monthly
OMI_monthly <- read_stars(grep("/OMI_monthly" , in_filepath_st , value = TRUE)) %>% 
  setNames("OMI_NO2") %>% 
  st_set_dimensions(3 , values = 1:12 , names = "month")
# daily
OMI_daily <- read_stars(grep("/OMI_daily" , in_filepath_st , value = TRUE)) %>% 
  setNames("OMI_NO2") %>% 
  st_set_dimensions(3 , 
                    values = st_get_dimension_values(. , 3) %>% as_date(origin = "2018-12-31") , 
                    names = "date")

# TROPOMI ----------------------------------------------
# annual
TROPOMI_annual <- read_stars(grep("TROPOMI_annual" , in_filepath_st , value = TRUE)) %>% 
  setNames("TROPOMI_NO2")
# monthly
TROPOMI_monthly <- read_stars(grep("TROPOMI_monthly" , in_filepath_st , value = TRUE)) %>% 
  setNames("TROPOMI_NO2") %>% 
  st_set_dimensions(3 , values = 1:12 , names = "month")
# daily
TROPOMI_daily <- read_stars(grep("TROPOMI_daily" , in_filepath_st , value = TRUE)) %>% 
  setNames("TROPOMI_NO2") %>% 
  st_set_dimensions(3 , 
                    values = st_get_dimension_values(. , 3) %>% as_date(origin = "2018-12-31") , 
                    names = "date")

# meteorological variables ----------------------------------------------
# a function for repetitive process
load_meteo <- function(VarName_file , VarName_env){ 
  # VarName_file: the variable name in the file path
  # VarName_env: the variable name you want to use in the environment
  stars_temp_annual <- read_stars(grep(sprintf("%s_\\d{2}H_annual" , VarName_file) , in_filepath_st , value = TRUE)) %>% 
    setNames(sprintf("%s_%s" , VarName_env , str_extract(names(.) , "\\d{2}H")))
  # monthly
  stars_temp_monthly <- read_stars(grep(sprintf("%s_\\d{2}H_monthly" , VarName_file) , in_filepath_st , value = TRUE)) %>% 
    setNames(sprintf("%s_%s" , VarName_env , str_extract(names(.) , "\\d{2}H"))) %>% 
    st_set_dimensions(3 , values = 1:12 , names = "month")
  # daily
  stars_temp_daily <- read_stars(grep(sprintf("%s_\\d{2}H_daily" , VarName_file) , in_filepath_st , value = TRUE)) %>% 
    setNames(sprintf("%s_%s" , VarName_env , str_extract(names(.) , "\\d{2}H"))) %>% 
    st_set_dimensions(3 , 
                      values = st_get_dimension_values(. , 3) %>% as_date(origin = "2018-12-31") , 
                      names = "date")
  # make a copy and rename
  assign(sprintf("%s_annual" , VarName_env) , stars_temp_annual , envir = parent.frame())
  assign(sprintf("%s_monthly" , VarName_env) , stars_temp_monthly , envir = parent.frame())
  assign(sprintf("%s_daily" , VarName_env) , stars_temp_daily , envir = parent.frame())
  # clean
  rm(list = ls(pattern = "stars_temp_"))
}

# temperature
load_meteo(VarName_file = "t2m" , VarName_env = "temperature")

# boundary layer height
load_meteo(VarName_file = "blh" , VarName_env = "blh")

# surface pressure
load_meteo(VarName_file = "sp" , VarName_env = "pressure")

# total precipitation
load_meteo(VarName_file = "tp" , VarName_env = "precipitation")

# wind speed
load_meteo(VarName_file = "ws" , VarName_env = "ws")

# wind direction
load_meteo(VarName_file = "wd" , VarName_env = "wd")

# total cloud cover
load_meteo(VarName_file = "tcc" , VarName_env = "tcc")


# NDVI ----------------------------------------------
# annual
NDVI_annual <- read_stars(grep("NDVI_annual" , in_filepath_st , value = TRUE)) %>% 
  setNames("NDVI")
# monthly
NDVI_monthly <- read_stars(grep("NDVI_monthly" , in_filepath_st , value = TRUE)) %>% 
  setNames("NDVI") %>% 
  st_set_dimensions(3 , values = 1:12 , names = "month")
# daily
NDVI_daily <- read_stars(grep("NDVI_daily_250m" , in_filepath_st , value = TRUE) , proxy = FALSE) %>% 
  setNames("NDVI") %>% 
  st_set_dimensions(3 , 
                    values = st_get_dimension_values(. , 3) %>% as_date(origin = "2018-12-31") , 
                    names = "date")


# =====================================
# spatial-temporal (annual) predictors as a data.frame
# with scaling (refer to preparation_3-extract.R)
# =====================================
st_annual_df <- 
  # OMI
  OMI_annual %>% 
  mutate(across(everything() , scale)) %>% 
  as.data.frame() %>% 
  # TROPOMI
  left_join(
    TROPOMI_annual %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() , 
    by = c("x" , "y")
  ) %>% 
  # temperature
  left_join(
    temperature_annual %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() , 
    by = c("x" , "y")
  ) %>% 
  # boundary layer height
  left_join(
    blh_annual %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() ,
    by = c("x" , "y")
  ) %>% 
  # surface pressure
  left_join(
    pressure_annual %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() , 
    by = c("x" , "y")
  ) %>% 
  # total precipitation
  left_join(
    precipitation_annual %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() , 
    by = c("x" , "y")
  ) %>% 
  # wind speed
  left_join(
    ws_annual %>% 
      mutate(across(everything() , scale)) %>%
      as.data.frame() , 
    by = c("x" , "y")
  ) %>% 
  # wind direction
  left_join(
    wd_annual %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() , 
    by = c("x" , "y")
  ) %>% 
  # total cloud cover
  left_join(
    tcc_annual %>% 
      as.data.frame() , 
    by = c("x" , "y")
  ) 

NDVI_annual_df <- NDVI_annual %>% 
  mutate(across(everything() , scale)) %>%
  as.data.frame()

# =====================================
# spatial-temporal (monthly) predictors as a data.frame
# with scaling (refer to preparation_3-extract.R)
# =====================================
st_monthly_df <- 
  # OMI
  OMI_monthly %>% 
  mutate(across(everything() , scale)) %>% 
  as.data.frame() %>% 
  # TROPOMI
  left_join(
    TROPOMI_monthly %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() , 
    by = c("x" , "y" , "month")
  ) %>% 
  # temperature
  left_join(
    temperature_monthly %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() , 
    by = c("x" , "y" , "month")
  ) %>% 
  # boundary layer height
  left_join(
    blh_monthly %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() ,
    by = c("x" , "y" , "month")
  ) %>% 
  # surface pressure
  left_join(
    pressure_monthly %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() , 
    by = c("x" , "y" , "month")
  ) %>% 
  # total precipitation
  left_join(
    precipitation_monthly %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() , 
    by = c("x" , "y" , "month")
  ) %>% 
  # wind speed
  left_join(
    ws_monthly %>% 
      mutate(across(everything() , scale)) %>%
      as.data.frame() , 
    by = c("x" , "y" , "month")
  ) %>% 
  # wind direction
  left_join(
    wd_monthly %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() , 
    by = c("x" , "y" , "month")
  ) %>% 
  # total cloud cover
  left_join(
    tcc_monthly %>% 
      as.data.frame() , 
    by = c("x" , "y" , "month")
  ) 
  
NDVI_monthly_df <- NDVI_monthly %>% 
  mutate(across(everything() , scale)) %>%
  as.data.frame()

# =====================================
# spatial-temporal (daily) predictors as a data.frame
# with scaling (refer to preparation_3-extract.R)
# =====================================
st_daily_df <- 
  # OMI
  OMI_daily %>% 
  mutate(across(everything() , scale)) %>% 
  as.data.frame() %>% 
  # TROPOMI
  left_join(
    TROPOMI_daily %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() , 
    by = c("x" , "y" , "date")
  ) %>% 
  # temperature
  left_join(
    temperature_daily %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() , 
    by = c("x" , "y" , "date")
  ) %>% 
  # boundary layer height
  left_join(
    blh_daily %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() ,
    by = c("x" , "y" , "date")
  ) %>% 
  # surface pressure
  left_join(
    pressure_daily %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() , 
    by = c("x" , "y" , "date")
  ) %>% 
  # total precipitation
  left_join(
    precipitation_daily %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() , 
    by = c("x" , "y" , "date")
  ) %>% 
  # wind speed
  left_join(
    ws_daily %>% 
      mutate(across(everything() , scale)) %>%
      as.data.frame() , 
    by = c("x" , "y" , "date")
  ) %>% 
  # wind direction
  left_join(
    wd_daily %>% 
      mutate(across(everything() , scale)) %>% 
      as.data.frame() , 
    by = c("x" , "y" , "date")
  ) %>% 
  # total cloud cover
  left_join(
    tcc_daily %>% 
      as.data.frame() , 
    by = c("x" , "y" , "date")
  ) 

NDVI_daily_df <- NDVI_daily %>% 
  mutate(across(everything() , scale)) %>%
  as.data.frame()


# =====================================
# export
# =====================================
out_dirpath <- "1_data/processed/cleaned/data-frame"
if(!dir.exists(out_dirpath)) dir.create(out_dirpath)

filepath_lookuptables <- list.files("1_data/processed/cleaned/spatialtemporal" , pattern = "spatial-lookup-table" , full.names = TRUE)
file.copy(
  filepath_lookuptables , 
  paste(out_dirpath , basename(filepath_lookuptables) , sep = "/" )
)

st_annual_df %>% 
  write_csv(file = paste0(out_dirpath , "/spatialtemporal_annual_df.csv"))
NDVI_annual_df %>% 
  write_csv(file = paste0(out_dirpath , "/spatialtemporal_NDVI_annual_df.csv"))
st_monthly_df %>% 
  write_csv(file = paste0(out_dirpath , "/spatialtemporal_monthly_df.csv"))
NDVI_monthly_df %>% 
  write_csv(file = paste0(out_dirpath , "/spatialtemporal_NDVI_monthly_df.csv"))
st_daily_df %>% 
  write_csv(file = paste0(out_dirpath , "/spatialtemporal_daily_df.csv"))
NDVI_daily_df %>% 
  write_csv(file = paste0(out_dirpath , "/spatialtemporal_NDVI_daily_df.csv"))

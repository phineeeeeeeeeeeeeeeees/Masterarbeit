#####################################################################################################
# Masterarbeit
# Prepare training data (3/3) : extracting point values
# 2021-05-27
#####################################################################################################
# coordinate system to work with: EPSG: 2056 (CH1903+/ LV95)

# =====================================
# required packages
# =====================================
library(readr)
library(stars) ; library(sf) 
library(dplyr) ; library(tidyr) 
library(ggplot2) 
library(lubridate) ; library(stringr)


# //////////////////////////////////////////////////////////////////////////
# import data
# //////////////////////////////////////////////////////////////////////////

# =====================================
# load Switzerland
# =====================================
# adm1
Swiss_adm1 <- st_read("1_data/raw/Switzerland_shapefile/CHE_adm1.shp") %>% 
  st_transform(st_crs(2056))

# =====================================
# load NO2 monitoring data
# =====================================
# NO2 monitoring data ----------------------------------------------
in_filepath_NO2_df <- "1_data/raw/NO2-monitoring/NO2_2019_idb_daily.csv"
NO2_df_raw <- read_delim(in_filepath_NO2_df , 
                         skip = 4 , delim = ";" , col_names = FALSE , 
                         locale = locale(encoding = "ISO-8859-1")) %>% 
  # set column names
  setNames(
    read_delim(in_filepath_NO2_df, 
               delim = ";", n_max = 1 , col_names = FALSE , locale = locale(encoding = "ISO-8859-1"), 
               trim_ws = TRUE) %>% unlist
  ) %>% 
  # the last column is NA --> remove
  select(-ncol(.)) %>% 
  # rename the date column
  rename(date = `MEZ, Anfangszeit`) %>% 
  mutate(date = as_date(date))

# metadata of the monitoring sites (with coordinates) ----------------------------------------------
NO2_sites <- read_csv("1_data/raw/NO2-monitoring/metadaten_idbluft_supplemented.csv") %>% 
  # clean column names
  setNames(str_replace_all(colnames(.) , " " , "_")) %>% 
  setNames(str_replace_all(colnames(.) , "-" , "_")) %>% 
  # convert to spatial points
  st_as_sf(coords = c("x" , "y") , crs = st_crs(2056))


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
# load spatial predictors
# =====================================
in_filepath_sp <- list.files("1_data/processed/cleaned/spatial" , 
                             full.names = TRUE , pattern = ".tif$")

# elevation ----------------------------------------------
# 3D: x, y, moving-window radius
in_filepath_sp_elevation <- grep("elevation" , in_filepath_sp , value = TRUE)
elevation <- read_stars(
  in_filepath_sp_elevation , 
  along = list(
    radius = str_extract(in_filepath_sp_elevation , "radius\\d+") %>% 
      str_extract("\\d+") %>% 
      ifelse(is.na(.) , "0" , .) %>% paste0("R_" , .)
  ) , 
  proxy = FALSE
) %>% 
  setNames("elevation")
# 535MB

# emission ----------------------------------------------
# 4D: x, y, sources, moving-window radius
in_filepath_sp_emission <- grep("emission" , in_filepath_sp , value = TRUE)
emission <- read_stars(
  in_filepath_sp_emission , 
  along = list(
    radius = str_extract(in_filepath_sp_emission , "radius\\d+") %>% 
      str_extract("\\d+") %>% 
      ifelse(is.na(.) , "0" , .) %>% paste0("R_" , .)
  ) , 
  proxy = FALSE
) %>% 
  st_set_dimensions(3 , names = "source") %>% 
  setNames("emission")
# 2.6GB

# landcover ----------------------------------------------
# 4D: x, y, lc_group, moving-window radius
in_filepath_sp_landcover <- grep("landcover-group_binary" , in_filepath_sp , value = TRUE)
landcover <- read_stars(
  in_filepath_sp_landcover , 
  along = list(
    radius = str_extract(in_filepath_sp_landcover , "radius\\d+") %>% 
      str_extract("\\d+") %>% 
      ifelse(is.na(.) , "0" , .) %>% paste0("R_" , .)
  ) , 
  proxy = FALSE
) %>% 
  st_set_dimensions(3 , names = "lc_group") %>% 
  setNames("landcover")
# 3.2GB

# nighttime light ----------------------------------------------
# 3D: x, y, moving-window radius
in_filepath_sp_light <- grep("nighttime-light" , in_filepath_sp , value = TRUE)
light <- read_stars(
  in_filepath_sp_light , 
  along = list(
    radius = str_extract(in_filepath_sp_light , "radius\\d+") %>% 
      str_extract("\\d+") %>% 
      ifelse(is.na(.) , "0" , .) %>% paste0("R_" , .)
  ) , 
  proxy = FALSE
) %>% 
  setNames("nighttime_light")
# 535MB

# population ----------------------------------------------
# 3D: x, y, moving-window radius
in_filepath_sp_population <- grep("population" , in_filepath_sp , value = TRUE)
population <- read_stars(
  in_filepath_sp_population , 
  along = list(
    radius = str_extract(in_filepath_sp_population , "radius\\d+") %>% 
      str_extract("\\d+") %>% 
      ifelse(is.na(.) , "0" , .) %>% paste0("R_" , .)
  ) , 
  proxy = FALSE
) %>% 
  setNames("population")
# 535MB

# traffic intensity ----------------------------------------------
# 3D: x, y, moving-window radius
in_filepath_sp_traffint <- grep("traffic-intensity" , in_filepath_sp , value = TRUE)
traffint <- read_stars(
  in_filepath_sp_traffint , 
  along = list(
    radius = str_extract(in_filepath_sp_traffint , "radius\\d+") %>% 
      str_extract("\\d+") %>% 
      ifelse(is.na(.) , "0" , .) %>% paste0("R_" , .)
  ) , 
  proxy = FALSE
) %>% 
  setNames("traffint")
# 535MB

# number of intersections  ----------------------------------------------
# 3D: x, y, moving-window radius
in_filepath_sp_intersection <- grep("n-intersections" , in_filepath_sp , value = TRUE)
intersection <- read_stars(
  in_filepath_sp_intersection , 
  along = list(
    radius = str_extract(in_filepath_sp_intersection , "radius\\d+") %>% 
      str_extract("\\d+") %>% 
      ifelse(is.na(.) , "0" , .) %>% paste0("R_" , .)
  ) , 
  proxy = FALSE
) %>% 
  setNames("intersection")
# 535MB

# major road density ----------------------------------------------
# 3D: x, y, moving-window radius
in_filepath_sp_mjdens <- grep("major-road-density" , in_filepath_sp , value = TRUE)
mjdens <- read_stars(
  in_filepath_sp_mjdens , 
  along = list(
    radius = str_extract(in_filepath_sp_mjdens , "radius\\d+") %>% 
      str_extract("\\d+") %>% 
      ifelse(is.na(.) , "0" , .) %>% paste0("R_" , .)
  ) , 
  proxy = FALSE
) %>% 
  setNames("mjdens")
# 535MB

# all road density ----------------------------------------------
# 3D: x, y, moving-window radius
in_filepath_sp_rdens <- grep("all-road-density" , in_filepath_sp , value = TRUE)
rdens <- read_stars(
  in_filepath_sp_rdens , 
  along = list(
    radius = str_extract(in_filepath_sp_rdens , "radius\\d+") %>% 
      str_extract("\\d+") %>% 
      ifelse(is.na(.) , "0" , .) %>% paste0("R_" , .)
  ) , 
  proxy = FALSE
) %>% 
  setNames("rdens")
# 535MB

# distance to nearest major road ----------------------------------------------
# 2D: x, y
in_filepath_sp_nearmjrd <- grep("dist-near-major-road" , in_filepath_sp , value = TRUE)
nearmjrd <- read_stars(in_filepath_sp_nearmjrd) %>% 
  setNames("nearmjrd")
# 66MB

# NDVI ----------------------------------------------
# 3D: x, y, moving-window radius
in_filepath_sp_NDVI <- grep("NDVI_sp" , in_filepath_sp , value = TRUE)
NDVI_sp <- read_stars(in_filepath_sp_NDVI) %>% 
  setNames("NDVI_sp")
# 66MB



# //////////////////////////////////////////////////////////////////////////
# scale and extract pixel values of measurement sites
# //////////////////////////////////////////////////////////////////////////
# a function for repetitive extraction
extract_NO2_sites <- function(stars_obj){
  stars_obj %>% 
    # extract by points
    st_extract(NO2_sites) %>% 
    # convert result stars to data.frame
    as.data.frame() %>% 
    # add monitoring station names
    left_join(NO2_sites %>% select(Station_name) , by = "geometry") %>% 
    select(-geometry) %>% 
    distinct()
}

# =====================================
# annual mean 
# =====================================
# OMI
OMI_annual_df <- OMI_annual %>% 
  mutate(across(everything() , scale)) %>% 
  extract_NO2_sites()
# TROPOMI
TROPOMI_annual_df <- TROPOMI_annual %>% 
  mutate(across(everything() , scale)) %>% 
  extract_NO2_sites()
# temperature
temperature_annual_df <- temperature_annual %>% 
  mutate(across(everything() , scale)) %>% 
  extract_NO2_sites()
# boundary layer height
blh_annual_df <- blh_annual %>% 
  mutate(across(everything() , scale)) %>% 
  extract_NO2_sites()
# surface pressure
pressure_annual_df <- pressure_annual %>% 
  mutate(across(everything() , scale)) %>% 
  extract_NO2_sites()
# total precipitation
precipitation_annual_df <- precipitation_annual %>% 
  mutate(across(everything() , scale)) %>% 
  extract_NO2_sites()
# wind speed
ws_annual_df <- ws_annual %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()
# wind direction
wd_annual_df <- wd_annual %>% 
  mutate(across(everything() , scale)) %>% 
  extract_NO2_sites()
# total cloud cover
tcc_annual_df <- tcc_annual %>% 
  extract_NO2_sites()
# NDVI
NDVI_annual_df <- NDVI_annual %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()

# =====================================
# monthly mean
# =====================================
# OMI
OMI_monthly_df <- OMI_monthly %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()
# TROPOMI
TROPOMI_monthly_df <- TROPOMI_monthly %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()
# temperature
temperature_monthly_df <- temperature_monthly %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()
# boundary layer height
blh_monthly_df <- blh_monthly %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()
# surface pressure
pressure_monthly_df <- pressure_monthly %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()
# total precipitation
precipitation_monthly_df <- precipitation_monthly %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()
# wind speed
ws_monthly_df <- ws_monthly %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()
# wind direction
wd_monthly_df <- wd_monthly %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()
# total cloud cover
tcc_monthly_df <- tcc_monthly %>% 
  extract_NO2_sites()
# NDVI
NDVI_monthly_df <- NDVI_monthly %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()

# =====================================
# daily
# =====================================
# OMI
OMI_daily_df <- OMI_daily %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()
# TROPOMI
TROPOMI_daily_df <- TROPOMI_daily %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()
# temperature
temperature_daily_df <- temperature_daily %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()
# boundary layer height
blh_daily_df <- blh_daily %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()
# surface pressure
pressure_daily_df <- pressure_daily %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()
# total precipitation
precipitation_daily_df <- precipitation_daily %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()
# wind speed
ws_daily_df <- ws_daily %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()
# wind direction
wd_daily_df <- wd_daily %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()
# total cloud cover
tcc_daily_df <- tcc_daily %>% 
  extract_NO2_sites()
# NDVI
NDVI_daily_df <- NDVI_daily %>% 
  mutate(across(everything() , scale)) %>%
  extract_NO2_sites()


# =====================================
# spatial
# =====================================
# elevation (3D) ----------------------------------------------
elevation_df <- elevation %>% 
  # 3D to 2D (split radius)
  split() %>% 
  # rescale (standardized to mean=0 and sd=1)
  mutate(across(everything() , scale)) %>%
  # extract by points
  extract_NO2_sites() %>% 
  # clean column variable names {variable}_{moving-window-radius}
  setNames(str_replace(colnames(.) , "R_" , "elevation_"))

# emission (4D) ----------------------------------------------
emission_df <- emission %>% 
  # 4D to 3D (split radius)
  split(4) %>% 
  # log-transformation
  mutate(across(everything() , function(x) log(x+1))) %>%
  # rescale (standardized to mean=0 and sd=1)
  mutate(across(everything() , scale)) %>%
  # extract by points
  extract_NO2_sites() %>% 
  # clean column variable names {variable}_{moving-window-radius}
  setNames(str_replace(colnames(.) , "R_" , "emission_")) %>% 
  # 3D to 2D (split source)
  pivot_wider(names_from = source , 
              values_from = starts_with("emission") , 
              id_cols = Station_name) %>% 
  # clean column variable names enox_{source}_{moving-window-radius}
  setNames(
    ifelse(!str_detect(colnames(.) , "emission") , 
           colnames(.) , 
           colnames(.) %>% 
             str_replace("emission" , str_c("enox_" , str_extract(. , "[:alpha:]+$"))) %>% 
             str_replace("_[:alpha:]+$" , ""))
  )

# land cover (4D) ----------------------------------------------
landcover_df <- landcover %>% 
  # 4D to 3D (split radius)
  split(4) %>% 
  # [0-1], not standardized
  # extract by points
  extract_NO2_sites() %>% 
  # clean column variable names {variable}_{moving-window-radius}
  setNames(str_replace(colnames(.) , "R_" , "lc_percent_")) %>% 
  # 3D to 2D (split land cover group)
  pivot_wider(names_from = lc_group , 
              values_from = starts_with("lc_percent") , 
              id_cols = Station_name) %>% 
  # clean column variable names lc_{group}_{moving-window-radius}
  setNames(
    ifelse(!str_detect(colnames(.) , "percent") , 
           colnames(.) , 
           colnames(.) %>% 
             str_replace("lc" , str_c("lc_" , str_extract(. , "[:upper:]+$"))) %>% 
             str_replace("_[:upper:]+$" , ""))
  )

# nighttime light (3D) ----------------------------------------------
light_df <- light %>% 
  # 3D to 2D (split radius)
  split() %>% 
  # log-transformation
  mutate(across(everything() , function(x) log(x+1))) %>%
  # rescale (standardized to mean=0 and sd=1)
  mutate(across(everything() , scale)) %>%
  # extract by points
  extract_NO2_sites() %>% 
  # clean column variable names {variable}_{moving-window-radius}
  setNames(str_replace(colnames(.) , "R_" , "light_"))

# population (3D) ----------------------------------------------
population_df <- population %>% 
  # 3D to 2D (split radius)
  split() %>% 
  # log-transformation
  mutate(across(everything() , function(x) log(x+1))) %>%
  # rescale (standardized to mean=0 and sd=1)
  mutate(across(everything() , scale)) %>%
  # extract by points
  extract_NO2_sites() %>% 
  # clean column variable names {variable}_{moving-window-radius}
  setNames(str_replace(colnames(.) , "R_" , "population_"))

# traffic intensity (3D) ----------------------------------------------
traffint_df <- traffint %>% 
  # 3D to 2D (split radius)
  split() %>% 
  # log-transformation
  mutate(across(everything() , function(x) log(x+1))) %>%
  # rescale (standardized to mean=0 and sd=1)
  mutate(across(everything() , scale)) %>%
  # extract by points
  extract_NO2_sites() %>% 
  # clean column variable names {variable}_{moving-window-radius}
  setNames(str_replace(colnames(.) , "R_" , "traffint_"))

# intersections (3D) ----------------------------------------------
intersection_df <- intersection %>% 
  # 3D to 2D (split radius)
  split() %>% 
  # log-transformation
  mutate(across(everything() , function(x) log(x+1))) %>%
  # rescale (standardized to mean=0 and sd=1)
  mutate(across(everything() , scale)) %>%
  # extract by points
  extract_NO2_sites() %>% 
  # clean column variable names {variable}_{moving-window-radius}
  setNames(str_replace(colnames(.) , "R_" , "intersection_"))

# major road density (3D) ----------------------------------------------
mjdens_df <- mjdens %>% 
  # 3D to 2D (split radius)
  split() %>% 
  # log-transformation
  mutate(across(everything() , function(x) log(x+1))) %>%
  # rescale (standardized to mean=0 and sd=1)
  mutate(across(everything() , scale)) %>%
  # extract by points
  extract_NO2_sites() %>% 
  # clean column variable names {variable}_{moving-window-radius}
  setNames(str_replace(colnames(.) , "R_" , "mjdens_"))

# all road density (3D) ----------------------------------------------
rdens_df <- rdens %>% 
  # 3D to 2D (split radius)
  split() %>% 
  # log-transformation
  mutate(across(everything() , function(x) log(x+1))) %>%
  # rescale (standardized to mean=0 and sd=1)
  mutate(across(everything() , scale)) %>%
  # extract by points
  extract_NO2_sites() %>% 
  # clean column variable names {variable}_{moving-window-radius}
  setNames(str_replace(colnames(.) , "R_" , "rdens_"))

# distance to nearest major road (3D) ----------------------------------------------
nearmjrd_df <- nearmjrd %>% 
  # rescale
  mutate(across(everything() , scale)) %>%
  # extract by points
  extract_NO2_sites()

# NDVI(spatial) (3D) ----------------------------------------------
NDVI_sp_df <- NDVI_sp %>% 
  # rescale
  mutate(across(everything() , scale)) %>%
  # extract by points
  extract_NO2_sites()


# //////////////////////////////////////////////////////////////////////////
# data wrangling for the monitoring data
# //////////////////////////////////////////////////////////////////////////
# =====================================
# monitoring sites
# =====================================
NO2_sites_adm1 <- NO2_sites %>% 
  # include Canton name and ID
  # note: some sites are outside Switzerland (Fridriechhafen, Konstanz, Vaduz)
  st_join(
    NO2_sites %>% 
      st_intersection(select(Swiss_adm1 ,  ID_1 , NAME_1)) %>% 
      rename(Canton_ID = ID_1 , Canton_name = NAME_1) %>% 
      select(Canton_ID , Canton_name)
  )
# ggplot() +
#   geom_stars(data = elevation %>%
#                filter(radius == "R_0") %>% .[Swiss_adm1] ,
#              alpha = 0.5) +
#   geom_sf(data = Swiss_adm1 , color = "azure1" , fill = NA) +
#   geom_sf(data = NO2_sites_adm1 , aes(color = Type_of_station)) +
#   coord_sf(expand = FALSE) +
#   scale_fill_gradientn(colors = terrain.colors(7)) +
#   labs(x = "Longtitude" , y = "Latitude" , fill = "Altitude" , color = "Type of station" ,
#        title = "Monitoring sites") +
#   theme_bw()

# =====================================
# measured NO2 values
# =====================================
# daily
NO2_daily_df <- NO2_df_raw %>% 
  # remove all-NA columns
  select_if(colSums(is.na(.)) < 365) %>% 
  # to long table (expend stations)
  pivot_longer(cols = -date , names_to = "Station_name" , values_to = "NO2") %>% 
  # clean names
  mutate(Station_name = str_replace(Station_name , "^Delemont$" , "Delémont")) %>% 
  mutate(Station_name = str_replace(Station_name , "^Lustenau-Wiesenrain$" , "Lustenau Wiesenrain")) %>% 
  mutate(Station_name = str_replace(Station_name , "^Messoco-A13$" , "Mesocco-A13")) %>% 
  mutate(Station_name = str_replace(Station_name , "^Nyon-Hopitâl$" , "Nyon-Hôpital")) %>% 
  mutate(Station_name = str_replace(Station_name , "^Dübendorf$" , "Dübendorf-EMPA")) %>% 
  mutate(Station_name = str_replace(Station_name , "^Härkingen$" , "Härkingen-A1")) 

# annual mean
NO2_annual_df <- NO2_daily_df %>% 
  # annual mean of the monitored NO2
  mutate(year = year(date)) %>% 
  group_by(year , Station_name) %>% 
  summarize(NO2 = mean(NO2 , na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(-year)

# monthly mean
NO2_monthly_df <- NO2_daily_df %>% 
  # monthly mean of the monitored NO2
  mutate(month = month(date)) %>% 
  group_by(month , Station_name) %>% 
  summarize(NO2 = mean(NO2 , na.rm = TRUE)) %>% 
  ungroup()


# //////////////////////////////////////////////////////////////////////////
# join to the observed NO2
# //////////////////////////////////////////////////////////////////////////
# =====================================
# annual data
# =====================================
data_annual <- NO2_annual_df %>% 
  # metadata of measurement stations
  left_join(
    NO2_sites_adm1 %>% 
      bind_cols(st_coordinates(.) %>% as.data.frame) %>% 
      as.data.frame() %>% 
      select(Station_name , Type_of_zone , Type_of_station , Altitude , Canton_ID , Canton_name , X , Y) , 
    by = "Station_name"
  ) %>% 
  # OMI
  left_join(OMI_annual_df , by = "Station_name") %>% 
  # TROPOMI
  left_join(TROPOMI_annual_df , by = "Station_name") %>% 
  # temperature
  left_join(temperature_annual_df , by = "Station_name") %>% 
  # blh
  left_join(blh_annual_df , by = "Station_name") %>% 
  # pressure
  left_join(pressure_annual_df , by = "Station_name") %>% 
  # precipitation
  left_join(precipitation_annual_df , by = "Station_name") %>% 
  # total cloud cover
  left_join(tcc_annual_df , by = "Station_name") %>% 
  # ws
  left_join(ws_annual_df , by = "Station_name") %>% 
  # wd
  left_join(wd_annual_df , by = "Station_name") %>% 
  # NDVI
  left_join(NDVI_annual_df , by = "Station_name") %>% 
  # elevation
  left_join(elevation_df , by = "Station_name") %>% 
  # emission
  left_join(emission_df , by = "Station_name") %>% 
  # land cover
  left_join(landcover_df , by = "Station_name") %>% 
  # nighttime light
  left_join(light_df , by = "Station_name") %>% 
  # population
  left_join(population_df , by = "Station_name") %>% 
  # traffic intensity
  left_join(traffint_df , by = "Station_name") %>% 
  # major road density
  left_join(mjdens_df , by = "Station_name") %>% 
  # all road density
  left_join(rdens_df , by = "Station_name") %>% 
  # intersections
  left_join(intersection_df , by = "Station_name") %>% 
  # distance to nearest road
  left_join(nearmjrd_df , by = "Station_name") %>% 
  # NDVI
  left_join(NDVI_sp_df , by = "Station_name")

# =====================================
# monthly data
# =====================================
data_monthly <- NO2_monthly_df %>% 
  # metadata of measurement stations
  left_join(
    NO2_sites_adm1 %>% 
      bind_cols(st_coordinates(.) %>% as.data.frame) %>% 
      as.data.frame() %>% 
      select(Station_name , Type_of_zone , Type_of_station , Altitude , Canton_ID , Canton_name , X , Y) , 
    by = "Station_name"
  ) %>% 
  # OMI
  left_join(OMI_monthly_df , by = c("Station_name" , "month")) %>% 
  # TROPOMI
  left_join(TROPOMI_monthly_df , by = c("Station_name" , "month")) %>% 
  # temperature
  left_join(temperature_monthly_df , by = c("Station_name" , "month")) %>% 
  # blh
  left_join(blh_monthly_df , by = c("Station_name" , "month")) %>% 
  # pressure
  left_join(pressure_monthly_df , by = c("Station_name" , "month")) %>% 
  # precipitation
  left_join(precipitation_monthly_df , by = c("Station_name" , "month")) %>% 
  # total cloud cover
  left_join(tcc_monthly_df , by = c("Station_name" , "month")) %>% 
  # ws
  left_join(ws_monthly_df , by = c("Station_name" , "month")) %>% 
  # wd
  left_join(wd_monthly_df , by = c("Station_name" , "month")) %>% 
  # NDVI
  left_join(NDVI_monthly_df , by = c("Station_name" , "month")) %>% 
  # elevation
  left_join(elevation_df , by = "Station_name") %>% 
  # emission
  left_join(emission_df , by = "Station_name") %>% 
  # land cover
  left_join(landcover_df , by = "Station_name") %>% 
  # nighttime light
  left_join(light_df , by = "Station_name") %>% 
  # population
  left_join(population_df , by = "Station_name") %>% 
  # traffic intensity
  left_join(traffint_df , by = "Station_name") %>% 
  # major road density
  left_join(mjdens_df , by = "Station_name") %>% 
  # all road density
  left_join(rdens_df , by = "Station_name") %>% 
  # intersections
  left_join(intersection_df , by = "Station_name") %>% 
  # distance to nearest road
  left_join(nearmjrd_df , by = "Station_name") %>% 
  # NDVI
  left_join(NDVI_sp_df , by = "Station_name")

# =====================================
# daily data
# =====================================
data_daily <- NO2_daily_df %>% 
  # metadata of measurement stations
  left_join(
    NO2_sites_adm1 %>% 
      bind_cols(st_coordinates(.) %>% as.data.frame) %>% 
      as.data.frame() %>% 
      select(Station_name , Type_of_zone , Type_of_station , Altitude , Canton_ID , Canton_name , X , Y) , 
    by = "Station_name"
  ) %>% 
  # OMI
  left_join(OMI_daily_df , by = c("Station_name" , "date")) %>% 
  # TROPOMI
  left_join(TROPOMI_daily_df , by = c("Station_name" , "date")) %>% 
  # temperature
  left_join(temperature_daily_df , by = c("Station_name" , "date")) %>% 
  # blh
  left_join(blh_daily_df , by = c("Station_name" , "date")) %>% 
  # pressure
  left_join(pressure_daily_df , by = c("Station_name" , "date")) %>% 
  # precipitation
  left_join(precipitation_daily_df , by = c("Station_name" , "date")) %>% 
  # total cloud cover
  left_join(tcc_daily_df , by = c("Station_name" , "date")) %>% 
  # ws
  left_join(ws_daily_df , by = c("Station_name" , "date")) %>% 
  # wd
  left_join(wd_daily_df , by = c("Station_name" , "date")) %>% 
  # NDVI
  left_join(NDVI_daily_df , by = c("Station_name" , "date")) %>% 
  # elevation
  left_join(elevation_df , by = "Station_name") %>% 
  # emission
  left_join(emission_df , by = "Station_name") %>% 
  # land cover
  left_join(landcover_df , by = "Station_name") %>% 
  # nighttime light
  left_join(light_df , by = "Station_name") %>% 
  # population
  left_join(population_df , by = "Station_name") %>% 
  # traffic intensity
  left_join(traffint_df , by = "Station_name") %>% 
  # major road density
  left_join(mjdens_df , by = "Station_name") %>% 
  # all road density
  left_join(rdens_df , by = "Station_name") %>% 
  # intersections
  left_join(intersection_df , by = "Station_name") %>% 
  # distance to nearest road
  left_join(nearmjrd_df , by = "Station_name") %>% 
  # NDVI
  left_join(NDVI_sp_df , by = "Station_name")

# //////////////////////////////////////////////////////////////////////////
# export 
# //////////////////////////////////////////////////////////////////////////
out_dirpath_extracted <- "1_data/processed/cleaned/extracted"
if(!dir.exists(out_dirpath_extracted)) dir.create(out_dirpath_extracted)

# =====================================
# annual model
# =====================================
data_annual %>% 
  write.csv(file = paste0(out_dirpath_extracted , "/annual_scaled.csv") , 
            row.names = FALSE)

# =====================================
# monthly model
# =====================================
data_monthly %>% 
  write.csv(file = paste0(out_dirpath_extracted , "/monthly_scaled.csv") , 
            row.names = FALSE)

# =====================================
# daily model
# =====================================
data_daily %>% 
  write.csv(file = paste0(out_dirpath_extracted , "/daily_scaled.csv") , 
            row.names = FALSE)


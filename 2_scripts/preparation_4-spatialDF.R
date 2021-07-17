#####################################################################################################
# Masterarbeit
# Prepare data : reshape the spatial predictor variables into a data.frame for later modeling prediction
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
library(stringr)

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

# =====================================
# spatial predictors as a data.frame
# with scaling (refer to preparation_3-extract.R)
# =====================================
spatial_df <- 
  # elevation ----------------------------
  elevation %>% 
  # 3D to 2D (split radius)
  split() %>% 
  # rescale (standardized to mean=0 and sd=1)
  mutate(across(everything() , scale)) %>%
  # as data.frame
  as.data.frame() %>% 
  # clean column variable names {variable}_{moving-window-radius}
  setNames(str_replace(colnames(.) , "R_" , "elevation_")) %>% 
  # emission ----------------------------
  left_join(
    emission %>% 
      # 4D to 3D (split radius)
      split(4) %>% 
      # log-transformation
      mutate(across(everything() , function(x) log(x+1))) %>%
      # rescale (standardized to mean=0 and sd=1)
      mutate(across(everything() , scale)) %>%
      # as data.frame
      as.data.frame() %>% 
      # clean column variable names {variable}_{moving-window-radius}
      setNames(str_replace(colnames(.) , "R_" , "emission_")) %>% 
      # 3D to 2D (split source)
      pivot_wider(names_from = source , 
                  values_from = starts_with("emission") , 
                  c(x,y)) %>% 
      # clean column variable names enox_{source}_{moving-window-radius}
      setNames(
        ifelse(!str_detect(colnames(.) , "emission") , 
               colnames(.) , 
               colnames(.) %>% 
                 str_replace("emission" , str_c("enox_" , str_extract(. , "[:alpha:]+$"))) %>% 
                 str_replace("_[:alpha:]+$" , ""))
      ) , 
    by = c("x","y")
  ) %>% 
  # land cover ----------------------------
  left_join(
    landcover %>% 
      # 4D to 3D (split radius)
      split(4) %>% 
      # [0-1], not standardized
      # as data.frame
      as.data.frame() %>% 
      # clean column variable names {variable}_{moving-window-radius}
      setNames(str_replace(colnames(.) , "R_" , "lc_percent_")) %>% 
      # 3D to 2D (split land cover group)
      pivot_wider(names_from = lc_group , 
                  values_from = starts_with("lc_percent") , 
                  c(x,y)) %>% 
      # clean column variable names lc_{group}_{moving-window-radius}
      setNames(
        ifelse(!str_detect(colnames(.) , "percent") , 
               colnames(.) , 
               colnames(.) %>% 
                 str_replace("lc" , str_c("lc_" , str_extract(. , "[:upper:]+$"))) %>% 
                 str_replace("_[:upper:]+$" , ""))
      ) , 
    by = c("x" , "y")
  ) %>% 
  # nighttime light ----------------------------
  left_join(
    light %>% 
      # 3D to 2D (split radius)
      split() %>% 
      # log-transformation
      mutate(across(everything() , function(x) log(x+1))) %>%
      # rescale (standardized to mean=0 and sd=1)
      mutate(across(everything() , scale)) %>%
      # as data.frame
      as.data.frame() %>% 
      # clean column variable names {variable}_{moving-window-radius}
      setNames(str_replace(colnames(.) , "R_" , "light_")) , 
    by = c("x" , "y")
  ) %>% 
  # population ----------------------------
  left_join(
    population %>% 
      # 3D to 2D (split radius)
      split() %>% 
      # log-transformation
      mutate(across(everything() , function(x) log(x+1))) %>%
      # rescale (standardized to mean=0 and sd=1)
      mutate(across(everything() , scale)) %>%
      # as data.frame
      as.data.frame() %>% 
      # clean column variable names {variable}_{moving-window-radius}
      setNames(str_replace(colnames(.) , "R_" , "population_")) , 
    by = c("x","y")
  ) %>% 
  # traffic intensity ----------------------------
  left_join(
    traffint %>% 
      # 3D to 2D (split radius)
      split() %>% 
      # log-transformation
      mutate(across(everything() , function(x) log(x+1))) %>%
      # rescale (standardized to mean=0 and sd=1)
      mutate(across(everything() , scale)) %>%
      # as data.frame
      as.data.frame() %>% 
      # clean column variable names {variable}_{moving-window-radius}
      setNames(str_replace(colnames(.) , "R_" , "traffint_")) , 
    by = c("x" , "y")
  ) %>% 
  # intersections ----------------------------
  left_join(
    intersection %>% 
      # 3D to 2D (split radius)
      split() %>% 
      # log-transformation
      mutate(across(everything() , function(x) log(x+1))) %>%
      # rescale (standardized to mean=0 and sd=1)
      mutate(across(everything() , scale)) %>%
      # as data.frame
      as.data.frame() %>% 
      # clean column variable names {variable}_{moving-window-radius}
      setNames(str_replace(colnames(.) , "R_" , "intersection_")) , 
    by = c("x" , "y")
  ) %>% 
  # major road density ----------------------------
  left_join(
    mjdens %>% 
      # 3D to 2D (split radius)
      split() %>% 
      # log-transformation
      mutate(across(everything() , function(x) log(x+1))) %>%
      # rescale (standardized to mean=0 and sd=1)
      mutate(across(everything() , scale)) %>%
      # as data.frame
      as.data.frame() %>% 
      # clean column variable names {variable}_{moving-window-radius}
      setNames(str_replace(colnames(.) , "R_" , "mjdens_")) , 
    by = c("x" , "y")
  ) %>% 
  # all road density ----------------------------
  left_join(
    rdens %>% 
      # 3D to 2D (split radius)
      split() %>% 
      # log-transformation
      mutate(across(everything() , function(x) log(x+1))) %>%
      # rescale (standardized to mean=0 and sd=1)
      mutate(across(everything() , scale)) %>%
      # as data.frame
      as.data.frame() %>% 
      # clean column variable names {variable}_{moving-window-radius}
      setNames(str_replace(colnames(.) , "R_" , "rdens_")) , 
    by = c("x" , "y")
  ) %>% 
  # distance to nearest major road ----------------------------
  left_join(
    nearmjrd %>% 
      # rescale
      mutate(across(everything() , scale)) %>%
      # as data.frame
      as.data.frame() , 
    by = c("x" , "y")
  ) %>% 
  # NDVI_sp ----------------------------
  left_join(
    NDVI_sp %>% 
      # rescale
      mutate(across(everything() , scale)) %>%
      # as data.frame
      as.data.frame() , 
    by = c("x" , "y")
  )

# =====================================
# export
# =====================================
out_dirpath <- "1_data/processed/cleaned/data-frame"
if(!dir.exists(out_dirpath)) dir.create(out_dirpath)
spatial_df %>% 
  write_csv(file = paste0(out_dirpath , "/spatial_df.csv") )


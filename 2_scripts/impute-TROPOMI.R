#####################################################################################################
# Masterarbeit
# Impute TROPOMI-NO2 
# 2021-05-11
#####################################################################################################

# =====================================
# required packages
# =====================================
library(stars) ; library(sf) ; library(raster)
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(ggsci)
library(lubridate) ; library(stringr)

library(randomForest) ; library(ranger)

# //////////////////////////////////////////////////////////////////////////
# data preparation 
# //////////////////////////////////////////////////////////////////////////

# =====================================
# load TROPOMI data
# =====================================
# file paths
files_TROPOMI <- list.files("1_data/raw/TROPOMI-NO2/preprocessed_resampled" , "_AOI_rs.tif$" , full.names = TRUE)
# timestamps
ts_TROPOMI_raw <- basename(files_TROPOMI) %>% 
  str_extract("\\d{8}") %>% 
  as_date()

# read as a 3D array: x, y, date
TROPOMI_raw <- read_stars(files_TROPOMI , 
                          along = list(date = ts_TROPOMI_raw) , 
                          proxy = FALSE) %>% 
  st_set_dimensions(values = rep(c("NO2" , "QA")) , "band") %>% 
  # !! screen NO2 pixels using QA_value > 0.75 !!
  split("band") %>%  # split dimension "band" into attributes: NO2 and QA
  transmute(value = ifelse(QA>0.75 , NO2 , NA_real_)) # filter NO2 values based on QA values


# =====================================
# load predictor variables 
# =====================================
# CAMS NO and NO2
# 3D array: x, y, time 
CAMS_raw <- read_stars("1_data/raw/ECMWF-CAMS-NO2/CAMS-NO2.nc") %>% 
  st_set_crs(st_crs(4326)) # long-lat coordinates

# elevation
# 2D array: x, y
DEM_raw <- read_stars("1_data/raw/EU-DEM-1.1/eu_dem_v11_E40N20_AOI.tif")

# ERA5
ERA_raw <- read_stars("1_data/raw/ECMWF-ERA5/ERA5-meteorology.nc") %>% 
  st_set_crs(st_crs(4326)) # long-lat coordinates

# =====================================
# load Switzerland, AOI
# =====================================
CH <- st_read("1_data/raw/Switzerland_shapefile/CHE_adm0.shp") %>% 
  st_geometry() %>% 
  st_simplify(preserveTopology = TRUE , dTolerance = 0.05)
AOI <- st_read("1_data/raw/Switzerland_shapefile/AOI_4326.shp") 


# =====================================
# data preparation
# resampling to TROPOMI grids
# =====================================
# 1. CAMS --------------------------------

# default: nearest neighbor resampling (downscaling 0.75˚ CAMS)
CAMS_rsNN <- CAMS_raw %>% 
  st_warp(dest = TROPOMI_raw) 
{ # separate the resampled CAMS data into sub-datasets(8*2=16) to have the same dimension as TROPOMI
  subset.CAMS_hour <- CAMS_rsNN %>% 
    st_get_dimension_values(3) %>% 
    hour() %>% 
    unique()
  # 2 attributes: "tcno2" "tc_no"
  subset.CAMS_var <- CAMS_rsNN %>% names()
  # subset into 16 3D arrays
  for(h in subset.CAMS_hour){
    for(v in subset.CAMS_var){
      # subset the raw dataset: at hour h and for variable (attribute) v
      CAMS_rs_temp <- CAMS_rsNN %>% 
        filter(hour(time) == h) %>% 
        select(all_of(v)) %>% # Note: Using an external vector in selections is ambiguous. Use `all_of(v)` instead of `v` to silence this message.
        setNames(v) %>% 
        # dimension: date
        st_set_dimensions(3 , 
                          values = as_date(st_get_dimension_values(. , 3)) , 
                          names = "date")
      # name the object name as CAMS_rs_{Variable}_{Hour}
      assign(paste0("CAMS_rs_" , v , "_" , h) , CAMS_rs_temp)
      rm(CAMS_rs_temp)
    }
  }
  # clean intermediate objects of the loop
  rm(h , v , subset.CAMS_hour , subset.CAMS_var)
}

# compare TROPOMI and CAMS grids
# only the grids
{
  ggplot() +
    geom_sf(data = CH , fill = "azure4" , color = NA) +
    # CAMS resampled to OMI grids
    geom_sf(data = CAMS_rs_tcno2_0[,,,1] %>% 
              # convert the grids to polygon to visualize the grid cells
              st_as_sf(as_points = FALSE) ,  
            color = "dodgerblue3" , fill = NA) +
    # original CAMS grids
    geom_sf(data = CAMS_raw[,,,1] %>% 
              # convert the grids to polygon to visualize the grid cells
              st_as_sf(as_points = FALSE) , 
            color = "deeppink" , fill = NA) +
    # crop graph canvas to AOI
    coord_sf(xlim = st_bbox(AOI)[c("xmin" , "xmax")] , 
             ylim = st_bbox(AOI)[c("ymin" , "ymax")]) +
    labs(x = "Longitude" , y = "Latitude" , 
         title = "Comparison between OMI and CAMS grids" , 
         subtitle = "Pink: original CAMS grids; Blue: CAMS resampled to TROPOMI grids") +
    theme_bw()
}

# with pixel values 
{
  cowplot::plot_grid(
    # plot: CAMS with original grids
    ggplot() +
      # first image
      geom_stars(data = CAMS_raw[1,,,977]) +
      # reference grid cells
      geom_sf(data = CAMS_raw[,,,1] %>% 
                # convert the grids to polygon to visualize the grid cells
                st_as_sf(as_points = FALSE) , 
              color = "azure3" , fill = NA , size = 0.1) +
      # Switzerland
      geom_sf(data = CH , fill = NA , color = "white") +
      scale_fill_viridis_c() +
      # crop graph canvas to AOI
      coord_sf(xlim = st_bbox(AOI)[c("xmin" , "xmax")] , 
               ylim = st_bbox(AOI)[c("ymin" , "ymax")]) +
      labs(x = "Longtitude" , y = "Latitude" , fill = "NO2" , 
           title = "Original CAMS" , 
           subtitle = "Data: CAMS total column NO2 on 2019-05-03 00H") , 
    # plot: CAMS resampled to TROPOMI grids
    ggplot() +
      # first image
      geom_stars(data = CAMS_rs_tcno2_0[,,,123]) +
      # reference grid cells
      geom_sf(data = CAMS_rsNN[,,,1] %>% 
                # convert the grids to polygon to visualize the grid cells
                st_as_sf(as_points = FALSE) , 
              color = "azure3" , fill = NA , size = 0.1) +
      # Switzerland
      geom_sf(data = CH , fill = NA , color = "white") +
      scale_fill_viridis_c() +
      # crop graph canvas to AOI
      coord_sf(xlim = st_bbox(AOI)[c("xmin" , "xmax")] , 
               ylim = st_bbox(AOI)[c("ymin" , "ymax")]) +
      labs(x = "Longtitude" , y = "Latitude" , fill = "NO2" , 
           title = "Resampled-CAMS to TROPOMI grids" , 
           subtitle = "Data: CAMS total column NO2 on 2019-05-03 00H") 
  )
}


# 2. elevation --------------------------------
# 25*25m -> 7*13km upscaling for continuous data
# bilinear interpolation
DEM_rs <- DEM_raw %>% 
  st_warp(TROPOMI_raw[,,,1] %>% split("date") , 
          use_gdal = TRUE , 
          method = "bilinear") %>% 
  setNames("DEM")
ggplot() +
  geom_stars(data = DEM_rs) +
  geom_sf(data = CH , fill = NA , color = "white") +
  scale_fill_gradientn(colors = terrain.colors(10)) +
  coord_sf(expand = FALSE) +
  labs(x = "Longtitude" , y = "Latitude" , fill = "Altitude")


# (3. ERA5)  --------------------------------
# default: nearest neighbor resampling
ERA_rsNN <- ERA_raw %>% 
  st_warp(dest = TROPOMI_raw) 
{ # separate the resampled CAMS data into sub-datasets(8*2=16) to have the same dimension as OMI
  subset.ERA_hour <- ERA_rsNN %>% 
    st_get_dimension_values(3) %>% 
    hour() %>% 
    unique()
  # 7 attributes: "u10" "v10" "t2m" "blh" "sp"  "tcc" "tp" 
  subset.ERA_var <- ERA_rsNN %>% names()
  # subset into 16 3D arrays
  for(h in subset.ERA_hour){
    for(v in subset.ERA_var){
      # subset the raw dataset: at hour h and for variable (attribute) v
      ERA_rs_temp <- ERA_rsNN %>% 
        filter(hour(time) == h) %>% 
        select(all_of(v)) %>% 
        setNames(v) %>% 
        # # dimension: date
        st_set_dimensions(3 , 
                          values = as_date(st_get_dimension_values(. , 3)) , 
                          names = "date")
      # name the object name as CAMS_rs_{Variable}_{Hour}
      assign(paste0("ERA_rs_" , v , "_" , h) , ERA_rs_temp)
      rm(ERA_rs_temp)
    }
  }
  # clean intermediate objects of the loop
  rm(h , v , subset.ERA_hour , subset.ERA_var)
}

# convert u,v wind components to wind speed, wind direction
{
  subset.ERA_hour <- ERA_rsNN %>% 
    st_get_dimension_values(3) %>% 
    hour() %>% 
    unique()
  for(h in subset.ERA_hour){
    ERA_rs_wind_temp <- c(
      get(paste0("ERA_rs_u10_" , h)) , 
      get(paste0("ERA_rs_v10_" , h))
    ) %>% 
      # wd = atan2(-u10,-v10)
      # ws = sqrt(u10^2 + v10^2)
      transmute(wd = atan2(-u10 , -v10) , 
                ws = sqrt(u10^2+v10^2))
    # name the object name as CAMS_rs_wind_{Hour}
    assign(paste0("ERA_rs_wind_" , h) , ERA_rs_wind_temp)
  }
  # clean
  rm(h , subset.ERA_hour , ERA_rs_wind_temp)
}


# =====================================
# data preparation
# combining spatial and spatialtemporal predictor sets
# =====================================
# spatialtemporal datasets: 3D (x,y,date) + attributes --------------------------------
spatialtemporal <- c(
  # TROPOMI 
  TROPOMI_raw %>% 
    setNames("TROPOMI_NO2") , 
  # CAMS NO2
  c(CAMS_rs_tcno2_0 , CAMS_rs_tcno2_3 , CAMS_rs_tcno2_6 , CAMS_rs_tcno2_9 , 
    CAMS_rs_tcno2_12 , CAMS_rs_tcno2_15 , CAMS_rs_tcno2_18 , CAMS_rs_tcno2_21) %>% 
    setNames(c("CAMS_NO2_00" , "CAMS_NO2_03" , "CAMS_NO2_06" , "CAMS_NO2_09" , 
               "CAMS_NO2_12" , "CAMS_NO2_15" , "CAMS_NO2_18" , "CAMS_NO2_21")) , 
  # CAMS NO
  c(CAMS_rs_tc_no_0 , CAMS_rs_tc_no_3 , CAMS_rs_tc_no_6 , CAMS_rs_tc_no_9 , 
    CAMS_rs_tc_no_12 , CAMS_rs_tc_no_15 , CAMS_rs_tc_no_18 , CAMS_rs_tc_no_21) %>% 
    setNames(c("CAMS_NO_00" , "CAMS_NO_03" , "CAMS_NO_06" , "CAMS_NO_09" , 
               "CAMS_NO_12" , "CAMS_NO_15" , "CAMS_NO_18" , "CAMS_NO_21")) , 
  # ERA blh
  c(ERA_rs_blh_0 , ERA_rs_blh_3 , ERA_rs_blh_6 , ERA_rs_blh_9 ,
    ERA_rs_blh_12 , ERA_rs_blh_15 , ERA_rs_blh_18 , ERA_rs_blh_21) %>%
    setNames(c("blh_00" , "blh_03" , "blh_06" , "blh_09" ,
               "blh_12" , "blh_15" , "blh_18" , "blh_21")) , 
  # ERA temperature
  c(ERA_rs_t2m_0 , ERA_rs_t2m_3 , ERA_rs_t2m_6 , ERA_rs_t2m_9 , 
    ERA_rs_t2m_12 , ERA_rs_t2m_15 , ERA_rs_t2m_18 , ERA_rs_t2m_21) %>% 
    setNames(c("t2m_00" , "t2m_03" , "t2m_06" , "t2m_09" , 
               "t2m_12" , "t2m_15" , "t2m_18" , "t2m_21")) , 
  # ERA surface pressure
  c(ERA_rs_sp_0 , ERA_rs_sp_3 , ERA_rs_sp_6 , ERA_rs_sp_9 , 
    ERA_rs_sp_12 , ERA_rs_sp_15 , ERA_rs_sp_18 , ERA_rs_sp_21) %>% 
    setNames(c("sp_00" , "sp_03" , "sp_06" , "sp_09" , 
               "sp_12" , "sp_15" , "sp_18" , "sp_21")) , 
  # ERA total cloud cover
  c(ERA_rs_tcc_0 , ERA_rs_tcc_3 , ERA_rs_tcc_6 , ERA_rs_tcc_9 , 
    ERA_rs_tcc_12 , ERA_rs_tcc_15 , ERA_rs_tcc_18 , ERA_rs_tcc_21) %>% 
    setNames(c("tcc_00" , "tcc_03" , "tcc_06" , "tcc_09" , 
               "tcc_12" , "tcc_15" , "tcc_18" , "tcc_21")) ,
  # ERA total precipitation
  c(ERA_rs_tp_0 , ERA_rs_tp_3 , ERA_rs_tp_6 , ERA_rs_tp_9 , 
    ERA_rs_tp_12 , ERA_rs_tp_15 , ERA_rs_tp_18 , ERA_rs_tp_21) %>% 
    setNames(c("tp_00" , "tp_03" , "tp_06" , "tp_09" , 
               "tp_12" , "tp_15" , "tp_18" , "tp_21")) , 
  # ERA wind
  c(ERA_rs_wind_0 , ERA_rs_wind_3 , ERA_rs_wind_6 , ERA_rs_wind_9 , 
    ERA_rs_wind_12 , ERA_rs_wind_15 , ERA_rs_wind_18 , ERA_rs_wind_21) %>% 
    setNames(c("wd_00", "ws_00", "wd_03", "ws_03", "wd_06", "ws_06", "wd_09", "ws_09",
               "wd_12", "ws_12", "wd_15", "ws_15", "wd_18", "ws_18", "wd_21", "ws_21"))
) %>% 
  st_set_dimensions(3 , values = yday(st_get_dimension_values(. , 3)) , names = "DOY")

# spatial datasets: 2D (x,y) + attributes --------------------------------
spatial <- c(
  # DEM
  DEM_rs
)


# input data frame for the imputation model --------------------------------
imputation_df <- spatialtemporal %>% 
  as.data.frame() %>% 
  mutate(across(everything() , as.numeric)) %>% 
  left_join(spatial %>% as.data.frame() , 
            by = c("x" , "y")) %>% 
  # Some pixels are outside of the AOI and are always NA, 
  # should be removed from the data.frame so that it wouldn’t be counted.
  full_join(
    # AOI mask in raster form
    AOI %>% 
      st_rasterize(template = spatial) %>% 
      # is.AOI: a column with TRUE/FALSE 
      mutate(is.AOI = ifelse(is.na(FID) , FALSE , TRUE)) %>% 
      as.data.frame(), 
    by = c("x" , "y")
  ) %>% 
  # remove the pixels that are outside AOI (is.AOI=FALSE)
  filter(is.AOI) %>% 
  # remove the FID and is.AOI columns from the AOI mask
  select(-FID , -is.AOI)


# =====================================
# cross validation design: spatial cross validation
# =====================================
# spatially blocked k-fold cross validation
k_fold <- 10

# spatially-blocked by grids across AOI
set.seed(2201)
# make grids and assign cross validation index
spatialGrid_CV <- AOI %>% 
  # reporject to OMI
  st_transform(st_crs(spatial)) %>% 
  # make grids
  st_make_grid(n = c(10,4)) %>% 
  st_as_sf() %>% 
  # randomly assign cross validation index
  mutate(spatial_CV = sample(1:k_fold , nrow(.) , replace = TRUE))
# rasterize the grids with CV-index to stars
spatialGrid_CV_stars <- st_rasterize(spatialGrid_CV["spatial_CV"], template = spatial) %>% 
  mutate(spatial_CV = as.factor(spatial_CV))
# visualize
ggplot() +
  geom_stars(data = spatialGrid_CV_stars) +
  geom_sf(data = CH , fill = NA , color = "white") +
  geom_sf(data = AOI , fill = NA , color = "azure2") +
  scale_fill_npg() +
  coord_sf(expand = FALSE) +
  labs(x = "Longtitude" , y = "Latitude" , fill = "k-fold \ncross \nvalidation \ngroup")
# check if the number of OMI pixels in each fold is even
table(pull(spatialGrid_CV_stars))

# include the spatial CV design into the input data
imputation_df <- spatialGrid_CV_stars %>% 
  setNames("spatial_CV") %>% 
  as.data.frame() %>% 
  # join to the input data
  right_join(imputation_df , by = c("x" , "y")) 


# //////////////////////////////////////////////////////////////////////////
# model development
# //////////////////////////////////////////////////////////////////////////

# ===================================== 
# model development: select hour for CAMS and ERA5 data
# =====================================
search_hour <- c("00" , "03" , "06" , "09" , "12" , "15" , "18" , "21")
search_var <- list(
  # base model
  base = c("x" , "y" , "DOY" , "DEM" , "CAMS_NO2_" , "CAMS_NO_") , 
  # base + temperature, blh, tcc, sp
  base_meteo6 = c("x" , "y" , "DOY" , "DEM" , "CAMS_NO2_" , "CAMS_NO_" , "t2m_" , "blh_" , "tcc_" , "sp_" , "wd_" , "ws_") ,
  # base + all meteorological variables
  base_meteo7 = c("x" , "y" , "DOY" , "DEM" , "CAMS_NO2_" , "CAMS_NO_" , "t2m_" , "blh_" , "tcc_" , "sp_" , "wd_" , "ws_" , "tp_")
)


# grid search over every combination
N.trees <- 250
{
  gridSearch_hour_var <- list()
  gridSearch_pb <- txtProgressBar(min = 0, max = length(search_hour) * length(search_var) , style = 3)
  for(h in 1:length(search_hour)){
    for(v in 1:length(search_var)){
      # progress bar
      setTxtProgressBar(gridSearch_pb , length(search_var) * (h-1) + v)
      result_list_hv <- list()
      # subset dataset for the predictor variable combination
      modelInput_df.cv <- imputation_df %>%
        # exclude rows with missing predictor/response value
        filter(!if_any(everything() , is.na)) %>%
        # only the hour in search_hour
        select(x , y , TROPOMI_NO2 , DOY , DEM , ends_with(search_hour[h]) , spatial_CV) %>%
        # only the variables in search_var
        select(TROPOMI_NO2 ,contains(search_var[[v]]) , spatial_CV)
      # model with full training data
      rf_temp <- ranger(TROPOMI_NO2 ~ . ,
                        data = modelInput_df.cv %>% select(-spatial_CV) ,
                        importance = "impurity" ,
                        keep.inbag = TRUE , 
                        num.trees = N.trees)
      # store OOB-R2 of the model
      result_list_hv$OOB_R2 <- rf_temp$r.squared
      # store variable importance of the model
      result_list_hv$varImportance <- sort(rf_temp$variable.importance , decreasing = TRUE)
      # cross-validation model
      result_list_hv$CV_R2_k <- c()
      for(k in 1:k_fold){
        rf_temp_cv <- ranger(TROPOMI_NO2 ~ . ,
                             data = modelInput_df.cv %>% 
                               filter(spatial_CV != as.character(k)) %>% 
                               select(-spatial_CV) ,
                             importance = "impurity" ,
                             keep.inbag = TRUE , num.trees = N.trees)
        rf_temp_cv_pred <- predict(rf_temp_cv , 
                                   data = modelInput_df.cv %>% 
                                     filter(spatial_CV == as.character(k)) %>% 
                                     select(-spatial_CV))
        rf_temp_cv_obs_pred <- modelInput_df.cv %>% 
          filter(spatial_CV == as.character(k)) %>% 
          select(TROPOMI_NO2) %>% 
          rename(obs = TROPOMI_NO2) %>% 
          mutate(pred = rf_temp_cv_pred$predictions)
        result_list_hv$CV_R2_k[k] <- cor(rf_temp_cv_obs_pred$obs , rf_temp_cv_obs_pred$pred)^2
      }
      result_list_hv$CV_R2 <- mean(result_list_hv$CV_R2_k)
      # store results
      if(v == 1) gridSearch_hour_var[[h]] <- list()
      gridSearch_hour_var[[h]][[v]] <- result_list_hv
      rm(rf_temp , rf_temp_cv)
    }
    # set names in the list
    names(gridSearch_hour_var[[h]]) <- names(search_var)
  }
  rm(h,v,k)
  names(gridSearch_hour_var) <- paste0("HOUR_" , search_hour)
}
str(gridSearch_hour_var) 

#save(gridSearch_hour_var , file = "3_results/output-data/imputation-TROPOMI/gridSearch_hour_var_spatialGridCv.Rdata")
#load("3_results/output-data/imputation-TROPOMI/gridSearch_hour_var_spatialGridCv.Rdata")

# table: OOB-R2
gridSearch_OOB_R2 <- gridSearch_hour_var %>% 
  unlist() %>% 
  as.data.frame() %>% 
  rename(.data = . , value = `.`) %>% 
  mutate(var = rownames(.)) %>% 
  filter(grepl("OOB_R2$" , var)) %>% 
  rename(OOB_R2 = value) %>% 
  separate(col = var , into = c("Hour" , "model" , "var") , sep = "\\.") %>% 
  select(-var) %>% 
  mutate(Hour = gsub("HOUR_" , "" , Hour)) %>% 
  as_tibble()
gridSearch_OOB_R2%>% 
  pivot_wider(id_cols = Hour , names_from = model , values_from = OOB_R2)
# visualization: OOB-R2
gridSearch_OOB_R2 %>% 
  ggplot(aes(x = Hour , y = model , fill = OOB_R2)) + 
  geom_raster() +
  geom_text(aes(label = round(OOB_R2 , 3)) , color = "white") +
  coord_cartesian(expand = FALSE) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(5 , "YlGnBu")) +
  labs(x = "Hour (CAMS and ERA5)" , y = "Predictor variable set" , 
       fill = expression("OOB-R"^2))

# table: CV-R2
gridSearch_CV_R2 <- gridSearch_hour_var %>% 
  unlist() %>% 
  as.data.frame() %>% 
  rename(.data = . , value = `.`) %>% 
  mutate(var = rownames(.)) %>% 
  filter(grepl("CV_R2$" , var)) %>% 
  rename(CV_R2 = value) %>% 
  separate(col = var , into = c("Hour" , "model" , "var") , sep = "\\.") %>% 
  select(-var) %>% 
  mutate(Hour = gsub("HOUR_" , "" , Hour)) %>% 
  as_tibble()
gridSearch_CV_R2%>% 
  pivot_wider(id_cols = Hour , names_from = model , values_from = CV_R2)
# visualization: CV-R2
gridSearch_CV_R2 %>% 
  ggplot(aes(x = Hour , y = model , fill = CV_R2)) + 
  geom_raster() +
  geom_text(aes(label = round(CV_R2 , 3)) , color = "white") +
  coord_cartesian(expand = FALSE) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(7 , "BuGn")) +
  labs(x = "Hour (CAMS and ERA5)" , y = "Predictor variable set" , 
       fill = expression("CV-R"^2))

# visualize the variable importance of a particular model
gridSearch_hour_var$HOUR_12$base_meteo5$varImportance %>% 
  as.data.frame() %>% 
  rename(.data = . , importance = `.`) %>% 
  mutate(var = rownames(.)) %>% 
  # re-order variables by importance
  mutate(var = factor(var , levels = var[order(importance)])) %>% 
  # visualization
  ggplot(aes(x = var , y = importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variable" , y = "Variable importance") +
  theme_bw()


# =====================================
# model development: grid search for final model
# =====================================
modelInput_df <- imputation_df %>%
  select(x , y , DOY , TROPOMI_NO2 , DEM , ends_with("_12") , spatial_CV) %>%
  # exclude rows with missing predictor/response value
  filter(!if_any(everything() , is.na)) %>%
  # random-value predictors to filter out useless predictors
  mutate(R1 = runif(nrow(.)) ,
         R2 = runif(nrow(.)) ,
         R3 = runif(nrow(.))) 

# screening with random-value predictors 
rf_12H <- ranger(TROPOMI_NO2 ~ x + y + DOY + DEM + CAMS_NO2_12 + CAMS_NO_12 + 
                   blh_12 + t2m_12 + sp_12 + tcc_12 + tp_12 + ws_12 + wd_12 + R1 + R2 + R3 , 
                 data = modelInput_df ,
                 importance = "impurity" ,
                 keep.inbag = TRUE)
rf_12H
importance(rf_12H) %>%
  as.data.frame() %>%
  rename(.data = . , importance = `.`) %>%
  mutate(var = rownames(.)) %>%
  # re-order variables by importance
  mutate(var = factor(var , levels = var[order(importance)])) %>%
  # visualization
  ggplot(aes(x = var , y = importance)) +
  geom_bar(stat = "identity") +
  coord_flip()
rm(rf_12H)

# grid search: exclude i least important variables at a time
# initial: all predictor variables
rf_initial <- ranger(TROPOMI_NO2 ~ x + y + DOY + DEM + CAMS_NO2_12 + CAMS_NO_12 + 
                       blh_12 + t2m_12 + sp_12 + tcc_12 + tp_12 + ws_12 + wd_12  , 
                     data = modelInput_df ,
                     importance = "impurity" ,
                     keep.inbag = TRUE , 
                     num.trees = N.trees)
inputVar_ordered <- rf_initial$variable.importance %>% 
  sort(decreasing = TRUE) %>% 
  names()
rm(rf_initial)
# grid search 
{
  gridSearch_12H <- list()
  gridSearch_i <- 1:7
  N.trees <- 200
  for(i in gridSearch_i){
    gridSearch_12H[[i]] <- list()
    # exclude i-1 least importance variables (starting from initial aka excluding no variable)
    formula_rf.temp <- sprintf("TROPOMI_NO2 ~ %s" , paste(inputVar_ordered[1:(length(inputVar_ordered)-i+1)] , collapse = '+')) %>% 
      formula() # making the formula object
    # also try: no ERA-5 at all
    if(i == max(gridSearch_i)) formula_rf.temp = TROPOMI_NO2 ~ x + y + DOY + DEM + CAMS_NO2_12 + CAMS_NO_12
    gridSearch_12H[[i]]$formula <- formula_rf.temp
    # train model
    rf_temp <- ranger(formula_rf.temp , 
                      data = modelInput_df ,
                      importance = "impurity" ,
                      keep.inbag = TRUE , 
                      num.trees = N.trees)
    # store OOB-R2 result
    gridSearch_12H[[i]]$OOB_R2 <- rf_temp$r.squared
    # store importance
    gridSearch_12H[[i]]$varImportance <- rf_temp$variable.importance
    # cross validation
    gridSearch_12H[[i]]$CV_R2_k <- c()
    gridSearch_12H[[i]]$CV_slope_k <- c()
    gridSearch_12H[[i]]$CV_RMSE_k <- c()
    for(k in 1:k_fold){
      # model
      rf_temp_cv <- ranger(formula_rf.temp ,
                           data = modelInput_df %>% 
                             filter(spatial_CV != as.character(k)) ,
                           importance = "impurity" ,
                           keep.inbag = TRUE)
      # prediction on test set
      rf_temp_cv_pred <- predict(rf_temp_cv , 
                                 data = modelInput_df %>% 
                                   filter(spatial_CV == as.character(k)) )
      rf_temp_cv_obs_pred <- modelInput_df %>% 
        filter(spatial_CV == as.character(k)) %>% 
        select(TROPOMI_NO2) %>% 
        rename(obs = TROPOMI_NO2) %>% 
        mutate(pred = rf_temp_cv_pred$predictions)
      # calculate CV-R2
      gridSearch_12H[[i]]$CV_R2_k[k] <- cor(rf_temp_cv_obs_pred$obs , rf_temp_cv_obs_pred$pred)^2
      # calculate CV-slope
      gridSearch_12H[[i]]$CV_slope_k[k] <- lm(obs ~ pred , data = rf_temp_cv_obs_pred)$coefficients[2]
      # calculate CV-RMSE
      gridSearch_12H[[i]]$CV_RMSE_k[k] <- rf_temp_cv_obs_pred %>% 
        summarize(RMSE = sqrt(sum(((pred-obs)^2)/n())) ) %>% 
        unlist
      # clean
      rm(rf_temp_cv , rf_temp_cv_pred , rf_temp_cv_obs_pred)
    }
    # result: mean CV-R2, mean slope, mean RMSE
    gridSearch_12H[[i]]$CV_R2 <- mean(gridSearch_12H[[i]]$CV_R2_k)
    gridSearch_12H[[i]]$CV_slope <- mean(gridSearch_12H[[i]]$CV_slope_k)
    gridSearch_12H[[i]]$CV_RMSE <- mean(gridSearch_12H[[i]]$CV_RMSE_k)
    # clean
    rm(formula_rf.temp , rf_temp)
  }
  rm(i,k)
  names(gridSearch_12H) <- paste0("subset" , gridSearch_i -1)
} 

#save(gridSearch_12H , file = "3_results/output-data/imputation-TROPOMI/gridSearch_12H.RData")

# visaulization of grid search result: OOB- and CV-R2
data.frame(
  subset = gridSearch_i-1 , 
  OOB_R2 = sapply(gridSearch_12H , function(mylist)mylist$OOB_R2 ) , 
  CV_R2 = sapply(gridSearch_12H , function(mylist)mylist$CV_R2 )
) %>% 
  pivot_longer(cols = c(OOB_R2 , CV_R2) , names_to = "var") %>% 
  ggplot(aes(x = subset , y = value , color = var) ) +
  geom_point() +
  geom_line() +
  scale_color_discrete(labels = c(CV_R2 = "Spatially-blocked cross validation" , OOB_R2 = "Out-of-bag")) +
  labs(x = "# variables excluded" , y = expression("R"^2) , 
       color = "Test set") +
  theme_bw() +
  theme(legend.position = "top")
# visaulization of grid search result: CV-RMSE
data.frame(
  subset = gridSearch_i-1 , 
  CV_RMSE = sapply(gridSearch_12H , function(mylist)mylist$CV_RMSE )
) %>% 
  ggplot(aes(x = subset , y = CV_RMSE)) +
  geom_point() +
  geom_line() +
  labs(x = "# variables excluded" , y = "CV-RMSE") +
  theme_bw()


# =====================================
# final model
# =====================================
formula_rf_final <- TROPOMI_NO2 ~ DEM + sp_12 + CAMS_NO_12 + CAMS_NO2_12 + y + x + DOY + t2m_12 + blh_12 + ws_12 + wd_12 + tcc_12
modelInput_df <- imputation_df %>%
  select(x , y , DOY , TROPOMI_NO2 , DEM , ends_with("_12") , spatial_CV) %>%
  # exclude rows with missing predictor/response value
  filter(!if_any(everything() , is.na)) 
rf_final <- ranger(formula_rf_final , 
                   data = modelInput_df , 
                   num.trees = 500 , 
                   mtry = 5 , 
                   importance = "impurity" ,
                   keep.inbag = TRUE)
rf_final
# after search through some hyperparameter values
# decided to use num.trees = 500 and mtry = 5

# =====================================
# evaluation
# =====================================
# OOB
rf_final$r.squared

# slope, intercept
rf_final_pred <- rf_final$predictions #OOB-predictions
rf_final_obs_pred <- modelInput_df %>% 
  select(x,y,DOY,TROPOMI_NO2) %>% 
  rename(obs = TROPOMI_NO2) %>% 
  mutate(pred = rf_final_pred ) 
summary(lm(obs ~ pred , data = rf_final_obs_pred))
rf_final_obs_pred %>% 
  ggplot(aes(x = pred , y = obs)) +
  geom_abline(slope = 1 , intercept = 0 , linetype = 2 , color = "azure4") +
  geom_point(shape = 1 , alpha = 0.5) +
  geom_smooth(method = "lm") +
  coord_fixed(1) +
  labs(x = expression("Modeled TROPOMI-NO"[2]) , y = expression("Observed TROPOMI-NO"[2])) +
  theme_bw()

# cross validation R2
rf_final_CV_R2_k <- c()
for(k in 1:k_fold){
  # model
  rf_temp_cv <- ranger(formula_rf_final ,
                       data = modelInput_df %>% 
                         filter(spatial_CV != as.character(k)) ,
                       importance = "impurity" ,
                       keep.inbag = TRUE , 
                       num.trees = rf_final$num.trees , 
                       mtry = rf_final$mtry)
  # prediction on test set
  rf_temp_cv_pred <- predict(rf_temp_cv , 
                             data = modelInput_df %>% 
                               filter(spatial_CV == as.character(k)) )
  rf_temp_cv_obs_pred <- modelInput_df %>% 
    filter(spatial_CV == as.character(k)) %>% 
    select(TROPOMI_NO2) %>% 
    rename(obs = TROPOMI_NO2) %>% 
    mutate(pred = rf_temp_cv_pred$predictions)
  # calculate CV-R2
  rf_final_CV_R2_k[k] <- cor(rf_temp_cv_obs_pred$obs , rf_temp_cv_obs_pred$pred)^2
  # clean
  rm(rf_temp_cv , rf_temp_cv_pred , rf_temp_cv_obs_pred)
}
#save(rf_final_CV_R2_k , file = "3_results/output-data/imputation-TROPOMI/rf_final_CV_R2_k.RData")
# mean CV-R2
mean(rf_final_CV_R2_k)
range(rf_final_CV_R2_k)



# //////////////////////////////////////////////////////////////////////////
# projection of imputation model result
# //////////////////////////////////////////////////////////////////////////
projected_rf <- predict(rf_final , 
                        data = imputation_df %>% 
                          filter(!is.na(DEM)) , 
                        type = "se")
projected <- imputation_df %>% 
  filter(!if_any(c(DEM,sp_12,t2m_12,blh_12,ws_12,wd_12,tcc_12) , is.na)) %>% 
  select(x,y,DOY,TROPOMI_NO2) %>% 
  # add the projected (model-predicted) OMI-NO2
  mutate(TROPOMI_NO2_pred = projected_rf$predictions , 
         TROPOMI_NO2_pred_se = projected_rf$se) %>% 
  # set negative values to 0
  mutate(TROPOMI_NO2_pred = ifelse(TROPOMI_NO2_pred < 0 , 0 , TROPOMI_NO2_pred))

projected_stars <- projected %>% 
  # convert DOY to date
  mutate(DOY = as_date(DOY , origin = "2018-12-31")) %>% 
  # convert to stars
  st_as_stars(dims = c("x" , "y" , "DOY")) %>% 
  # rename dimension DOY to date
  st_set_dimensions(3 , names = "date") %>% 
  # set coordinate system
  st_set_crs(st_crs(TROPOMI_raw)) %>% 
  # de-select the attribute "DOY"
  select(-DOY) %>% 
  # imputed: observed + predicted
  mutate(TROPOMI_NO2_imputed = ifelse(is.na(TROPOMI_NO2) , TROPOMI_NO2_pred , TROPOMI_NO2))

# save(projected_stars , file = "3_results/output-data/imputation-TROPOMI/projected_stars.RData")

# visualization
selected_dates <- interval("2019-02-01" , "2019-02-10")
ggplot() +
  geom_stars(
    data = projected_stars %>% 
      filter(date %within% selected_dates) %>% 
      merge() %>% 
      st_set_dimensions(4 , values = c("observed" , "predicted" , "se" , "imputed")) 
  ) +
  geom_sf(data = CH , fill = NA , color = "white") +
  scale_fill_viridis_c(limits = c(0,1e16) , oob = scales::squish) +
  coord_sf(expand = FALSE) +
  facet_grid(attributes~date) +
  labs(x = "Longtitude" , y = "Latitude" , fill = "TropColumnNO2") +
  theme(axis.text = element_text(size = 6))
# visualization with animation
annimation_obs_pred <- ggplot() +
  geom_stars(
    data = projected_stars %>%
      select(TROPOMI_NO2 , TROPOMI_NO2_pred , TROPOMI_NO2_imputed) %>%
      merge() %>%
      st_set_dimensions(4 , values = c("observed" , "predicted" , "imputed"))
  ) +
  geom_sf(data = CH , fill = NA , color = "white") +
  scale_fill_viridis_c(limits = c(0,1e16) , oob = scales::squish) +
  coord_sf(expand = FALSE) +
  facet_grid(~attributes) +
  transition_time(date) +
  labs(x = "Longtitude" , y = "Latitude" , fill = "TropColumnNO2" ,
       title = "Date: {frame_time}") +
  theme(axis.text = element_text(size = 6))
animate(annimation_obs_pred , duration = 365 , fps = 1)
anim_save(filename = "3_results/output-data/imputation-TROPOMI/obs_pred_imputed.gif")


# spatial-temporal patterns of residuals
projected_stars %>% 
  mutate(residual = TROPOMI_NO2 - TROPOMI_NO2_pred) %>% 
  aggregate(by = "1 month" , FUN = "mean" , na.rm = TRUE) 
ggplot() +
  geom_stars(
    data = projected_stars %>% 
      mutate(residual = TROPOMI_NO2 - TROPOMI_NO2_pred) %>% 
      aggregate(by = "1 month" , FUN = "mean" , na.rm = TRUE) %>% 
      select(residual)
  ) +
  geom_sf(data = CH , fill = NA , color = "white") +
  facet_wrap(~time) +
  coord_sf(expand = FALSE) +
  scale_fill_gradient2(limits = c(-4e14 , 4e14) , 
                       low = "deeppink3" , mid = "white" , high = "dodgerblue3" , 
                       midpoint = 0) +
  labs(x = "Longitude" , y = "Latitude" , fill = "Mean residual")

#save(list = ls() , file = "3_results/output-data/imputation-TROPOMI/working-environment.RData")

# =====================================
# output imputation result
# =====================================
if(!dir.exists("1_data/processed/TROPOMI_imputed")) dir.create("1_data/processed/TROPOMI_imputed")
projected_stars %>% 
  select(TROPOMI_NO2_imputed) %>% 
  write_stars(dsn = "1_data/processed/TROPOMI_imputed/S5P_OFFL_L2__NO2____2019_daily_imputed_AOI.nc")

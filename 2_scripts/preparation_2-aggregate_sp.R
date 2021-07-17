#####################################################################################################
# Masterarbeit
# Prepare training data (2/3) : spatial aggregation (moving window summation)
# 2021-05-27
#####################################################################################################
# coordinate system to work with: EPSG: 2056 (CH1903+/ LV95)

# =====================================
# required packages
# =====================================
library(stars) ; library(sf) ; library(raster)
library(dplyr) ; library(tidyr) 
library(ggplot2) 
library(lubridate) ; library(stringr)

# //////////////////////////////////////////////////////////////////////////
# spatial aggregation for spatial datasets
# //////////////////////////////////////////////////////////////////////////
in_dirpath_cleaned_sp <- "1_data/processed/cleaned/spatial"
in_files_cleaned_sp <- list.files(in_dirpath_cleaned_sp , full.names = TRUE , pattern = ".tif$") %>% 
  # only continuous data, exlucde landcover-code
  .[!str_detect(. , "landcover-code")] %>% 
  # exclude distance-to-nearest-major road and NDVI
  .[!str_detect(. , "dist-near-major-road|NDVI_sp")] %>% 
  # do not recycle the spatially-aggregated products
  .[!str_detect(. , "radius")]

focal_radius <- c(100, 200, 500, 1000, 2000, 5000, 10000)

for(sp_file in in_files_cleaned_sp){
  raster_temp <- stack(sp_file) 
  for(d in focal_radius){
    for(band in 1:dim(raster_temp)[3]){
      # Calculate focal ("moving window") values for the neighborhood of focal cells using a matrix of weights
      raster_focal_band_temp <- focal(raster_temp[[band]] , 
                                      w = focalWeight(raster_temp , d = d , type = "circle") , 
                                      fun = "mean" , na.rm = TRUE)
      # rename the object `raster_focal_{band}_temp`
      assign(sprintf("raster_focal_%s_temp" , band) , raster_focal_band_temp)
      # clean
      rm(raster_focal_band_temp)
      gc() ; gc()
    } # end of band loop
    # stack the bands
    mget(ls(pattern = "raster_focal_\\d+_temp")) %>% 
      stack() %>% 
      setNames(names(raster_temp)) %>% 
      # export to folder
      st_as_stars() %>%  # use stars to export the stacked .tif file, because layer names can be preserved
      write_stars(dsn = str_replace(sp_file , "_100m.tif" , sprintf("_radius%s_100m.tif" , d)))
    # print progress
    cat(basename(sp_file) , "radius =" , d , "m ; written in directory" , dirname(sp_file) , "\n")
    # clean
    rm(list = ls(pattern = "raster_focal_\\d+_temp"))
    gc() ; gc()
  } # end of d loop
  # clean
  rm(raster_temp)
  gc()
  gc()
}


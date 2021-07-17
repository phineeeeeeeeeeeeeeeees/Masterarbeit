#####################################################################################################
# Masterarbeit
# Download MODIS data from NASA EarthData 
# Part 2: subsetting NDVI and clipping)
# 2021-04-28
#####################################################################################################
# This script only runs on Windows and does not run on MacOS because of GDAL HDF4 driver issue
rgdal::getGDALDriverNames() # There suppose to have HDF4 included in the drivers

# =====================================
# required packages
# =====================================
library(raster) ; library(sp)
library(rgdal) ; library(gdalUtils)
library(dplyr) ; library(stringr) ; library(lubridate)
library(pbapply)

# =====================================
# organize the files to be processed
# =====================================
in_HDFfolder <- "1_data/raw/MODIS-vegetation/raw-HDF4"
out_TIFFfolder <- "1_data/raw/MODIS-vegetation/NDVI-GTiff"
in_out_files <- list.files(in_HDFfolder , ".hdf$" , full.names = TRUE) %>% 
  as_tibble() %>% 
  rename(in_filepath = value) %>% 
  mutate(date = basename(in_filepath) %>% 
           str_extract("\\.A\\d{7}\\.") %>% 
           str_extract("\\d{7}") %>% 
           as_date(format = "%Y%j")) %>% 
  mutate(date_string = as.character(date) %>% str_replace_all("-" , "")) %>% 
  mutate(out_filepath = paste0(out_TIFFfolder , "/MYD13Q1_NDVI_" , date_string , "_AOI.tiff"))

if(!dir.exists(out_TIFFfolder)) dir.create(out_TIFFfolder)

# =====================================
# subset only NDVI from the other indices and clip to AOI
# =====================================
# AOI
AOI <- readOGR("1_data/raw/Switzerland_shapefile" , "AOI_4326")
# automatic batch process through the files
pbapply(
  in_out_files , 
  MARGIN = 1 , 
  function(filelist_df){
    # load the sub-datasets
    sds <- gdalUtils::get_subdatasets(filelist_df["in_filepath"])
    # open the first variable: 16 days NDVI
    NDVI_raster <- raster(sds[1])
    # clip to AOI
    AOI_proj <- spTransform(AOI , proj4string(NDVI_raster))
    NDVI_AOI <- mask(NDVI_raster , AOI_proj)
    # save the clipped raster image
    writeRaster(NDVI_AOI , 
                filelist_df["out_filepath"] , 
                format = "GTiff")
    rm(sds , NDVI_raster , NDVI_AOI)
  }
)


# filelist_df <- in_out_files[1,]
# # load the sub-datasets
# sds <- gdalUtils::get_subdatasets(filelist_df["in_filepath"])
# # open the first variable: 16 days NDVI
# NDVI_raster <- raster(sds[1])
# # clip to AOI
# AOI_proj <- spTransform(AOI , proj4string(NDVI_raster))
# NDVI_AOI <- mask(NDVI_raster , AOI_proj)
# # save the clipped raster image
# writeRaster(NDVI_AOI , 
#             filelist_df["out_filepath"] , 
#             format = "GTiff")

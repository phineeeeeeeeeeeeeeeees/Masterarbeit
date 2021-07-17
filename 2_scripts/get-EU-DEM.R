#####################################################################################################
# Masterarbeit
# Download EU-DEM elevation data
# 2021-04-19
# https://land.copernicus.eu/imagery-in-situ/eu-dem/eu-dem-v1.1?tab=download 
#####################################################################################################
#library(stars) ; library(sf)
library(raster) ; library(rgdal) ; library(sp)

if(!dir.exists("1_data/raw/EU-DEM-1.1")) dir.create("1_data/raw/EU-DEM-1.1")

# =====================================
# download data
# =====================================
# the data is downloaded from https://land.copernicus.eu/imagery-in-situ/eu-dem/eu-dem-v1.1?tab=download
data_url <- "https://land.copernicus.eu/land-files/97824c12f357f50638d665b5a58707cd82857d57.zip"
filename_zip <- paste0("1_data/raw/EU-DEM-1.1/" , basename(data_url))
download.file(data_url, 
              filename_zip)
# unzip
unzip(filename_zip , 
      exdir = "1_data/raw/EU-DEM-1.1") 
filename_unzipped <- unzip("1_data/raw/EU-DEM-1.1/eu_dem_v11_E40N20.zip" ,  # 第一層解壓縮完還是一個.zip
                           exdir = "1_data/raw/EU-DEM-1.1")
# remove the raw .zip file
#file.remove(list.files("1_data/raw/EU-DEM-1.1" , ".zip$" , full.names = TRUE))

# =====================================
# clip to AOI
# =====================================
# read the image
DEM_raw <- raster("1_data/raw/EU-DEM-1.1/eu_dem_v11_E40N20/eu_dem_v11_E40N20.TIF")
# AOI
AOI <- readOGR("1_data/raw/Switzerland_shapefile" , "AOI_4326")
AOI_proj <- spTransform(AOI , proj4string(DEM_raw))
# clipping 
DEM_AOI <- mask(DEM_raw , AOI_proj)
# save the clipped image
crs(DEM_AOI)
writeRaster(DEM_AOI , 
            "1_data/raw/EU-DEM-1.1/eu_dem_v11_E40N20_AOI.tif" , 
            format = "GTiff")

# # read the image
# DEM_raw <- read_stars("1_data/raw/EU-DEM-1.1/eu_dem_v11_E40N20/eu_dem_v11_E40N20.TIF" , proxy = FALSE)
# # AOI
# AOI <- st_read("1_data/raw/Switzerland_shapefile" , "AOI_4326" , quiet = TRUE) %>% 
#   st_transform(st_crs(DEM_raw))
# # clipping
# DEM_AOI <- DEM_raw[AOI]
# # save the clipped image
# write_stars(DEM_AOI , 
#             "1_data/raw/EU-DEM-1.1/eu_dem_v11_E40N20_AOI.tif")

# remove the original file
file.remove(filename_zip) # .zip file
unlink("1_data/raw/EU-DEM-1.1/eu_dem_v11_E40N20" , recursive = TRUE)  # decompressed folder



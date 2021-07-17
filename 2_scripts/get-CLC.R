#####################################################################################################
# Masterarbeit
# Download CORINE land cover 2018
# 2021-04-19
# https://land.copernicus.eu/pan-european/corine-land-cover/clc2018?tab=download 
#####################################################################################################
library(stars) ; library(sf)

if(!dir.exists("1_data/raw/CORINE-land-cover")) dir.create("1_data/raw/CORINE-land-cover")

# =====================================
# download data
# =====================================
# the data is downloaded from https://land.copernicus.eu/pan-european/corine-land-cover/clc2018?tab=download 
data_url <- "https://land.copernicus.eu/land-files/83684d24c50f069b613e0dc8e12529b893dc172f.zip"
filename_zip <- paste0("1_data/raw/CORINE-land-cover/" , basename(data_url))
download.file(data_url, filename_zip)
# unzip
filename_unzipped <- unzip(filename_zip , exdir = "1_data/raw/CORINE-land-cover") %>% 
  unzip(exdir = "1_data/raw/CORINE-land-cover")
# 解壓縮出來的資料夾移動到1_data/CORINE-land-cover
filename_unzipped %>% 
  gsub("1_data/raw/CORINE-land-cover/u2018_clc2018_v2020_20u1_raster100m" , 
       "1_data/raw/CORINE-land-cover" , x = .) %>% 
  dirname() %>% 
  unique() %>% 
  sapply(. , function(x)if(!dir.exists(x))dir.create(x , recursive = TRUE))
file.rename(filename_unzipped , 
            gsub("1_data/raw/CORINE-land-cover/u2018_clc2018_v2020_20u1_raster100m/" , "1_data/raw/CORINE-land-cover/" , filename_unzipped))
# remove .zip
file.remove(list.files("1_data/raw/CORINE-land-cover" , ".zip$" , full.names = TRUE))
# remove unzip folder
unlink("1_data/raw/CORINE-land-cover/u2018_clc2018_v2020_20u1_raster100m" , recursive = TRUE)

rm(data_url , filename_unzipped , filename_zip)

# =====================================
# clip to AOI
# =====================================
# read the image
CLC_raw <- read_stars("1_data/raw/CORINE-land-cover/DATA/U2018_CLC2018_V2020_20u1.tif")
# AOI
AOI <- st_read("1_data/raw/Switzerland_shapefile" , "AOI_4326" , quiet = TRUE) %>% 
  st_transform(st_crs(CLC_raw))
# clipping
CLC_AOI <- CLC_raw[AOI]
# save the clipped image
write_stars(CLC_AOI , 
            "1_data/raw/CORINE-land-cover/U2018_CLC2018_V2020_20u1_AOI.tif")

# remove the raw .tif file
file.remove("1_data/raw/CORINE-land-cover/DATA/U2018_CLC2018_V2020_20u1.tif")

rm(AOI , CLC_AOI , CLC_raw)


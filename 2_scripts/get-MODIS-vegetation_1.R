#####################################################################################################
# Masterarbeit
# Download MODIS data from NASA EarthData 
# 2021-03-25
# data source site: https://search.earthdata.nasa.gov/search?q=C194001221-LPDAAC_ECS&_ga=2.235454977.1647628824.1618299700-1560109989.1617441372 
#####################################################################################################
# The Aqua Moderate Resolution Imaging Spectroradiometer (MODIS) Vegetation Indices (MYD13Q1) Version 6 data are generated 
# every 16 days at 250 meter (m) spatial resolution as a Level 3 product. 

# The MYD13Q1 product provides two primary vegetation layers. 
# The first is the Normalized Difference Vegetation Index (NDVI) which is referred to as the continuity index to the existing National Oceanic and Atmospheric Administration-Advanced Very High Resolution Radiometer (NOAA-AVHRR) derived NDVI. 
# The second vegetation layer is the Enhanced Vegetation Index (EVI), which has improved sensitivity over high biomass regions. 
# The algorithm chooses the best available pixel value from all the acquisitions from the 16 day period. 
# The criteria used is low clouds, low view angle, and the highest NDVI/EVI value.
# more description: https://lpdaac.usgs.gov/products/myd13q1v006/

# =====================================
# required packages
# =====================================
library(httr)
library(dplyr) ; library(stringr) ; library(lubridate)
library(pbapply)

if(!dir.exists("1_data/raw/MODIS-vegetation")) dir.create("1_data/raw/MODIS-vegetation")

# =====================================
# set up Login Authentication through termninal
# =====================================
# creating .netrc file for the login information
source("2_scripts/login-authentication/login-NASA-earthdata_MODIS.R")

# =====================================
# Downloading data
# =====================================
# for user credentials and login authorization
netrc_path <- "1_data/raw/MODIS-vegetation/.netrc"
cookie_path <- "1_data/raw/MODIS-vegetation/.urs_cookies"
GET_config <- config(followlocation=1,
                     netrc=1,
                     netrc_file=netrc_path,
                     cookie=cookie_path,
                     cookiefile=cookie_path,
                     cookiejar=cookie_path)

# the list of files to download from 
# this list is aquired from https://search.earthdata.nasa.gov/search?q=C194001221-LPDAAC_ECS&_ga=2.235454977.1647628824.1618299700-1560109989.1617441372 
query_result <- read.table("1_data/raw/MODIS-vegetation/query_result.txt" , stringsAsFactors = FALSE , col.names = "url") %>% 
  # 自動由網址擷取檔名與日期 以便後續下載時重新命名
  mutate(filename = basename(url)) %>% 
  mutate(date = dirname(url) %>%  strsplit("/") %>% lapply(tail,1) %>% unlist)

if(!dir.exists("1_data/raw/MODIS-vegetation/raw-HDF4")) dir.create("1_data/raw/MODIS-vegetation/raw-HDF4")

# automatic download through the download file list
pbapply(
  query_result , 
  MARGIN = 1 ,            # apply to every row of the data frame 
  function(filelist_df){
    # download
    httr::GET(url = filelist_df["url"] , config = GET_config , 
              write_disk(paste0("1_data/raw/MODIS-vegetation/raw-HDF4" , filelist_df["filename"]) , 
                         overwrite = TRUE))
  }
)

# The downloaded HDF4 files cannot be opened with GDAL on MacOS
# The following subsetting and clipping will be done on Windows in "get-MODIS-vegetation_2.R"



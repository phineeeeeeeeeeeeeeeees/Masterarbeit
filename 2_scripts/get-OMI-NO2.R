#####################################################################################################
# Masterarbeit
# Download OMI-NO2 data from NASA EarthData 
# 2021-03-25
# data source site: https://disc.gsfc.nasa.gov/datasets/OMI_MINDS_NO2d_1/summary 
#####################################################################################################
# OMI/Aura NO2 Tropospheric, Stratospheric & Total Columns MINDS Daily L3 Global Gridded 0.25 degree x 0.25 degree 
# (OMI_MINDS_NO2d)

# =====================================
# required packages
# =====================================
library(httr)
library(dplyr) ; library(stringr) ; library(lubridate)
library(pbapply)
library(stars) ; library(sf)

if(!dir.exists("1_data/raw/OMI-NO2")) dir.create("1_data/raw/OMI-NO2")

# =====================================
# set up Login Authentication through termninal
# =====================================
# creating .netrc file for the login information
source("2_scripts/login-authentication/login_NASA-earthdata_OMI.R")

# =====================================
# Downloading data
# =====================================
# for user credentials and login authorization
netrc_path <- "1_data/raw/OMI-NO2/.netrc"
cookie_path <- "1_data/raw/OMI-NO2/.urs_cookies"
GET_config <- config(followlocation=1,
                     netrc=1,
                     netrc_file=netrc_path,
                     cookie=cookie_path,
                     cookiefile=cookie_path,
                     cookiejar=cookie_path)

# the list of files to download from 
# this list is aquired from https://disc.gsfc.nasa.gov/datasets/OMI_MINDS_NO2d_1/summary
query_result <- read.table("1_data/raw/OMI-NO2/subset_OMI_MINDS_NO2d_1_20210419_074014.txt" , stringsAsFactors = FALSE , col.names = "url") %>% 
  # 自動由網址擷取檔名 以便後續下載時重新命名
  mutate(filename = basename(url)) %>% 
  # 自動由網址擷取檔案日期
  mutate(date = str_extract(filename , "2019m\\d{4}") %>% 
           str_replace("m" , ""))
write.csv(query_result , "1_data/raw/OMI-NO2/query_result_raw.csv" , row.names = FALSE)

# automatic download through the download file list
AOI <- st_read("1_data/raw/Switzerland_shapefile" , "AOI_4326" , quiet = TRUE)
pbapply(
  query_result , 
  MARGIN = 1 ,            # apply to every row of the data frame 
  function(filelist_df){
    # download
    httr::GET(url = filelist_df["url"] , config = GET_config , 
              write_disk(paste0("1_data/raw/OMI-NO2/" , filelist_df["filename"]) , overwrite = TRUE))
    # clip the global-scale dataset to AOI
    if(tools::file_ext(filelist_df["filename"]) == "nc"){    # only for the .nc files
      # read temporary raster image
      tempImg <- stars::read_ncdf(paste0("1_data/raw/OMI-NO2/" , filelist_df["filename"]) , 
                                  var = "ColumnAmountNO2TropCloudScreened")
      # clip and output
      tempImg[st_transform(AOI , st_crs(tempImg))] %>% 
        write_stars(paste0("1_data/raw/OMI-NO2/" , 
                           "OMI-Aura_L3-OMI_MINDS_NO2d_" , filelist_df["date"] , "_AOI" , ".tif"))
      # remove temporary raster image file
      file.remove(paste0("1_data/raw/OMI-NO2/" , filelist_df["filename"]))
    }
  }
)

rm(AOI, GET_config , query_result , cookie_path , netrc_path)



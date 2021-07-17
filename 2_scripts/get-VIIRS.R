#####################################################################################################
# Masterarbeit
# Download nighttime ligh data (VIIRS)
# 2021-04-13
# https://eogdata.mines.edu/products/vnl/ 
#####################################################################################################

# =====================================
# required packages
# =====================================
library(httr)
library(jsonlite)
library(utils)
library(stars) ; library(sf) ; library(ggplot2)

# =====================================
# Retrieve access token
# reference: https://payneinstitute.mines.edu/eog-2/transition-to-secured-data-access/ 
# =====================================
# set a list "params" with client_id, client_secret, username, password, grant_type
source("2_scripts/login-authentication/login_EOG_VIIRS.R")

token_url <- 'https://eogauth.mines.edu/auth/realms/master/protocol/openid-connect/token'
response <- POST(token_url, body = params, encode = "form")
access_token_list <- fromJSON(content(response , as="text" , encoding="UTF-8"))
access_token <- access_token_list$access_token
# Submit request with token bearer and write to output file


# =====================================
# download (2019 annual)
# =====================================
data_url <- "https://eogdata.mines.edu/nighttime_light/annual/v20/2019/VNL_v2_npp_2019_global_vcmslcfg_c202101211500.average.tif.gz"
auth <- paste('Bearer', access_token)
download.file(data_url ,
              paste0("1_data/raw/VIIRS-nighttime-light/" , basename(data_url)) , 
              mode = "wb", headers = list(Authorization = auth))

# =====================================
# clipping to AOI
# =====================================
# the downloaded .tif.gz file
filename_tif.gz <- list.files("1_data/raw/VIIRS-nighttime-light" , pattern = ".gz$" , full.names = TRUE)
# decompress the .tif.gz file
filename_tif <-  R.utils::gunzip(filename = filename_tif.gz , 
                                 destname = gsub(".gz" , "" , filename_tif.gz) , 
                                 skip = TRUE , remove = FALSE)
# read the image
VIIRS_raw <- read_stars(filename_tif)
st_crs(VIIRS_raw)  # ESPG4326
# AOI
AOI <- st_read("1_data/raw/Switzerland_shapefile" , "AOI_4326" , quiet = TRUE)
# clipping
VIIRS_AOI <- VIIRS_raw[AOI]
# save the clipped image
write_stars(VIIRS_AOI , 
            gsub(".tif$" , "_AOI.tif" , filename_tif))

# remove the raw .tif.gz file
file.remove(filename_tif.gz)
# remove the raw global-scale .tif file
file.remove(filename_tif)








# # =====================================
# # download (2019 monthly)
# # =====================================
# # this list of file url is obtained manually from https://eogdata.mines.edu/nighttime_light/monthly/v10/2019/ 
# query_result <- read.table("1_data/raw/VIIRS-nighttime-light/data_url.txt" , stringsAsFactors = FALSE , col.names = "url") %>% 
#   mutate(filename = basename(url))
# auth <- paste('Bearer', access_token)
# 
# pbapply(
#   query_result[1:2,] , 
#   MARGIN = 1 ,            # apply to every row of the data frame 
#   function(filelist_df){
#     # download from internet
#     download.file(filelist_df["url"] ,
#                   paste0("1_data/raw/VIIRS-nighttime-light/" , filelist_df["filename"]) , 
#                   mode = "wb", headers = list(Authorization = auth))
#     # unzip untar()
#     # clip
#   }
# )



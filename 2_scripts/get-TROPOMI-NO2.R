#####################################################################################################
# Masterarbeit
# Download TROPOMI-NO2 data from Sentinel-5P Pre-Operations Data Hub 
# 2021-04-11
# data source site: https://s5phub.copernicus.eu/dhus/#/home  
#####################################################################################################

if(!dir.exists("1_data/raw/TROPOMI-NO2")) dir.create("1_data/raw/TROPOMI-NO2")

# =====================================
# required packages
# =====================================
#devtools::install_github("16EAGLE/getSpatialData", force=TRUE)
#devtools::install_github("MBalthasar/S5Processor")
library(getSpatialData) ; library(S5Processor)
library(raster) ; library(sp) ; library(ncdf4) ; library(rgdal)
library(sf) ; library(stars)
library(dismo)
library(geosphere)
library(rgeos)
library(ggplot2)
library(maptools)
library(dplyr) ; library(lubridate)
library(cat)



# =====================================
# set up Login Authentication through package getSpatialData
# =====================================
source("2_scripts/login-authentication/login_CopHub.R")

# =====================================
# Query parameters
# =====================================
# AOI
AOI <- st_read("1_data/raw/Switzerland_shapefile/AOI_4326.shp") %>% 
  st_geometry()
# Set AOI using Shapefile
set_aoi(AOI)
# Display AOI
view_aoi()

# time range
time_range <- c("2019-01-01", "2020-01-01") 

# specific satellite
platform <- "Sentinel-5P"


# =====================================
# Query 
# =====================================
# Query in the Sentinel Hub (takes a while)
records <- getSentinel_records(time_range = time_range, platform = platform, aoi = AOI)
# BF added - needs this column
records$download_available <- TRUE

# Filter the resulting scenes by product level and product type
records_filtered <- records[which(records$product_type == "L2__NO2___") , ] # filtered by "NO2"
plot_records(records_filtered)  # works for small queries

save(records_filtered , file = "1_data/raw/TROPOMI-NO2/query_results/records_filtered.RData")



rm(records , AOI , platform , time_range)

# =====================================
# Download datasets
# =====================================
# Define the output path where the downloaded scenes are saved
set_archive("1_data/raw/TROPOMI-NO2/temp")
# load query results (records_filtered)
load("1_data/raw/TROPOMI-NO2/query_results/records_filtered.RData")
records_filtered <- records_filtered %>% 
  arrange(as_date(date_acquisition))
downloadID_df <- records_filtered %>% 
  as_tibble() %>% 
  select(date_acquisition) %>% 
  group_by(date_acquisition) %>%               # download one day at a time
  mutate(downloadID = cur_group_id()) %>% 
  ungroup 
downloadIDs <- downloadID_df$downloadID %>% unique()

AOI_sp <- raster::shapefile("1_data/raw/Switzerland_shapefile/AOI_4326.shp") 

# login
source("2_scripts/login-authentication/login_CopHub.R")

# download for every day
for(i in 1:max(downloadIDs)){    # i in 1:max(downloadIDs)
  date_i <- records_filtered$date_acquisition[downloadID_df$downloadID == i] %>% 
    unique() %>% 
    as.character()
  #setTxtProgressBar(pb,i)
  cat("\nNow processing:" , date_i , 
      "(" , i , "/" , length(downloadIDs) , ")...............\n")
  # records of the day i
  records_i <- records_filtered[downloadID_df$downloadID == i , ]
  
  # download dataset --------------------------
  datasets <- function() {
    getSentinel_data(records = records_i)                          # filtered scenes as input variable
  }
  r <- NULL
  attempt <- 1
  while(is.null(r) && attempt <= 10){                                     # tries to download the scenes 10x before script stops
    attempt <- attempt+1
    try(
      r <- datasets()
    )
  }
  
  # write log file --------------------------
  # Specify the path in which the log file is created
  #log_con <- file("C:\\data\\Sentinel5\\Sentinel5_R_log.txt", open="a") # adjust manually
  #writes date and time to log file after successful download
  #cat("Download successful on", as.character(Sys.time()), "\n", file = log_con)
  
  # preprocessing of downloaded images --------------------------
  in_folder <- "1_data/raw/TROPOMI-NO2/temp_datasets/Sentinel-5P"	
  in_files_i <- list.files(in_folder, 
                           pattern = paste0("NO2____" , gsub("-" , "" , date_i)) , 
                           full.names = TRUE , all.files = FALSE , include.dirs = FALSE)
  S5P_process_success <- FALSE
  try({
    # NO2 # Merge all scenes of a day, retrieve a specific variable (product) and convert the unit to molecules/cm2
    S5P_mask_unit <- S5P_process(input = in_files_i, 
                                 my_res = 10000 ,              # resolution 10000m
                                 product = 6 ,                 # product 6 = nitrogendioxide_tropospheric_column (troposphere mole content of NO2)
                                 my_aoi = AOI_sp ,
                                 extent_only = FALSE ,
                                 apply_scale_factor = TRUE)    # conversion of mol/m2 to molecules/cm2
    # cloud filter # Merge all scenes of a day, retrieve a specific variable (product)
    S5P_mask_unit_cloud <- S5P_process(input = in_files_i , 
                                       my_res = 10000 ,
                                       product = 5 ,     #product 5 = qa_value (< 0.5 is cloud)
                                       my_aoi = AOI_sp ,
                                       extent_only = FALSE)
    S5P_process_success <- TRUE
  })
  # preprocessing and saving
  out_folder <- "1_data/raw/TROPOMI-NO2/preprocessed"
  if(!dir.exists(out_folder)) dir.create(out_folder)
  out_filename <- paste0("S5P_OFFL_L2__NO2____" , gsub("-" , "" , date_i) , "_AOI")
  if(S5P_process_success){ # if the `S5P_process` ran successfully
    # extract clouds
    S5P_NO2_final <- S5P_mask_unit
    S5P_NO2_final[S5P_mask_unit_cloud < 0.5] <- NA    # NA as NoData value
    # rough cropping
    S5P_NO2_final_crop <- crop(S5P_NO2_final, AOI_sp)
    # output raster band 1: NO2_cloudScreened_AOI; band 2: QA value
    out_raster <- stack(c(NO2_cloudScreened_AOI = S5P_NO2_final_crop , 
                          QA_value = S5P_mask_unit_cloud))
    # save raster as TIFF
    writeRaster(out_raster , 
                filename = paste0(out_folder, "/" , out_filename, ".tif") , 
                format = "GTiff" , overwrite = TRUE)
    cat("\nProcessed and saved:" , date_i , 
        "(" , i , "/" , length(downloadIDs) , ")...............\n" , 
        paste0(out_folder, "/" , out_filename, ".tif") , "\n")
  }else{ # if the `S5P_process` was interupted (Error in na.fail.default(list(lon = c(-111.910614013672, -112.378707885742,  : missing values in object)
    # use a previous image as a template and assign every pixel values to NA
    out_raster <- read_stars(list.files(out_folder , ".tif$" , full.names = TRUE)[1]) %>% 
      transmute(value = NA) 
    # save NA raster as TIFF
    write_stars(out_raster , 
                dsn = paste0(out_folder, "/" , out_filename, ".tif"))
    cat("\nProcessed and saved:" , date_i , 
        "(" , i , "/" , length(downloadIDs) , ")...............\n" , 
        paste0(out_folder, "/" , out_filename, ".tif") , "\n")
    warning(paste("NA values for" , date_i , "; \nnevertheless an emply NA raster witten as" , out_folder, "/" , out_filename, ".tif"))
  }
  
  # delete all NetDCFs after creating the TIFF files   --------------------------
  file.remove(in_files_i)
  
  rm(date_i , records_i , r , attempt , in_files_i , 
     S5P_mask_unit , S5P_mask_unit_cloud , S5P_NO2_final , S5P_NO2_final_crop , out_raster , out_filename)
  gc() # garbage collection; free memory space
  gc()
}

# errors occurred: 
# i = 86 (2019-03-27)

# =====================================
# summary of the downloaded data
# =====================================
downloaded_df <- list.files("1_data/raw/TROPOMI-NO2/preprocessed" , ".tif$") %>% 
  stringr::str_extract("\\d{8}") %>% 
  as_date() %>% as.character() %>% 
  as_tibble() %>% 
  rename(date_acquisition = value) %>% 
  mutate(downloaded = TRUE) %>% 
  full_join(downloadID_df)
# check whether data of all dates are downloaded
anyNA(downloaded_df$downloaded)
# downloaded
downloaded_df$downloadID[downloaded_df$downloaded] %>% unique()  # all data of the query result are downloaded

# =====================================
# check if data of every day 
# =====================================
missing_date <- setdiff(
  seq(as_date("2019-01-01") , as_date("2019-12-31") , 1) ,   # full time frame
  downloaded_df %>% 
    select(date_acquisition) %>% unlist() %>% unique() %>%   # downloaded
    as_date() 
) %>% 
  as_date() %>% 
  as.character() # 2019-04-15 is missing

# create a empty raster with only NA
for(k in missing_date){
  list.files("1_data/raw/TROPOMI-NO2/preprocessed" , "_AOI.tif$" , full.names = TRUE)[1] %>%  # use the first image as the template
    read_stars() %>%    # template raster
    split("band") %>% 
    mutate(X1 = NA_real_ ,  # all NA values
           X2 = NA_real_) %>% 
    merge() %>% 
    st_set_dimensions(3 , names = "band") %>% 
    write_stars( dsn = sprintf("1_data/raw/TROPOMI-NO2/preprocessed/S5P_OFFL_L2__NO2____%s_AOI.tif" , gsub("-" , "" , k)) ) # output
}

# =====================================
# clean directory
# =====================================
# remove temp folder
unlink("1_data/raw/TROPOMI-NO2/temp" , recursive = TRUE)
unlink("1_data/raw/TROPOMI-NO2/temp_datasets" , recursive = TRUE)


# =====================================
# the spatial dimension of each image varies slightly (because of data availability)
# --> resample to the same X,Y dimensions
# =====================================
# file paths
files_TROPOMI <- list.files("1_data/raw/TROPOMI-NO2/preprocessed" , "_AOI.tif$" , full.names = TRUE)
# read every images separately and put together as a list
TROPOMI_raw_list <- lapply(files_TROPOMI , 
                           function(file_path){
                             read_stars(file_path)
                           }) 
# a table of the dimensions of the respective images
dim_raw_df <- lapply(TROPOMI_raw_list , dim) %>% 
  unlist %>% matrix(ncol = 3 , byrow = TRUE) %>% 
  as.data.frame() %>% 
  rename(x = V1 , y = V2 , band = V3)
distinct(dim_raw_df) # errrrr! messy dimensions! 

# resample to the template (using the first image with44*32)
TROPOMI_rs_list <- lapply(TROPOMI_raw_list , 
                          st_warp , dest = TROPOMI_raw_list[[1]]) # template
dim_rs_df <- lapply(TROPOMI_rs_list , dim) %>% 
  unlist %>% matrix(ncol = 3 , byrow = TRUE) %>% 
  as.data.frame() %>% 
  rename(x = V1 , y = V2 , band = V3)
distinct(dim_rs_df) # now every image is with the same dimension of 44*32

# export resampled images
out_folder_rs <- "1_data/raw/TROPOMI-NO2/preprocessed_resampled"
if(!dir.exists(out_folder_rs)) dir.create(out_folder_rs)
pbapply::pblapply(
  TROPOMI_rs_list , 
  function(Img){
    write_stars(Img , 
                dsn = paste0(out_folder_rs , "/" , gsub(".tif" , "_rs.tif" , names(Img))))
  }
)




#####################################################################################################
# Masterarbeit
# Download population data (GHS population grid)
# 2021-04-13
# https://data.europa.eu/euodp/en/data/dataset/jrc-ghsl-ghs_pop_eurostat_europe_r2016a 
#####################################################################################################
# Global Human Settlement Layer (GHSL) project
# GHS population grid, derived from EUROSTAT census data (2011) and ESM R2016 
# This spatial raster dataset depicts the distribution and density of residential population, 
# expressed as the number of people per cell. 
# Resident population from censuses for year 2011 provided by Eurostat were disaggregated from source zones 
# to grid cells, informed by land use and land cover from Corine Land Cover Refined 2006 
# and by the distribution and density of built-up as mapped in the European Settlement Map 2016 layer.

if(!dir.exists("1_data/raw/GHS-population")) dir.create("1_data/raw/GHS-population")

# =====================================
# download data
# =====================================
# the data is downloaded from https://data.europa.eu/euodp/en/data/dataset/jrc-ghsl-ghs_pop_eurostat_europe_r2016a
download.file("http://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_EUROSTAT_EUROPE_R2016A/GHS_POP_SOURCE_EUROPE_R2016A_3035_100/V1-0/GHS_POP_SOURCE_EUROPE_R2016A_3035_100_v1_0.zip" , 
              "1_data/raw/GHS-population/GHS_POP_SOURCE_EUROPE_R2016A_3035_100.zip")
# unzip the file
unzipped_files <- unzip("1_data/raw/GHS-population/GHS_POP_SOURCE_EUROPE_R2016A_3035_100.zip" , 
                        exdir = "1_data/raw/GHS-population" , overwrite = TRUE)
unzipped_files_moved <- gsub("1_data/raw/GHS-population/GHS_POP_SOURCE_EUROPE_R2016A_3035_100_v1_0" , 
                             "1_data/raw/GHS-population" , 
                             unzipped_files)
# 解壓縮出來的資料夾移動到1_data/raw/GHS-population
unzipped_files_moved %>% 
  dirname() %>% 
  unique() %>% 
  sapply(. , function(x)if(!dir.exists(x))dir.create(x , recursive = TRUE))
file.rename(unzipped_files , 
            gsub("1_data/raw/GHS-population/GHS_POP_SOURCE_EUROPE_R2016A_3035_100_v1_0" , "1_data/raw/GHS-population" , unzipped_files))
# remove .zip
file.remove("1_data/raw/GHS-population/GHS_POP_SOURCE_EUROPE_R2016A_3035_100.zip")
# remove unzip folder
unlink("1_data/raw/GHS-population/GHS_POP_SOURCE_EUROPE_R2016A_3035_100_v1_0" , recursive = TRUE)


# =====================================
# clip to AOI
# =====================================
library(stars) ; library(sf)

# read the image
GHS_raw <- read_stars("1_data/raw/GHS-population/GHS_POP_SOURCE_EUROPE_R2016A_3035_100_v1_0.tif")
# AOI
AOI <- st_read("1_data/raw/Switzerland_shapefile" , "AOI_4326" , quiet = TRUE) %>% 
  st_transform(st_crs(GHS_raw))
# clipping
GHS_AOI <- GHS_raw[AOI]
# save the clipped image
write_stars(GHS_AOI , 
            "1_data/raw/GHS-population/GHS_POP_SOURCE_EUROPE_R2016A_3035_100_v1_0_AOI.tif")

# remove the raw .tif file
file.remove("1_data/raw/GHS-population/GHS_POP_SOURCE_EUROPE_R2016A_3035_100_v1_0.tif")

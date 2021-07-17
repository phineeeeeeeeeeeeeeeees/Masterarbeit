#####################################################################################################
# Masterarbeit
# Prepare training data (2/3) : temproal aggregation (annual and monthly mean)
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
# temporal aggregation for spatialtemporal datasets
# //////////////////////////////////////////////////////////////////////////
in_dirpath_cleaned_st <- "1_data/processed/cleaned/spatialtemporal"
in_filepath_cleaned_st <- list.files(in_dirpath_cleaned_st , 
                                     full.names = TRUE , 
                                     pattern = "daily_\\d{+}m.tif")

# =====================================
# aggregate to annual and monthly mean
# =====================================
for(st_file in in_filepath_cleaned_st){
  stars_daily_temp <- read_stars(st_file , proxy = FALSE) %>%
    st_set_dimensions(3 ,
                      values = st_get_dimension_values(. , 3) %>%
                        as_date(origin = "2018-12-31") ,
                      names = "date")
  # annual mean
  stars_daily_temp %>%
    aggregate(by = "1 year" , FUN = "mean" , na.rm = TRUE) %>%
    write_stars(dsn = str_replace(st_file , "daily" , "annual") , type = "Float64")
  # monthly mean
  stars_daily_temp %>%
    aggregate(by = "1 month" , FUN = "mean" , na.rm = TRUE) %>%
    write_stars(dsn = str_replace(st_file , "daily" , "monthly") , type = "Float64")
  # clean temporary object
  rm(stars_daily_temp) ; gc()
  cat("\n" , st_file , "processed")
}
rm(st_file)





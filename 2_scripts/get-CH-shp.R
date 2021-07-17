#####################################################################################################
# Masterarbeit
# Download Switzerland shapefile (AOI)
# 2021-04-06
#####################################################################################################

if(!dir.exists("1_data/raw/Switzerland_shapefile")) dir.create("1_data/raw/Switzerland_shapefile")

# =====================================
# download shapefile 
# =====================================
# the shapefile is downloaded from https://geodata.lib.berkeley.edu/catalog/stanford-jn788wp9910 
download.file("https://biogeo.ucdavis.edu/data/diva/adm/CHE_adm.zip" , 
              "1_data/raw/Switzerland_shapefile/CH-shapefile.zip")
# unzip the file
unzip("1_data/raw/Switzerland_shapefile/CH-shapefile.zip" , exdir = "1_data/raw/Switzerland_shapefile" , overwrite = FALSE)
# remove .zip
file.remove("1_data/raw/Switzerland_shapefile/CH-shapefile.zip")


# =====================================
# make AOI: 50km from Swiss boundary
# =====================================
library(sf) ; library(ggplot2)
CHE_adm0 <- read_sf("1_data/raw/Switzerland_shapefile" , layer = "CHE_adm0") %>% 
  st_geometry()
AOI <- CHE_adm0 %>% 
  st_transform(st_crs(21781)) %>%     # transform from WGS84 to Swiss projection to calculate buffer
  st_buffer(50000) 
ggplot() +
  geom_sf(data = AOI) +
  geom_sf(data = CHE_adm0)

st_write(AOI , dsn = "1_data/raw/Switzerland_shapefile/AOI_21781.shp")
st_write(st_transform(AOI , st_crs(4326)) , dsn = "1_data/raw/Switzerland_shapefile/AOI_4326.shp")


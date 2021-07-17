#####################################################################################################
# Masterarbeit
# Possibilities to reduce computation cost
# reduce data size by smaller research area
# 2021-06-04
#####################################################################################################
# coordinate system to work with: EPSG: 2056 (CH1903+/ LV95)

# =====================================
# required packages
# =====================================
library(stars) ; library(sf) 
library(dplyr)
library(ggplot2)


# =====================================
# load area of interest boundary
# =====================================
Swiss_adm1 <- st_read("1_data/raw/Switzerland_shapefile/CHE_adm1.shp") %>% 
  st_transform(st_crs(2056))

# AOI (national boundary + 50km buffer)
AOI <- st_read("1_data/raw/Switzerland_shapefile/AOI_4326.shp") %>% 
  st_transform(st_crs(2056))

# =====================================
# compare the area of the bounding box (proportional to data size in stars)
# =====================================
AOI_bbox <- AOI %>% 
  st_bbox() %>% st_as_sfc()
Swiss_bbox <- Swiss_adm1 %>% 
  st_bbox() %>% st_as_sfc()
ggplot() +
  geom_sf(data = AOI_bbox , fill = "dodgerblue4" , alpha = 0.9) +
  geom_sf(data = AOI , fill = NA , color = "white") +
  geom_sf(data = Swiss_bbox , fill = "dodgerblue" , alpha = 0.4) +
  geom_sf(data = Swiss_adm1 %>% summarize , fill = NA , color = "white") +
  theme_bw()
st_area(Swiss_bbox) / st_area(AOI_bbox)

# =====================================
# compare the data size in stars
# =====================================
AOI_grid100 <- AOI %>% 
  st_rasterize(crs = st_crs(2056) , dx = 100 , dy = 100) 
Swiss_grid100 <- Swiss_adm1 %>% 
  summarize() %>% 
  st_rasterize(crs = st_crs(2056) , dx = 100 , dy = 100)

lobstr::obj_size(AOI_grid100)
lobstr::obj_size(Swiss_grid100)

lobstr::obj_size(Swiss_grid100)/lobstr::obj_size(AOI_grid100)

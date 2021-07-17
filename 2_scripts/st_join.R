#####################################################################################################
# Masterarbeit
# Possibilities to reduce computation cost: by-pass the 100*100m spatial-temporal predictors 
# spatial join: 1*1km and 100*100m 
# 2021-06-04
#####################################################################################################
# coordinate system to work with: EPSG: 2056 (CH1903+/ LV95)

# =====================================
# required packages
# =====================================
library(stars) ; library(sf) 
library(dplyr) ; library(tidyr) 
library(ggplot2)
library(lubridate) ; library(stringr)
library(data.table)

# =====================================
# load Switzerland
# =====================================
# adm1
Swiss_adm1 <- st_read("1_data/raw/Switzerland_shapefile/CHE_adm1.shp") %>% 
  st_transform(st_crs(2056))

# Zürich canton as an example
ZH <- Swiss_adm1 %>% 
  filter(str_detect(NAME_1 , "Zürich"))

# =====================================
# make a toy raster
# =====================================
ZH_toy_values <- ZH %>% 
  st_make_grid(n = c(5,8)) %>% 
  st_as_sf() %>% 
  mutate(value = sample(1:10 , nrow(.) , replace = TRUE)) %>% 
  st_intersection(ZH %>% st_geometry())
# 1*1km stars
ZH_toy1000 <- ZH_toy_values %>% 
  st_rasterize(crs = st_crs(2056) , dx = 1000 , dy = 1000)
# 100*100m stars
ZH_toy100 <- ZH_toy_values %>% 
  st_rasterize(crs = st_crs(2056) , dx = 100 , dy = 100)

lobstr::obj_size(ZH_toy1000) / lobstr::obj_size(ZH_toy100)

cowplot::plot_grid(
  ggplot() +
    geom_stars(data = ZH_toy1000) +
    geom_sf(data = ZH_toy1000 %>% 
              # convert the grids to polygon to visualize the grid cells
              st_as_sf(as_points = FALSE) ,  
            color = "azure1" , fill = NA , alpha = 0.5 , size = 0.1) +
    geom_sf(data = ZH , fill = NA , color = "white") +
    scale_fill_gradientn(colors = RColorBrewer::brewer.pal(5,"GnBu")) +
    coord_sf(expand = FALSE) +
    labs(title = "Toy raster: 1*1km" , x = "" , y = "") +
    theme_bw() , 
  ggplot() +
    geom_stars(data = ZH_toy100) +
    geom_sf(data = ZH_toy100 %>% 
              # convert the grids to polygon to visualize the grid cells
              st_as_sf(as_points = FALSE) ,  
            color = "azure1" , fill = NA , alpha = 0.5 , size = 0.02) +
    geom_sf(data = ZH , fill = NA , color = "white") +
    scale_fill_gradientn(colors = RColorBrewer::brewer.pal(5,"GnBu")) +
    coord_sf(expand = FALSE) +
    labs(title = "Toy raster: 100*100m" , x = "" , y = "") +
    theme_bw() , 
  ncol = 2
)

# =====================================
# benchmark: resampling 
# =====================================
ZH_toy1000 # 48*60
ZH_toy100  # 477*595

system.time(st_warp(ZH_toy1000 , ZH_toy100)) # 0.3 seconds


# =====================================
# instead of resampling 1*1km into 100*100m and store a large raster file
# spatially join the 1*1km values to the 100*100m 
# =====================================
# spatial join with sf
system.time({
  # vectorize
  ZH_toy1000_sf <- ZH_toy1000 %>% 
    st_as_sf(as_points = FALSE , merge = FALSE) # coarse 1km as polygon
  ZH_toy100_sf <- ZH_toy100 %>% 
    st_as_sf(as_points = TRUE , merge = FALSE)  # finer 100m as points 
  # spatial join
  ZH_toy_join_sf <- st_join(ZH_toy1000_sf , ZH_toy100_sf , suffix = c("_1000", "_100"))
})
# 3 seconds

# sf objects are much larger (16-28X) than stars objects
lobstr::obj_size(ZH_toy1000_sf) / lobstr::obj_size(ZH_toy1000)
lobstr::obj_size(ZH_toy100_sf) / lobstr::obj_size(ZH_toy100)



# =====================================
# instead of resampling 1*1km into 100*100m and store a large raster file
# spatially join the 1*1km values to the 100*100m by spatial look-up table
# =====================================
# convert to data.frame
system.time({
  ZH_toy1000_df <- ZH_toy1000 %>% 
    # convert to data.frame
    as.data.frame() %>% 
    # when stars is converted to data.frame, it contains the surrounding pixels with NA values (not actually in AOI) 
    filter(!is.na(value)) # remove these NA pixels
  ZH_toy100_df <- ZH_toy100 %>% 
    # convert to data.frame
    as.data.frame() %>% 
    # when stars is converted to data.frame, it contains the surrounding pixels with NA values (not actually in AOI) 
    filter(!is.na(value)) # remove these NA pixels
})

# data.frames are just less than 2 time larger than stars objects
lobstr::obj_size(ZH_toy1000_df) / lobstr::obj_size(ZH_toy1000)
lobstr::obj_size(ZH_toy100_df) / lobstr::obj_size(ZH_toy100)

# create a spatial look-up table (create one in advance, and apply for every dataset)
# 1km to 100m
system.time({
  ZH_grid1000 <- ZH %>% 
    st_rasterize(crs = st_crs(2056) , dx = 1000 , dy = 1000) %>% 
    # create a column/attribute: the ID of each 1*1km cell
    mutate(cellID = 1:(dim(.)[1]*dim(.)[2]) %>% 
             as.factor())
  ZH_spatial.join.table <- ZH_grid1000 %>% # the 1*1km stars with cell IDs
    select(cellID) %>% 
    # resample this cell ID to 100m 
    st_warp(ZH_toy100) %>% # I know which 100m cell each 1km cell co-locates
    as.data.frame() %>% 
    arrange(cellID) %>% 
    # include the 1km-resolution x and y coordinates
    left_join(as.data.frame(ZH_grid1000) , by = "cellID" , suffix = c("_100" , "_1000")) %>% 
    select(cellID , starts_with("x") , starts_with("y"))
}) # 0.386 second (but it's an one-time calculation; can be used for every dataset)
head(ZH_spatial.join.table)

# joining
system.time(
  ZH_toy_join_df <- ZH_toy100_df %>% 
    # the look-up table
    left_join(ZH_spatial.join.table , by = c("x" = "x_100" , "y" = "y_100")) %>% 
    # the 1km dataset
    left_join(ZH_toy1000_df , by = c("x_1000" = "x" , "y_1000" = "y") , suffix = c("_100" , "_1000"))
) # 0.074 second

ZH_toy_join_stars <- ZH_toy_join_df %>% 
  select(x,y,starts_with("value")) %>% 
  st_as_stars(dim = c("x" , "y"))
ggplot() +
  geom_stars(data =  ZH_toy_join_stars %>% merge()) +
  geom_sf(data = ZH , fill = "NA" , color = "white" , size = 0.2) +
  facet_grid(~attributes) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(5,"GnBu")) +
  coord_sf(expand = FALSE) +
  theme_bw()


# using data.table
ZH_toy1000_dt <- ZH_toy1000 %>% 
  # conver to data.table
  as.data.table() %>% 
  # when stars is converted to data.frame, it contains the surrounding pixels with NA values (not actually in AOI) 
  .[!is.na(value) , ] # remove these NA pixels
ZH_toy100_dt <- ZH_toy100 %>% 
  # conver to data.table
  as.data.table() %>% 
  # when stars is converted to data.frame, it contains the surrounding pixels with NA values (not actually in AOI) 
  .[!is.na(value) , ] # remove these NA pixels

# data.tables are just less than 2 time larger than stars objects
lobstr::obj_size(ZH_toy1000_dt) / lobstr::obj_size(ZH_toy1000)
lobstr::obj_size(ZH_toy100_dt) / lobstr::obj_size(ZH_toy100)

# joining
system.time(
  ZH_toy_join_dt <- ZH_toy100_dt %>% 
    # the look-up table
    merge(ZH_spatial.join.table , 
          by.x = c("x" , "y") , by.y = c("x_100" , "y_100") , 
          all.x = TRUE) %>% 
    # the 1km dataset
    merge(ZH_toy1000_df , 
          by.x = c("x_1000" , "y_1000") , by.y = c("x" , "y") , 
          all.x = TRUE)
) # 0.173 seconds


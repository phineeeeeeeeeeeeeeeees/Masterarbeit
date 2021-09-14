library(dplyr) ; library(tidyr) ; library(sf)

Zurich <- tibble(ID = 1 , Name = "Zurich") %>% 
  mutate(
    geometry = data.frame(
      x = c(2666441 , 2666441 , 2720575 , 2720575) , 
      y = c(1258645 , 1216900 , 1216900 , 1258645)
    ) %>% 
      slice(c(1:4,1)) %>% 
      as.matrix() %>% 
      list() %>% 
      st_polygon() %>% 
      list()
  ) %>% 
  st_as_sf() %>% 
  st_set_crs(st_crs(2056))

st_write(Zurich , dsn = "1_data/processed/cleaned/data-frame/Zurich_area.shp")

library(stars)
projected <- read_stars(file.choose() , proxy = FALSE)

Zurich_df <- projected[Zurich] %>% 
  split(3) %>% 
  select(1) %>% 
  as.data.frame() %>% 
  select(x,y)

readr::write_csv(Zurich_df , "1_data/processed/cleaned/data-frame/Zurich_area.csv")


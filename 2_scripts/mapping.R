#####################################################################################################
# Masterarbeit
# Modeling
# Mapping model projection
# 2021-09-14
#####################################################################################################

# =====================================
# required packages
# =====================================
library(sf) ; library(stars)
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(ggsci) ; library(ggthemes) ; library(cowplot)
library(lubridate) ; library(stringr)

# =====================================
# load model projection maps as stars
# =====================================
# annual
projection_annual <- read_stars("3_results/output-data/model_annual/projection_annual_Zurich.tif") %>% 
  filter(!str_detect(band , "spatial")) %>% 
  st_set_dimensions(3 , names = "model" , 
                    values = factor(st_get_dimension_values(. , 3) , levels = c("SLR_OMI" , "SLR_TROPOMI" , "RF_OMI" , "RF_TROPOMI" , "XGB_OMI" , "XGB_TROPOMI" , "LGB_OMI" , "LGB_TROPOMI" , "NN_OMI" , "NN_TROPOMI")))

# monthly
projection_monthly <- read_stars(list.files("3_results/output-data/model_monthly/projection_monthly_Zurich" , pattern = ".tif$" , full.names = TRUE)) %>% 
  setNames(names(.) %>% str_remove(".tif")) %>% 
  merge() %>% 
  st_set_dimensions(3 , names = "month") %>% 
  st_set_dimensions(4 , names = "model" , 
                    values = factor(st_get_dimension_values(. , 4) , levels = c("SLR_OMI" , "SLR_TROPOMI" , "SLMER_OMI" , "SLMER_TROPOMI" , "RF_OMI" , "RF_TROPOMI" , "XGB_OMI" , "XGB_TROPOMI" , "LGB_OMI" , "LGB_TROPOMI" , "NN_OMI" , "NN_TROPOMI")))


# =====================================
# map: annual
# =====================================
colorramp_annual <- c("#2b83ba" , "#6bb0af" , "#abdda4" , "#d5eeb2" , "#ffffbf" , "#fed790" , "#fdae61" , "#ea633e" , "#d7191c")

ggplot() +
  geom_stars(data = projection_annual) +
  facet_wrap( ~ model , nrow = 2 , dir = "v") +
  coord_sf(crs = st_crs(2056) , expand = FALSE) +
  scale_fill_gradientn(colors = colorramp) +
  labs(title = expression("Model-predicted ground-level NO"[2] * " concentration") ,
       fill = expression("NO"[2] * " (µg/m"^3 * ")") , 
       x = "Longitude" , y = "Latitude") +
  theme_bw() +
  theme(axis.text = element_text(size = 10) , axis.text.x = element_text(angle = 90 , vjust = 0.5 , hjust = 1))
save_plot(
  "3_results/output-graph/model_annual/mapping.png" , 
  plot = last_plot() ,
  base_height = 8 , base_width = 13
)

# zoomed
ggplot() +
  geom_stars(data = projection_annual) +
  facet_wrap( ~ model , nrow = 2 , dir = "v") +
  coord_sf(crs = st_crs(2056) , expand = FALSE , xlim = c(2674951 , 2685258) , ylim = c(1244393 , 1252307)) +
  scale_fill_gradientn(colors = colorramp) +
  labs(title = expression("Model-predicted ground-level NO"[2] * " concentration") ,
       fill = expression("NO"[2] * " (µg/m"^3 * ")") , 
       x = "Longitude" , y = "Latitude") +
  theme_bw() +
  theme(axis.text = element_text(size = 10) , axis.text.x = element_text(angle = 90 , vjust = 0.5 , hjust = 1))
save_plot(
  "3_results/output-graph/model_annual/mapping_zoomed.png" , 
  plot = last_plot() ,
  base_height = 8 , base_width = 13
)  

# =====================================
# map: monthly
# =====================================
colorramp_monthly <- c("#2b83ba" , "#6bb0af" , "#abdda4" , "#d5eeb2" , "#ffffbf" , "#fed790" , "#fdae61" , "#ea633e" , "#d7191c" , "#801E1E")

ggplot() +
  geom_stars(data = projection_monthly) +
  facet_grid(model ~ month) +
  coord_sf(crs = st_crs(2056) , expand = FALSE) +
  scale_fill_gradientn(colors = colorramp_monthly) +
  labs(title = expression("Model-predicted ground-level NO"[2] * " concentration") ,
       fill = expression("NO"[2] * " (µg/m"^3 * ")") , 
       x = "Longitude" , y = "Latitude") +
  theme_bw() +
  theme(axis.text = element_text(size = 6) , 
        axis.text.x = element_text(angle = 90 , vjust = 0.5 , hjust = 1) , 
        strip.text.y = element_text(size = 6))

save_plot(
  "3_results/output-graph/model_monthly/mapping.png" , 
  plot = last_plot() ,
  base_height = 13 , base_width = 15
)

# zoomed
ggplot() +
  geom_stars(data = projection_monthly) +
  facet_grid(model ~ month) +
  coord_sf(crs = st_crs(2056) , expand = FALSE , xlim = c(2674951 , 2685258) , ylim = c(1244393 , 1252307)) +
  scale_fill_gradientn(colors = colorramp_monthly) +
  labs(title = expression("Model-predicted ground-level NO"[2] * " concentration") ,
       fill = expression("NO"[2] * " (µg/m"^3 * ")") , 
       x = "Longitude" , y = "Latitude") +
  theme_bw() +
  theme(axis.text = element_text(size = 6) , 
        axis.text.x = element_text(angle = 90 , vjust = 0.5 , hjust = 1) , 
        strip.text.y = element_text(size = 6))

save_plot(
  "3_results/output-graph/model_monthly/mapping_zoomed.png" , 
  plot = last_plot() ,
  base_height = 13 , base_width = 15
)


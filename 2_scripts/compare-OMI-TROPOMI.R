#####################################################################################################
# Masterarbeit
# Compare raw OMI- and TROPOMI-NO2
# version.2
# 2021-05-04
#####################################################################################################

# =====================================
# required packages
# =====================================
library(stars) ; library(sf) ; library(raster)
library(dplyr) ; library(tidyr) 
library(ggplot2)
library(lubridate) ; library(stringr)

# =====================================
# load AOI
# =====================================
CH <- st_read("1_data/raw/Switzerland_shapefile/CHE_adm0.shp") %>% 
  st_geometry() %>% 
  st_simplify(preserveTopology = TRUE , dTolerance = 0.05)

# =====================================
# load OMI data
# =====================================
# file paths
files_OMI <- list.files("1_data/raw/OMI-NO2" , "_AOI.tif$" , full.names = TRUE)
# timestamps
ts_OMI_raw <- basename(files_OMI) %>% 
  str_extract("\\d{8}") %>% 
  as_date()

# read as a stacked raster
#OMI_raw <- read_stars(files_OMI) # but every image of each day as one separate attribute; needs to merge()
OMI_raw <- read_stars(files_OMI , along = list(date = ts_OMI_raw))  %>% 
  setNames("value")


# =====================================
# load TROPOMI data
# =====================================
# file paths
files_TROPOMI <- list.files("1_data/raw/TROPOMI-NO2/preprocessed_resampled" , "_AOI_rs.tif$" , full.names = TRUE)
# timestamps
ts_TROPOMI_raw <- basename(files_TROPOMI) %>% 
  str_extract("\\d{8}") %>% 
  as_date()

# read as a stacked raster
TROPOMI_raw <- read_stars(files_TROPOMI , 
                          along = list(date = ts_TROPOMI_raw) , 
                          proxy = FALSE) %>% 
  st_set_dimensions(values = rep(c("NO2" , "QA")) , "band") %>% 
  setNames("value")
# band1: NO2 
# band2: QA_value
# date


# =====================================
# comparison for selected dates
# =====================================
{
  # assign the dates to visualize and compare
  selected_dates <- interval("2019-06-01" , "2019-06-10")
  # subset the image arrays based on the assigned date
  OMI_selected_dates <- OMI_raw %>% 
    filter(date %within% selected_dates) 
  TROPOMI_selected_dates <- TROPOMI_raw %>% 
    slice(band , 1) %>% 
    filter(date %within% selected_dates) 
}
# visualize 
cowplot::plot_grid(
  ggplot() +
    geom_stars(data = OMI_selected_dates) +
    geom_sf(data = CH , fill = NA , color = "white") + 
    facet_grid(~date) +
    scale_fill_viridis_c() +
    labs(title = "OMI-NO2") + 
    theme_void() , 
  ggplot() +
    geom_stars(data = TROPOMI_selected_dates) +
    geom_sf(data = CH , fill = NA , color = "white") + 
    facet_grid(~date) +
    scale_fill_viridis_c(limits = c(0,1e16)) +
    labs(title = "TROPOMI-NO2 (QA>0.50)") + 
    theme_void() , 
  align = "v" , axis = "lr" , ncol = 1
)


# =====================================
# originally the NO2 values are screened with the criteria QA>0.5
# try here with the criteria QA>0.75
# =====================================
TROPOMI_QA75 <- TROPOMI_raw %>% 
  split("band") %>%  # split dimension "band" into attributes: NO2 and QA
  mutate(NO2_QA75 = ifelse(QA>=0.75 , NO2 , NA_real_)) %>% # filter NO2 values based on QA values
  merge() %>%        # merge the attributes (bands) back as a dimension
  setNames("value") %>% # rename attribute
  st_set_dimensions(4 , names = "band") # clean dimension name
# three bands of TROPOMI_QA75: NO2, QA_value, NO2_QA75

# compare again
{
  # assign the dates to visualize and compare
  selected_dates <- interval("2019-06-01" , "2019-06-10")
  # subset the image arrays based on the assigned date
  OMI_selected_dates <- OMI_raw %>% 
    filter(date %within% selected_dates) 
  TROPOMI_selected_dates_QA75 <- TROPOMI_QA75 %>% 
    slice(band , 3) %>%  # band 3: NO2_QA75
    filter(date %within% selected_dates) 
  }

cowplot::plot_grid(
  ggplot() +
    geom_stars(data = OMI_selected_dates) +
    geom_sf(data = CH , fill = NA , color = "white") + 
    facet_grid(~date) +
    scale_fill_viridis_c() +
    labs(title = "OMI-NO2") + 
    theme_void() , 
  ggplot() +
    geom_stars(data = TROPOMI_selected_dates_QA75) +
    geom_sf(data = CH , fill = NA , color = "white") + 
    facet_grid(~date) +
    scale_fill_viridis_c() +
    labs(title = "TROPOMI-NO2 (QA>0.75)") + 
    theme_void() , 
  align = "v" , axis = "lr" , ncol = 1
)


# =====================================
# data preparation for pixel-to-pixel comparison
# =====================================
# 1. use NO2_QA75 for TROPOMI ----------------------------
TROPOMI_fine <- TROPOMI_QA75 %>% 
  filter(band == "NO2_QA75") %>% 
  split("band") %>% 
  setNames("TROPOMI")

# 2. resample OMI to the finer TROPOMI grids ----------------------------
OMI_rs <- OMI_raw %>% 
  st_warp(dest = TROPOMI_fine) %>% 
  setNames("OMI")
# compare resampled and original
cowplot::plot_grid(
  # plot: OMI with original grids
  ggplot() +
    # first image (2019-01-01)
    geom_stars(data = OMI_raw[,,,1]) +
    # reference grid cells
    geom_sf(data = OMI_raw[,,,50] %>% 
              st_as_sf(as_points = FALSE) , 
            color = "azure3" , fill = NA , size = 0.1) +
    # Switzerland
    geom_sf(data = CH , fill = NA , color = "white") +
    scale_fill_viridis_c() +
    coord_sf(expand = FALSE) +
    labs(x = "Longtitude" , y = "Latitude" , fill = "NO2" , 
         title = "Original OMI" , 
         subtitle = "Data: OMI-NO2 on 2019-01-01") , 
  # plot: OMI resampled to TROPOMI grids
  ggplot() +
    # first image (2019-01-01)
    geom_stars(data = OMI_rs[,,,1]) +
    # reference grid cells
    geom_sf(data = OMI_rs[,,,50] %>% 
              st_as_sf(as_points = FALSE) , 
            color = "azure3" , fill = NA , size = 0.1) +
    # Switzerland
    geom_sf(data = CH , fill = NA , color = "white") +
    scale_fill_viridis_c() +
    coord_sf(expand = FALSE) +
    labs(x = "Longtitude" , y = "Latitude" , fill = "NO2" , 
         title = "Resampled-OMI to TROPOMI grids" , 
         subtitle = "Data: OMI-NO2 on 2019-01-01") 
)

# plot: compare the grids
ggplot() +
  geom_sf(data = CH , fill = "azure4") +
  geom_sf(data = OMI_rs[,,,50] %>% 
            st_as_sf(as_points = FALSE) , 
          color = "dodgerblue3" , fill = NA) +
  geom_sf(data = OMI_raw[,,,50] %>% 
            st_as_sf(as_points = FALSE) , 
          color = "deeppink" , fill = NA) +
  labs(x = "Longitude" , y = "Latitude" , 
       title = "Comparison between OMI and TROPOMI grids" , 
       subtitle = "Pink: original OMI grids; Blue: OMI resampled to TROPOMI grids")


# 3. stack TROPOMI and OMI into one array (FULL) ----------------------------
FULL <- c(TROPOMI_fine , OMI_rs)
# | 3 dimensions: x, y, date
# | 2 attributes: TROPOMI, OMI

# 4. mask (some pixels are outside of the AOI and are always NA ) ----------------------------
# --> remove from the table so that it would be counted 
AOI_stars <- st_read("1_data/raw/Switzerland_shapefile/AOI_4326.shp") %>% 
  st_rasterize(template = TROPOMI_fine) %>% 
  slice("band" , 1) %>% 
  mutate(AOI = ifelse(is.na(FID) , FALSE , TRUE))
plot(select(AOI_stars , AOI))


# =====================================
# pixel-to-pixel comparison
# =====================================
# 1. simple scatter plot and linear regression ----------------------------
FULL %>% 
  as.data.frame() %>% 
  filter(TROPOMI < quantile(TROPOMI , 0.99 , na.rm = TRUE)) %>% 
  filter(OMI < quantile(OMI , 0.99 , na.rm = TRUE)) %>% 
  ggplot(aes(x = TROPOMI , y = OMI)) +
  geom_abline(intercept = 0 , slope = 1 , color = "azure4" , linetype = 2) +
  geom_point(shape = 1 , alpha = 0.3) +
  geom_smooth(method = lm) +
  coord_fixed(1) +
  labs(title = expression("Scatter plot of OMI- and TROPOMI-NO"[2]*" products") , 
       x = expression("TROPOMI-NO"[2] * " (molecule/cm"^2*")") ,
       y = expression("OMI-NO"[2] * " (molecule/cm"^2*")")) +
  theme_bw()

# sqrt scale
FULL %>% 
  as.data.frame() %>% 
  mutate(TROPOMI = ifelse(TROPOMI < 0 , NA , TROPOMI) , 
         OMI = ifelse(OMI < 0 , NA , OMI)) %>% 
  ggplot(aes(x = TROPOMI , y = OMI)) +
  geom_point(shape = 1 , alpha = 0.3) +
  geom_smooth(method = lm) +
  coord_trans(x = "sqrt" , y = "sqrt") +
  labs(title = expression("Scatter plot of OMI- and TROPOMI-NO"[2]* " products \\n(Square-root scale)") , 
       x = expression("TROPOMI-NO"[2] * " (molecule/cm"^2*")") ,
       y = expression("OMI-NO"[2] * " (molecule/cm"^2*")"))+
  theme_bw()

# linear regression
lm_FULL <- FULL %>% 
  as.data.frame() %>% 
  filter(TROPOMI < quantile(TROPOMI , 0.99 , na.rm = TRUE)) %>% 
  filter(OMI < quantile(OMI , 0.99 , na.rm = TRUE)) %>% 
  mutate(TROPOMI_scaled = scale(TROPOMI) ,     # scaling
         OMI_scaled = scale(OMI)) %>% 
  lm(OMI_scaled ~ TROPOMI_scaled , data = .)
summary(lm_FULL)

# 2. cross table of NA pixels between TROPOMI and OMI ----------------------------
# visualization: missing value pixels for selected dates
ggplot() +
  geom_stars(
    data = FULL %>% 
      # counting NAs as two TRUE.FALSE bands
      mutate(TROPOMI = is.na(TROPOMI) , 
             OMI = is.na(OMI)) %>% 
      filter(date %within% selected_dates) %>% 
      merge()
  ) +
  geom_sf(data = CH , fill = NA , color = "white") +
  coord_sf(expand = FALSE) +
  scale_fill_discrete(direction = -1) +
  facet_grid(attributes ~ date) +
  #theme_void() +
  labs(x = "Longtitude" , y = "Latitude" , fill = "missing \nvalues")

# cross table
xtabs_NA <- FULL %>% 
  # counting NAs as two TRUE.FALSE bands
  transmute(NA_TROPOMI = is.na(TROPOMI) , 
            NA_OMI = is.na(OMI)) %>% 
  as.data.frame() %>% 
  xtabs(~NA_TROPOMI + NA_OMI , data = .)
# percentage
(xtabs_NA/sum(xtabs_NA)) %>% 
  round(3)

# 3. disagreement per pixel ----------------------------
agree <- FULL %>% 
  # counting NAs as two TRUE.FALSE bands
  mutate(NA_TROPOMI = is.na(TROPOMI) , 
         NA_OMI = is.na(OMI)) %>% 
  # calculating disagreement between NA_TROPOMI and NA_OMI
  mutate(agree = ifelse(NA_TROPOMI , -1 , 1) * ifelse(NA_OMI , -1 , 1)) %>%  # 1 as agree; -1 as disagree
  select(agree)

# aggregate the whole year
agree_sum <- agree %>% 
  mutate(agree_binary = ifelse(agree == 1 , 1 , 0) , 
         disagree_binary = ifelse(agree == -1 , 1 , 0)) %>% 
  select(-agree) %>% 
  st_apply(c("x","y") , sum) %>%  # sum value for each pixel (across dates)
  merge() %>% 
  st_set_dimensions(3 , values = c("agree" , "disagree") , names = c("var"))

# percentage of missing-value disagreement 
ggplot() +
  geom_stars(data = agree_sum[,,,2]/365*100) + # only plot disagree; calculate percentage
  geom_sf(data = CH , fill = NA , color = "white") +
  scale_fill_viridis_c() +
  coord_sf(expand = FALSE) +
  labs(x = "Longtitude" , y = "Latitude" , fill = "(%)" , 
       title = "Percentage of missing-value disagreement")

# 4. NA per pixel ----------------------------
NA_sum <- FULL %>% 
  # pixel=1 if value is NA, else 0
  transmute(NA_TROPOMI = ifelse(is.na(TROPOMI) , 1 , 0) , 
            NA_OMI = ifelse(is.na(OMI) , 1 , 0)) %>% 
  # aggregate the whole year for each pixel: sum
  st_apply(c("x","y") , sum)

ggplot() +
  geom_stars(
    data = NA_sum %>% 
      merge() %>% 
      st_set_dimensions(3 , values = c("TROPOMI" , "OMI") , names = "source")
  ) +
  geom_sf(data = CH , fill = NA , color = "white") +
  scale_fill_viridis_c(breaks = seq(0,360,60)) +
  coord_sf(expand = FALSE) +
  facet_grid(~source) +
  labs(x = "Longtitude" , y = "Latitude" , fill = "days" , 
       title = "Number of missing values per year")

# 5. NA per season  ----------------------------
NA_season <- FULL %>% 
  # pixel=1 if value is NA, else 0
  transmute(NA_TROPOMI = ifelse(is.na(TROPOMI) , 1 , 0) , 
            NA_OMI = ifelse(is.na(OMI) , 1 , 0)) %>% 
  # calculate season from the date dimension 
  # and use season as the third dimension
  st_set_dimensions(3 , 
                    values = as.character(quarter(st_get_dimension_values(. , 3) , fiscal_start = 12)) , 
                    names = "season")
# summary statistics: proportion of missing values per season
NA_season_df <- NA_season %>% 
  as.data.frame() %>% 
  # remove the pixels that are outside AOI
  full_join(as.data.frame(AOI_stars) , by = c("x" , "y")) %>% 
  filter(AOI) %>% 
  select(-FID , -AOI) %>% 
  # calculate the total missing-value pixels for each season
  pivot_longer(cols = starts_with("NA_") , names_to = "product" , names_prefix = "NA_" , values_to = "is_NA") %>% 
  group_by(season , product , is_NA) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  # calculate proportion
  pivot_wider(names_from = is_NA , values_from = count) %>% 
  rename(valid = `0` , missing = `1`) %>% 
  mutate(missing_proportion = missing/(valid + missing))
# visualization: bar chart
NA_season_df %>% 
  ggplot(aes(x = season , y = missing_proportion , fill = product)) +
  geom_bar(stat = "identity" , position = "dodge") +
  scale_x_discrete(labels = c("Winter" , "Spring" , "Summer" , "Autumn")) +
  scale_y_continuous(breaks = seq(0,1,0.25) , labels = seq(0,1,0.25)*100 , limits = c(0,1)) +
  labs(x = "Season" , y = "Percent of missing-value pixels (%)" , fill = "Product") +
  theme_bw()
# visualization: maps  
NA_season_sum <- c(
  # calculate the total number of missing-values per pixel for each season
  NA_season %>% 
    filter(season == "1") %>% 
    st_apply(c("x" , "y") , sum) %>% 
    merge() %>% 
    st_set_dimensions(3 , values = c("TROPOMI" , "OMI") , names = "source") , 
  NA_season %>% 
    filter(season == "2") %>% 
    st_apply(c("x" , "y") , sum) %>% 
    merge() %>% 
    st_set_dimensions(3 , values = c("TROPOMI" , "OMI") , names = "source") , 
  NA_season %>% 
    filter(season == "3") %>% 
    st_apply(c("x" , "y") , sum) %>% 
    merge() %>% 
    st_set_dimensions(3 , values = c("TROPOMI" , "OMI") , names = "source") , 
  NA_season %>% 
    filter(season =="4") %>% 
    st_apply(c("x" , "y") , sum) %>% 
    merge() %>% 
    st_set_dimensions(3 , values = c("TROPOMI" , "OMI") , names = "source") , 
  along = list("season" = c("Winter" , "Spring" , "Summer" , "Autumn") )
)
ggplot() +
  geom_stars(data = NA_season_sum) +
  geom_sf(data = CH , fill = NA , color = "white") +
  scale_fill_viridis_c(breaks = seq(0,90,15)) +
  coord_sf(expand = FALSE) +
  facet_grid(source~season) +
  labs(x = "Longtitude" , y = "Latitude" , fill = "days" , 
       title = "Number of missing-value pixels per season")


# =====================================
# elevation <-> missing value pixels
# =====================================
# load DEM
file_DEM <-"1_data/raw/EU-DEM-1.1/eu_dem_v11_E40N20_AOI.tif"
DEM_rs <- read_stars(file_DEM) %>% 
  st_warp(dest = NA_sum , use_gdal = TRUE , method = "bilinear") %>% 
  setNames("altitude")

# altitude class
DEM_rs %>% 
  # cut altitude into 4 classes
  mutate(altitude_class = cut(altitude , 
                              seq(0,4000,1000) , 
                              labels = as.character(0:3))) %>% 
  c(. , NA_sum) %>% 
  as.data.frame() %>% 
  filter(!is.na(altitude)) %>% 
  # NA pixel-day for each altitude class
  pivot_longer(cols = starts_with("NA_") , 
               names_to = "product" , names_prefix = "NA_" , values_to = "nNA") %>% 
  ggplot(aes(x = altitude_class , y = nNA)) +
  geom_boxplot() +
  facet_grid(~product) +
  scale_x_discrete(labels = c("0-999" , "1,000-1,999" , "2,000-2,999" , "3,000-3,999")) +
  scale_y_continuous(limits = c(0,365)) +
  coord_flip() +
  labs(x = "Altitude (m)" , y = "Days" , 
       title = "Days of missing values per pixel per year") +
  theme_bw()
  #theme(axis.text.x = element_text(angle = 90 , vjust = 0.5 , hjust = 1))
  


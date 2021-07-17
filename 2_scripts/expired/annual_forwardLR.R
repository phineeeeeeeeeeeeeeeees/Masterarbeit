#####################################################################################################
# Masterarbeit
# Modeling
# Annual model: unsupervised forward stepwise linear regression
# 2021-06-10
#####################################################################################################

# =====================================
# required packages
# =====================================
library(readr)
library(sf) ; library(stars)
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(ggsci) ; library(ggthemes)
library(lubridate) ; library(stringr)


# =====================================
# load datasets
# =====================================
# =====================================
# load datasets
# =====================================
# training data (OMI)
data_annual <- read_csv("1_data/processed/cleaned/extracted/annual_scaled.csv") %>% 
  # for the OMI model: exclude TROPOMI and meteorological variables at 12H
  select(-TROPOMI_NO2 , -ends_with("_12H")) %>% 
  # exclude the variables with 10000m buffer
  select(-ends_with("_10000"))
# training data (TROPOMI)
# data_annual <- read_csv("1_data/processed/cleaned/extracted/annual_scaled2.csv") %>% 
#   # for the OMI model: exclude TROPOMI and meteorological variables at 15H
#   select(-OMI_NO2 , -ends_with("_15H"))

# AOI: national boundary + 5km buffer
AOI <- st_read("1_data/raw/Switzerland_shapefile/CHE_adm0.shp") %>% 
  select(ID_0) %>% # drop unnecessary columns
  st_transform(st_crs(2056)) %>% 
  st_buffer(5000)

NO2_sites <- read_csv("1_data/raw/NO2-monitoring/metadaten_idbluft_supplemented.csv") %>% 
  # clean column names
  setNames(str_replace_all(colnames(.) , " " , "_")) %>% 
  setNames(str_replace_all(colnames(.) , "-" , "_")) %>% 
  # convert to spatial points
  st_as_sf(coords = c("x" , "y") , crs = st_crs(2056))

# =====================================
# cross validation design: 10-fold spatially-blocked cross validation
# =====================================
k_fold <- 5
set.seed(10142)
AOI_CV <- AOI %>%
  # make grids
  st_make_grid(n = c(4,3)) %>% 
  st_as_sf() %>% 
  st_intersection(AOI %>% st_geometry) %>% 
  # randomly assign cross validation index
  mutate(spatial_CV = sample(rep(1:k_fold, length.out = nrow(.)) , replace = FALSE) %>% as.factor())
NO2_sites_CV <- NO2_sites %>% 
  select(Station_name) %>% 
  # CV grids
  st_join(AOI_CV) %>% 
  # the stations that are included in the monitoring data
  filter(Station_name %in% data_annual$Station_name)
table(NO2_sites_CV$spatial_CV)
ggplot() +
  geom_sf(data = AOI_CV , color = "azure3") +
  geom_sf(data = NO2_sites_CV, aes(color = spatial_CV)) +
  scale_color_jco(labels = paste0(1:10 , " (n=" , table(NO2_sites_CV$spatial_CV) , ")")) +
  labs(color = "CV-group" , 
       title = sprintf("Spatially-blocked %s-fold cross validation" , k_fold)) +
  theme_light()
# cross table: CV group and station type
NO2_sites %>% 
  st_join(NO2_sites_CV) %>% 
  xtabs(~Type_of_station + spatial_CV , data = .)

# implement the CV design in the training data
data_annual <- data_annual %>% 
  left_join(NO2_sites_CV , by = "Station_name") %>% 
  select(-geometry)


# //////////////////////////////////////////////////////////////////////////
# data inspection
# //////////////////////////////////////////////////////////////////////////
data_annual %>% 
  select(-c(Station_name , spatial_CV , Type_of_zone , Type_of_station , Altitude , Canton_ID , Canton_name , X , Y)) %>% 
  pivot_longer(cols = everything() , names_to = "variables") %>% 
  ggplot(aes(x = value)) +
  geom_density() +
  facet_wrap(~variables , scales = "free")


# //////////////////////////////////////////////////////////////////////////
# benchmark: unsupervised forward stepwise linear regression
# //////////////////////////////////////////////////////////////////////////
lm_null <- lm(NO2 ~ 1 , data = data_annual %>% 
                select(-c(Station_name , spatial_CV , Type_of_zone , Type_of_station , Altitude , Canton_ID , Canton_name , X , Y)) %>% 
                na.omit()  )
lm_full <- lm(NO2 ~ . , data = data_annual %>% 
                select(-c(Station_name , spatial_CV , Type_of_zone , Type_of_station , Altitude , Canton_ID , Canton_name , X , Y)) %>% 
                na.omit()  )
lm_unsupervised <- step(lm_null , 
                        scope = list(lower = lm_null , upper = lm_full) , 
                        direction = "forward" , 
                        steps = 50 , 
                        trace = FALSE)

# =====================================
# cross validation
# =====================================
for(k in as.factor(1:k_fold)){
  # partition
  training.data <- data_annual %>% filter(spatial_CV != k)
  testing.data <- data_annual %>% filter(spatial_CV == k)
  # train model
  model_train <- lm(
    formula = as.formula(lm_unsupervised$call) , # <-
    data = training.data
  )
  # prediction
  prediction_test <- testing.data %>% 
    select(Station_name , NO2 , Type_of_station) %>% 
    # prediction
    mutate(predicted = predict(model_train , newdata = testing.data))
  # indices
  if(as.character(k) == "1"){
    lm_unsupervised_CV <- prediction_test %>%  # <-
      # R2, RMSE
      summarize(R2 = cor(NO2 , predicted , use = "na.or.complete")^2 , 
                RMSE = sqrt(sum(((predicted-NO2)^2)/n() , na.rm = TRUE))) %>% 
      pivot_longer(cols = everything()) %>% 
      # slope, intercept
      bind_rows(
        prediction_test %>% 
          group_modify( ~ lm(NO2 ~ predicted , data = .) %>% coef %>% as_tibble) %>% 
          mutate(name = c("intercept" , "slope"))
      ) %>% 
      mutate(k = k)
  }else{ # append
    lm_unsupervised_CV <- bind_rows(  # <-
      lm_unsupervised_CV ,            # <-
      prediction_test  %>% 
        # R2, RMSE
        summarize(R2 = cor(NO2 , predicted , use = "na.or.complete")^2 , 
                  RMSE = sqrt(sum(((predicted-NO2)^2)/n() , na.rm = TRUE))) %>% 
        pivot_longer(cols = everything()) %>% 
        # slope, intercept
        bind_rows(
          prediction_test %>% 
            group_modify( ~ lm(NO2 ~ predicted , data = .) %>% coef %>% as_tibble) %>% 
            mutate(name = c("intercept" , "slope"))
        ) %>% 
        mutate(k = k)
    )
  }
  # clean environment
  rm(training.data , testing.data , model_train , prediction_test , k)
}



# //////////////////////////////////////////////////////////////////////////
# model evaluation
# //////////////////////////////////////////////////////////////////////////
# =====================================
# unsupervised forward stepwise linear regression
# =====================================
summary(lm_unsupervised)
# diagnostic plot
par(mfrow = c(2,2)) ; plot(lm_unsupervised) ; par(mfrow = c(1,1))
# model performance indices as a data.frame
prediction_unsupervised <- data_annual %>% 
  select(Station_name , NO2 , Type_of_station) %>% 
  # prediction
  mutate(predicted = predict(lm_unsupervised , newdata = data_annual))
lm_unsupervised_indices <- prediction_unsupervised  %>% 
  # R2, RMSE
  summarize(R2 = cor(NO2 , predicted , use = "na.or.complete")^2 , 
            RMSE = sqrt(sum(((predicted-NO2)^2)/n() , na.rm = TRUE))) %>% 
  pivot_longer(cols = everything()) %>% 
  # slope, intercept
  bind_rows(
    prediction_unsupervised %>% # <-
      group_modify( ~ lm(NO2 ~ predicted , data = .) %>% broom::tidy() ) %>% 
      select(term , estimate , std.error) %>% 
      # tidy
      rename(name = term , value = estimate) %>% 
      mutate(name = str_replace_all(name , "[:punct:]" , "") %>% str_to_lower()) %>% 
      mutate(name = ifelse(name == "predicted" , "slope" , name)) %>% 
      # standard deviation
      mutate(min = value - 1.96*std.error , 
             max = value + 1.96*std.error) %>% 
      select(-std.error)
  )


# cross validation
lm_unsupervised_CV_indices <- lm_unsupervised_CV %>% 
  group_by(name) %>% 
  summarize(mean = mean(value) , 
            min = min(value) , max = max(value)) %>% 
  ungroup()

# # remove the variables whose p-value > 0.10
# lm_unsupervised2 <- lm_unsupervised %>% 
#   broom::tidy() %>% 
#   filter(term != "(Intercept)") %>% 
#   # remove the variables whose p-value > 0.10
#   filter(p.value < 0.1) %>% 
#   # # remove the variables with VIF>3 to avoid multi-collinearity
#   # left_join(
#   #   # VIF measures how much the variance of a regression coefficient 
#   #   # is inflated due to multicollinearity in the model
#   #   lm_unsupervised %>% 
#   #     car::vif() %>% # VIF of the model variables
#   #     as.list() %>% 
#   #     as_tibble() %>% 
#   #     pivot_longer(cols = everything() , names_to = "term" , values_to = "VIF") %>% 
#   #     arrange(-VIF) , 
#   #   by = "term"
#   # ) %>% 
#   # filter(VIF < 3) %>% 
#   select(term) %>% 
#   unlist %>% unname %>% 
#   paste(collapse = "+") %>% 
#   sprintf("NO2~%s" , .) %>% 
#   formula() %>% 
#   lm(data = data_annual)
# summary(lm_unsupervised2)
# par(mfrow = c(2,2)) ; plot(lm_unsupervised2) ; par(mfrow = c(1,1))




# =====================================
# visualization and comparison
# =====================================
lm_indices <- lm_SLR_CV_indices %>% 
  # SLR with CV
  rename(value = mean) %>% 
  mutate(model = "SLR" , CV = "10-fold") %>% 
  # SLR without CV (100% training data)
  bind_rows(
    lm_SLR_indices %>% 
      mutate(model = "SLR" , CV = "without")
  ) %>% 
  # unsupervised with CV
  bind_rows(
    lm_unsupervised_CV_indices %>% 
      rename(value = mean) %>% 
      mutate(model = "forward" , CV = "10-fold")
  ) %>% 
  # unsupervised without CV
  bind_rows(
    lm_unsupervised_indices %>% 
      mutate(model = "forward" , CV = "without")
  )

lm_indices %>% 
  filter(name %in% c("R2" , "RMSE")) %>% 
  ggplot(aes(x = model , y = value , fill = CV)) +
  geom_bar( stat = "identity" , position = position_dodge(width = 0.9)) +
  geom_pointrange(aes(ymin = min , ymax = max) , 
                  size = 0.3 , position = position_dodge(width = 0.9)) +
  facet_grid(name~. , scales = "free_y") +
  scale_fill_jco(labels = c("10-fold CV" , "without CV") , name = "") +
  labs(title = "Model performance indices") +
  theme_bw()
lm_indices %>% 
  filter(name %in% c("intercept" , "slope")) %>% 
  ggplot(aes(x = model , y = value , fill = CV)) +
  geom_crossbar(aes(ymin = min , ymax = max) , 
                position = position_dodge(width = 0.9)) +
  facet_grid(name~. , scales = "free_y") +
  scale_fill_jco(labels = c("10-fold CV" , "without CV") , name = "") +
  labs(title = "Goodness-of-fit") +
  theme_bw()


# =====================================
# projection
# =====================================
# import full spatial predictor variables 
spatial_df <- vroom::vroom("1_data/processed/cleaned/data-frame/spatial_df.csv" , 
                           col_select = c(x,y,all_of(names(lm_unsupervised$coefficients)[-1])))
# projections
projection_unsupervised <- spatial_df %>% 
  mutate(NO2_pred = predict(lm_unsupervised , newdata = .) %>% unname)
# convert the projected NO2 to stars
projection_unsupervised_stars <- projection_unsupervised %>% 
  select(x,y,NO2_pred) %>% 
  st_as_stars(dim = c("x" , "y")) %>% 
  st_set_crs(st_crs(2056))
# visualization
ggplot() +
  geom_stars(data = projection_unsupervised_stars) +
  geom_sf(data = AOI , fill = NA , color = "white") +
  coord_sf(crs = st_crs(2056) , expand = FALSE) +
  scale_fill_viridis_c() +
  labs(title = expression("Forward-linear-regression predicted ground-level NO"[2] * " concentration") , 
       fill = expression("NO"[2] * " (µg/m"^3 * ")"))

# export the projected NO2
projection_unsupervised_stars %>% 
  write_stars("3_results/output-data/model_annual/projection_unsupervised.tif")


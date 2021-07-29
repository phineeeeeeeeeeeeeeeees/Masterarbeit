#####################################################################################################
# Masterarbeit
# Modeling
# Annual model: supervised stepwise linear regression -- OMI and TROPOMI
# 2021-06-10
#####################################################################################################

# =====================================
# required packages
# =====================================
library(readr) ; library(vroom)
library(sf) ; library(stars)
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(ggsci) ; library(ggthemes) ; library(cowplot)
library(lubridate) ; library(stringr)
library(spdep)
library(Metrics)

source("2_scripts/utils_model-eval.R")

# =====================================
# load datasets
# =====================================
# training data
data_annual_raw <- read_csv("1_data/processed/cleaned/extracted/annual_scaled.csv")

# cross validation
{
  in_filepath_CV <- list.files("1_data/processed/cleaned/extracted/" , "CV.shp$" , full.names = TRUE)
  sites_CV <- st_read(in_filepath_CV) %>% 
    rename(Station_name = Station)
  k_fold <- in_filepath_CV %>% 
    str_extract("\\d+-fold") %>% str_extract("\\d+") %>% 
    as.integer()
}

# implement the CV design in the training data
data_annual_raw <- data_annual_raw %>% 
  inner_join(sites_CV , by = "Station_name") %>% # this also excludes the 3 sites that are outside Switzerland and was not counted in the CV groups
  select(-geometry)

# non-predictor columns
columns_nonpredictor <- c("Station_name" , "NO2" , "Type_of_zone" , "Type_of_station" , 
                          "Altitude" , "Canton_ID" , "Canton_name" , "X" , "Y" , 
                          "CV" , "spatial_CV")

# //////////////////////////////////////////////////////////////////////////
# data inspection
# //////////////////////////////////////////////////////////////////////////
# data_annual %>% 
#   select(-c(Station_name , spatial_CV , Type_of_zone , Type_of_station , Altitude , Canton_ID , Canton_name , X , Y)) %>% 
#   pivot_longer(cols = everything() , names_to = "variables") %>% 
#   ggplot(aes(x = value)) +
#   geom_density() +
#   facet_wrap(~variables , scales = "free")



# //////////////////////////////////////////////////////////////////////////
# naming the model
# //////////////////////////////////////////////////////////////////////////
model_name <- "Supervised stepwise linear regression"
model_abbr <- "SLR"
SAT_product <- c("spatial" , "OMI" , "TROPOMI")[3]

# //////////////////////////////////////////////////////////////////////////
# supervised stepwise linear regression algorithm
# //////////////////////////////////////////////////////////////////////////
{
  # subset data: satellite-product
  if(SAT_product == "OMI"){
    # for the OMI model: exclude TROPOMI and meteorological variables at 12H
    data_annual <- data_annual_raw %>% 
      select(-TROPOMI_NO2 , -ends_with("_12H")) 
  }else if(SAT_product == "TROPOMI"){
    # for the TROPOMI model: exclude OMI and meteorological variables at 15H
    data_annual <- data_annual_raw %>% 
      select(-OMI_NO2 , -ends_with("_15H")) 
  }else if(SAT_product == "spatial"){
    data_annual <- data_annual_raw %>% 
      select(-OMI_NO2 , -ends_with("_15H")) 
  }
  # =====================================
  # expected direction of effect: 
  # =====================================
  vars_positive <- c("OMI_NO2" , "enox" , "lc_RES" , "lc_IND" , "lc_BUILT" , "light" , "population" , 
                     "traffint" , "mjdens" , "rdens" , "intersection")
  vars_negative <- c("blh" , "ws" , "NDVI" , "elevation" , "lc_URBGR" , "lc_NAT" , "nearmjrd")
  vars_neutral <- c("temperature" , "precipitation" , "pressure" , "tcc" , "wd" , "lc_AGR")
  
  vars_direction <- data_annual %>% 
    # the predictor variables
    select(-all_of(columns_nonpredictor)) %>% 
    colnames() %>% 
    as_tibble() %>% 
    rename(predictors = value) %>% 
    # the expected direction of effects
    mutate(expect_positive = ifelse(str_detect(predictors , paste(vars_positive , collapse = "|")) , TRUE , NA)) %>% 
    mutate(expect_positive = ifelse(str_detect(predictors , paste(vars_negative , collapse = "|")) , FALSE , expect_positive)) %>% 
    mutate(expect_positive = ifelse(str_detect(predictors , paste(vars_neutral , collapse = "|")) , NA , expect_positive)) 
  
  
  # =====================================
  # 1. initial univariate regression
  # =====================================
  # coefficient
  model_coefs_initial <- data_annual %>% 
    pivot_longer(cols = -all_of(columns_nonpredictor) , 
                 names_to = "predictors") %>% 
    # fit linear regressions
    group_by(predictors) %>% 
    group_modify(~ lm(NO2~value , data = .x) %>% coef %>% .[2] %>% as_tibble) %>% 
    ungroup() %>% 
    rename(coef = value) %>% 
    # check the direction of effect
    mutate(coef_positive = coef > 0) %>% 
    left_join(vars_direction , by = "predictors") %>% 
    mutate(consistent = coef_positive == expect_positive | is.na(expect_positive))
  # R-squared
  model_R2_initial <- data_annual %>% 
    pivot_longer(cols = -all_of(columns_nonpredictor) , 
                 names_to = "predictors") %>% 
    # fit linear regressions
    group_by(predictors) %>% 
    group_modify(~ lm(NO2~value , data = .) %>% summary %>% .$adj.r.squared %>% as_tibble) %>% 
    ungroup() %>% 
    rename(R2 = value) %>% 
    arrange(-R2)
  
  # =====================================
  # 2. initial model: the predictor variable with the highest adj-R2 & expected direction of effect
  # =====================================
  if(SAT_product == "spatial"){
    included_var_initial <- model_R2_initial %>%
      left_join(model_coefs_initial , by = "predictors") %>%
      # direction of effect as the defined prior
      filter(consistent) %>%
      # the predictor with the highest adj-R2
      slice(1) %>%
      select(predictors) %>%
      unlist() %>% unname
  }else if(SAT_product %in% c("OMI" , "TROPOMI")){
    # forcing OMI/TROPOMI to enter the model
    included_var_initial <- model_R2_initial %>%
      left_join(model_coefs_initial , by = "predictors") %>%
      filter(str_detect(predictors , "OMI_NO2")) %>%
      # direction of effect as the defined prior
      filter(consistent) %>%
      # the predictor with the highest adj-R2
      slice(1) %>%
      select(predictors) %>%
      unlist() %>% unname
  }
  
  # =====================================
  # 3. sequentially adding predictor variables
  # =====================================
  # | increase the adj-R2 the most
  # | the direction of effect is as expected
  # | does not change the direction of effect of the existing predictors in the model
  
  included_var <- c(included_var_initial)
  formula_SLR <- paste("NO2 ~" , paste(included_var , collapse = " + "))  # initial formula
  while(TRUE){
    # coefficient
    model_coefs <- data_annual %>% 
      pivot_longer(cols = -c(all_of(included_var) , all_of(columns_nonpredictor)) , 
                   names_to = "predictors") %>% 
      # fit linear regressions
      group_by(predictors) %>% 
      group_modify(~ lm(formula(paste(formula_SLR , "+ value")) , data = .x) %>% 
                     broom::tidy()
      ) %>% 
      ungroup() %>% 
      # clean the regression result table
      select(predictors , term , estimate) %>% 
      filter(term != "(Intercept)") %>% 
      pivot_wider(names_from = term , values_from = estimate) %>% 
      pivot_longer(cols = -c("predictors" , "value") , 
                   names_to = "existing_vars" , values_to = "coef_existing_vars") %>% 
      rename(coef = value) %>% 
      # check the direction of effect
      mutate(coef_positive = coef > 0) %>% 
      mutate(coef_existing_vars_positive = coef_existing_vars > 0) %>% 
      # the expected direction of effect
      left_join(vars_direction , by = "predictors") %>% 
      left_join(vars_direction %>% 
                  rename(expect_existing_vars_positive = expect_positive) ,
                by = c("existing_vars" = "predictors")) %>% 
      mutate(consistent = (coef_positive == expect_positive) | is.na(expect_positive)) %>% 
      mutate(consistent_existing_var = (coef_existing_vars_positive == expect_existing_vars_positive) | is.na(expect_existing_vars_positive)) %>% 
      # check: does not change the direction of existing predictor variables
      group_by(predictors, coef , coef_positive , expect_positive , consistent) %>% 
      summarize(consistent_existing_var = all(consistent_existing_var)) %>% 
      ungroup()
    # R-squared
    model_R2 <- data_annual %>% 
      pivot_longer(cols = -c(all_of(included_var) , all_of(columns_nonpredictor)) , 
                   names_to = "predictors") %>% 
      # fit linear regressions
      group_by(predictors) %>% 
      group_modify(~ lm(formula(paste(formula_SLR , "+ value")) , data = .x) %>% 
                     summary %>% .$adj.r.squared %>% as_tibble) %>% 
      ungroup() %>% 
      rename(R2 = value) %>% 
      # R2 added compared to the previous model
      mutate(original_R2 = lm(formula(formula_SLR) , data = data_annual) %>% summary %>% .$adj.r.squared) %>% 
      mutate(delta_R2 = R2 - original_R2) %>% 
      arrange(-delta_R2) 
    # check: 
    # any predictor with the expected direction effect 
    # & any predictor adds to the adj-R2 of the model 
    # & the existing variables have the same direction of effect
    delta_R2_threshold <- 0.01
    if(any(model_coefs$consistent) & any(model_R2$delta_R2 > delta_R2_threshold) & any(model_coefs$consistent_existing_var) ){
      # the predictor variable added in this round
      included_var_new <- model_R2 %>% 
        filter(delta_R2 > delta_R2_threshold) %>% 
        left_join(model_coefs , by = "predictors") %>% 
        # direction of effect as the defined prior
        filter(consistent & consistent_existing_var) %>% 
        # the predictor that add the most to the adj-R2
        slice(1) %>% 
        select(predictors) %>% 
        unlist() %>% unname()
      # check: any predictor variable that meets the requirement 
      if(length(included_var_new) == 0) break
      # a vector of the added predictor variables
      included_var <- c(included_var , included_var_new)
      # the formula with the added predictor variables
      formula_SLR <- paste("NO2 ~" , paste(included_var , collapse = " + "))
    }else{
      break
    }
  }
  
  # =====================================
  # 4. remove the variables whose p-value > 0.10 (iteratively)
  # 5. remove the variables with VIF>3 to avoid multi-collinearity
  # =====================================
  while(TRUE){
    model_p_VIF <- lm(formula(formula_SLR) , data = data_annual) %>% 
      # p-value
      broom::tidy() %>% 
      filter(term != "(Intercept)") %>% 
      # VIF
      left_join(
        # VIF measures how much the variance of a regression coefficient 
        # is inflated due to multicollinearity in the model
        lm(formula(formula_SLR) , data = data_annual) %>% 
          car::vif() %>% # VIF of the model variables
          as.list() %>% 
          as_tibble() %>% 
          pivot_longer(cols = everything() , names_to = "term" , values_to = "VIF") , 
        by = "term"
      ) 
    if(all(model_p_VIF$p.value < 0.1) & all(model_p_VIF$VIF < 3)){
      # if every variable is p<0.1 and VIF<3 : done
      formula_SLR_final <- included_var %>% 
        paste(collapse = "+") %>% 
        sprintf("NO2~%s" , .) %>% 
        formula() 
      break
    }else{
      included_var <- model_p_VIF %>% 
        # remove the variables whose p-value > 0.10
        filter(p.value < 0.1) %>% 
        # remove the variables with VIF>3 to avoid multi-collinearity
        filter(VIF < 3) %>% 
        # rewrite the remaining variables as a formula
        select(term) %>% 
        unlist %>% unname
      formula_SLR <- paste("NO2 ~" , paste(included_var , collapse = " + "))
    }
  }
  # the end
}


# =====================================
# cross validation
# =====================================
# calculated and save the CV-prediction
# conventional CV
for(k in as.factor(1:k_fold)){
  # partition
  training.data <- data_annual %>% filter(CV != k)
  testing.data <- data_annual %>% filter(CV == k)
  # train model
  model_train <- lm(
    formula = formula_SLR_final ,  # <-
    data = training.data
  )
  # prediction
  prediction_test <- testing.data %>% 
    select(Station_name , NO2 , Type_of_station , CV) %>% 
    # prediction
    mutate(predicted = predict(model_train , newdata = testing.data))
  # prediction data.frame
  if(as.character(k) == "1"){
    lm_SLR_prediction_CV <- prediction_test # <-
  }else{ # append
    lm_SLR_prediction_CV <- bind_rows(lm_SLR_prediction_CV , prediction_test) # <-
  }
  # clean environment
  rm(training.data , testing.data , model_train , prediction_test , k)
}

# spatial CV
for(k in as.factor(1:k_fold)){
  # partition
  training.data <- data_annual %>% filter(spatial_CV != k)
  testing.data <- data_annual %>% filter(spatial_CV == k)
  # train model
  model_train <- lm(
    formula = formula_SLR_final ,  # <-
    data = training.data
  )
  # prediction
  prediction_test <- testing.data %>% 
    select(Station_name , NO2 , Type_of_station , spatial_CV) %>% 
    # prediction
    mutate(predicted = predict(model_train , newdata = testing.data))
  # prediction data.frame
  if(as.character(k) == "1"){
    lm_SLR_prediction_CV_sp <- prediction_test # <-
  }else{ # append
    lm_SLR_prediction_CV_sp <- bind_rows(lm_SLR_prediction_CV_sp , prediction_test) # <-
  }
  # clean environment
  rm(training.data , testing.data , model_train , prediction_test , k)
}

# combine the prediction of the two CV
lm_SLR_prediction_CV <- lm_SLR_prediction_CV %>% 
  left_join(lm_SLR_prediction_CV_sp , 
            by = c("Station_name" , "NO2" , "Type_of_station") , 
            suffix = c("_CV" , "_spatialCV"))


# //////////////////////////////////////////////////////////////////////////
# model evaluation
# //////////////////////////////////////////////////////////////////////////
lm_SLR <- lm(formula(formula_SLR_final) , data = data_annual)

# =====================================
# model summary
# =====================================
summary(lm_SLR)

# =====================================
# observed, predicted, residuals
# =====================================
lm_SLR_prediction <- data_annual %>% 
  select(Station_name , NO2 , Type_of_station , X , Y) %>% 
  # prediction
  mutate(predicted = predict(lm_SLR , newdata = data_annual)) %>% 
  # CV-prediction
  full_join(lm_SLR_prediction_CV , 
            by = c("Station_name" , "NO2" , "Type_of_station")) %>% 
  # residuals
  mutate(residual = NO2 - predicted , 
         residual_CV = NO2 - predicted_CV , 
         residual_spatialCV = NO2 - predicted_spatialCV)

# =====================================
# performance indices
# =====================================
# model performance indices as a data.frame
lm_SLR_indices <- lm_SLR_prediction %>% 
  eval_performance_indices()


# =====================================
# visualization
# =====================================
out_dirpath_plots <- sprintf("3_results/output-graph/model_annual/%s" , model_abbr)
if(!dir.exists(out_dirpath_plots)) dir.create(out_dirpath_plots)

# diagnostic plot
# par(mfrow = c(2,2)) ; plot(lm_SLR) ; par(mfrow = c(1,1))

# predicted <-> observed
plot_obs_pred(lm_SLR_prediction , # <-
              sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/obs-pred_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 5 , base_height = 3.5
)

# residual diagnostic plots
plot_resid(lm_SLR_prediction , # <-
           title_text = sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/residuals_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 7.8 , base_height = 6
)

# =====================================
# spatial autocorrelation of the residuals
# =====================================
moran_df <- eval_resid_moran(lm_SLR_prediction)

# visualization
plot_resid_map(lm_SLR_prediction , # <-
               sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/residual-map_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 6 , base_height = 4
)

# =====================================
# export datasets
# =====================================
{
  # export the model summary
  out_dirpath_lmsummary <- "3_results/output-data/model_annual/SLR_summary"
  if(!dir.exists(out_dirpath_lmsummary)) dir.create(out_dirpath_lmsummary , recursive = TRUE)
  lm_SLR %>%
    broom::tidy() %>%
    write_csv(sprintf("%s/%s.csv" , out_dirpath_lmsummary , SAT_product))
  
  # export the predicted values
  out_dirpath_predicted <- "3_results/output-data/model_annual/observed-predicted"
  if(!dir.exists(out_dirpath_predicted)) dir.create(out_dirpath_predicted , recursive = TRUE)
  lm_SLR_prediction %>% # <-
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_predicted , model_abbr , SAT_product))
  
  # export the model performance indices
  out_dirpath_indices <- "3_results/output-data/model_annual/indices"
  if(!dir.exists(out_dirpath_indices)) dir.create(out_dirpath_indices , recursive = TRUE)
  lm_SLR_indices %>% # <- 
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_indices , model_abbr , SAT_product))
  
  # Moran's I
  out_dirpath_Moran <- "3_results/output-data/model_annual/Moran"
  if(!dir.exists(out_dirpath_Moran)) dir.create(out_dirpath_Moran , recursive = TRUE)
  moran_df %>% 
    pivot_longer(cols = everything()) %>% 
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_Moran , model_abbr , SAT_product))
}

# =====================================
# export model
# =====================================
{
  out_dirpath_model <- "3_results/output-model/model_annual"
  if(!dir.exists(out_dirpath_model)) dir.create(out_dirpath_model)
  saveRDS(lm_SLR , # <-
          file = sprintf("%s/%s_%s.rds" , out_dirpath_model , model_abbr , SAT_product))
}

# =====================================
# projection
# =====================================
# # import full dataset
# spatial_df <- vroom("1_data/processed/cleaned/data-frame/spatial_df.csv" , 
#                     col_select = c(x,y,one_of(included_var))) %>% 
#   mutate(across(c(x,y) , as.integer))
# spatialtemporal_df <- vroom("1_data/processed/cleaned/data-frame/spatialtemporal_annual_df.csv" , 
#                             col_select = c(x,y,one_of(included_var))) %>% 
#   mutate(across(c(x,y) , as.integer))
# st_NDVI_df <- vroom("1_data/processed/cleaned/data-frame/spatialtemporal_NDVI_annual_df.csv" , 
#                     col_select = c(x,y,one_of(included_var))) %>% 
#   mutate(across(c(x,y) , as.integer))
# lookup_1000m <- vroom("1_data/processed/cleaned/data-frame/AOI_spatial-lookup-table_1000to100m.csv") %>% 
#   mutate(across(everything() , as.integer))
# lookup_250m <- vroom("1_data/processed/cleaned/data-frame/AOI_spatial-lookup-table_250to100m.csv") %>% 
#   mutate(across(everything() , as.integer))
# 
# full_df <- spatial_df %>% 
#   # 1000m lookup-table
#   left_join(lookup_1000m , by = c("x" = "x_100" , "y" = "y_100")) %>% 
#   # 1000m dataset
#   left_join(spatialtemporal_df , by = c("x_1000" = "x" , "y_1000" = "y")) %>% 
#   # 250m loop-up table
#   left_join(lookup_250m , by = c("x" = "x_100" , "y" = "y_100")) %>% 
#   # 250m dataset
#   left_join(st_NDVI_df , by = c("x_250" = "x" , "y_250" = "y"))
#   
# 
# # projections
# projection_SLR <- full_df %>% 
#   mutate(NO2_pred = predict(lm_SLR , newdata = .) %>% unname)
# # convert the projected NO2 to stars
# projection_SLR_stars <- projection_SLR %>% 
#   select(x,y,NO2_pred) %>% 
#   st_as_stars(dim = c("x" , "y")) %>% 
#   st_set_crs(st_crs(2056))
# # visualization
# ggplot() +
#   geom_stars(data = projection_SLR_stars) +
#   geom_sf(data = AOI , fill = NA , color = "white") +
#   coord_sf(crs = st_crs(2056) , expand = FALSE) +
#   scale_fill_viridis_c() +
#   labs(title = expression("SLR-predicted ground-level NO"[2] * " concentration") , 
#        fill = expression("NO"[2] * " (µg/m"^3 * ")")) +
#   theme_bw()
# 
# # zoom to a Canton
# zoomed.area <- st_read("1_data/raw/Switzerland_shapefile/CHE_adm1.shp") %>% 
#   st_transform(st_crs(2056)) %>% 
#   filter(str_detect(NAME_1 , "Zürich"))
# ggplot() +
#   geom_stars(data = projection_SLR_stars[zoomed.area]) +
#   coord_sf(crs = st_crs(2056) , expand = FALSE) +
#   scale_fill_viridis_c() +
#   labs(title = expression("SLR-predicted ground-level NO"[2] * " concentration") , 
#        fill = expression("NO"[2] * " (µg/m"^3 * ")")) +
#   theme_bw()
# 
# # export the projected NO2
# projection_SLR_stars %>% 
#   write_stars("3_results/output-data/model_annual/projection_SLR_OMI.tif")

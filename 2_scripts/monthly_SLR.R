#####################################################################################################
# Masterarbeit
# Modeling
# Monthly model: supervised stepwise linear regression -- OMI and TROPOMI
# 2021-06-29
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
data_monthly_raw <- read_csv("1_data/processed/cleaned/extracted/monthly_scaled.csv") 

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
data_monthly_raw <- data_monthly_raw %>% 
  inner_join(sites_CV , by = "Station_name") %>% 
  select(-geometry)

# non-predictor columns
columns_nonpredictor <- c("Station_name" , "NO2" , "Type_of_zone" , "Type_of_station" , 
                          "Altitude" , "Canton_ID" , "Canton_name" , "X" , "Y" , 
                          "CV" , "spatial_CV")

# //////////////////////////////////////////////////////////////////////////
# naming the model
# //////////////////////////////////////////////////////////////////////////
model_name <- "Supervised stepwise linear regression"
model_abbr <- "SLR"
SAT_product <- c("OMI" , "TROPOMI")[1]


# //////////////////////////////////////////////////////////////////////////
# data inspection and preparation
# //////////////////////////////////////////////////////////////////////////
# =====================================
# distribution of [NO2]
# =====================================
# data_monthly_raw %>% 
#   ggplot(aes(x = NO2)) +
#   geom_histogram() +
#   labs(x = expression("NO"[2]) , y = "Count" , 
#        title = "Histogram of the monthly average concentration") +
#   theme_bw()

# =====================================
# monthly variation
# =====================================
# data_monthly_raw %>% 
#   ggplot(aes(x = month , y = NO2)) +
#   geom_point(shape = 1) +
#   geom_line(aes(group = Station_name) , alpha = 0.4) +
#   facet_grid(~Type_of_station) +
#   scale_x_continuous(breaks = seq(1,12,2)) +
#   theme_bw()

# transformation of month (fit cosine wave)
# fit cosine wave: DOY, weekly cycle
data_monthly_raw <- data_monthly_raw %>% 
  group_modify( ~ {
    # period = 12
    phi <<- .x %>% 
      select(month , Station_name , NO2) %>% 
      mutate(z1 = cos(2*pi*month/12) , 
             z2 = sin(2*pi*month/12)) %>% 
      group_modify( ~ {
        x <- lm(NO2 ~ 0+z1+z2 , data = .x) %>% coef %>% unname
        atan(-(x[2]/x[1]))
      }) 
    # transformation
    .x %>% 
      mutate(month_trans = cos(2 * pi * month/12 + phi))
  }) 
# phi = -0.5593252

# # original <-> transformed month value
# data_monthly %>% 
#   ggplot(aes(x = month , y = month_trans)) +
#   geom_line() +
#   scale_x_continuous(breaks = seq(1,12,2)) +
#   labs(title = "Fitting a cosine wave to month" , 
#        subtitle = expression("M' = cos(2" * pi * "M" * omega *"+" * phi * ") , " * omega * "= 1/12, " * phi * "= -0.55626")) +
#   theme_bw()

# # more a linear relationship now
# data_monthly_raw %>%
#   group_by(month , Type_of_station , month_trans) %>%
#   summarize(mean = mean(NO2 , na.rm = TRUE) ,
#             sd = sd(NO2 , na.rm = TRUE)) %>%
#   ungroup() %>%
#   ggplot(aes(x = month_trans , y = mean , group = Type_of_station)) +
#   geom_point(aes(color = Type_of_station)) +
#   scale_x_continuous(breaks = cos(2 * pi * 1:12/12 + phi) ,
#                      labels = 1:12) +
#   labs(y = "Mean concentration") +
#   theme_bw()


# //////////////////////////////////////////////////////////////////////////
# supervised stepwise linear regression algorithm
# //////////////////////////////////////////////////////////////////////////
{
  # subset data: satellite-product
  if(SAT_product == "OMI"){
    # for the OMI model: exclude TROPOMI and meteorological variables at 12H
    data_monthly <- data_monthly_raw %>% 
      select(-TROPOMI_NO2 , -ends_with("_12H")) 
  }else if(SAT_product == "TROPOMI"){
    # for the TROPOMI model: exclude OMI and meteorological variables at 15H
    data_monthly <- data_monthly_raw %>% 
      select(-OMI_NO2 , -ends_with("_15H")) 
  }
  # =====================================
  # expected direction of effect: 
  # =====================================
  vars_positive <- c("OMI_NO2" , "enox" , "lc_RES" , "lc_IND" , "lc_BUILT" , "light" , "population" , 
                     "traffint" , "mjdens" , "rdens" , "intersection")
  vars_negative <- c("blh" , "ws" , "NDVI" , "elevation" , "lc_URBGR" , "lc_NAT" , "nearmjrd")
  vars_neutral <- c("temperature" , "precipitation" , "pressure" , "tcc" , "wd" , "lc_AGR" , "month_trans")
  
  vars_direction <- data_monthly %>% 
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
  model_coefs_initial <- data_monthly %>% 
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
  model_R2_initial <- data_monthly %>% 
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
    model_coefs <- data_monthly %>% 
      pivot_longer(cols = -c(all_of(included_var) , month , all_of(columns_nonpredictor)) , 
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
    model_R2 <- data_monthly %>% 
      pivot_longer(cols = -c(all_of(included_var) , month , all_of(columns_nonpredictor)) , 
                   names_to = "predictors") %>% 
      # fit linear regressions
      group_by(predictors) %>% 
      group_modify(~ lm(formula(paste(formula_SLR , "+ value")) , data = .x) %>% 
                     summary %>% .$adj.r.squared %>% as_tibble) %>% 
      ungroup() %>% 
      rename(R2 = value) %>% 
      # R2 added compared to the previous model
      mutate(original_R2 = lm(formula(formula_SLR) , data = data_monthly) %>% summary %>% .$adj.r.squared) %>% 
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
        dplyr::slice(1) %>% 
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
    model_p_VIF <- lm(formula(formula_SLR) , data = data_monthly) %>% 
      # p-value
      broom::tidy() %>% 
      filter(term != "(Intercept)") %>% 
      # VIF
      left_join(
        # VIF measures how much the variance of a regression coefficient 
        # is inflated due to multicollinearity in the model
        lm(formula(formula_SLR) , data = data_monthly) %>% 
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
  training.data <- data_monthly %>% filter(CV != k)
  testing.data <- data_monthly %>% filter(CV == k)
  # train model
  model_train <- lm(
    formula = formula_SLR_final ,  # <-
    data = training.data
  )
  # prediction
  prediction_test <- testing.data %>% 
    select(month , Station_name , NO2 , Type_of_station , CV) %>% 
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
  training.data <- data_monthly %>% filter(spatial_CV != k)
  testing.data <- data_monthly %>% filter(spatial_CV == k)
  # train model
  model_train <- lm(
    formula = formula_SLR_final ,  # <-
    data = training.data
  )
  # prediction
  prediction_test <- testing.data %>% 
    select(month , Station_name , NO2 , Type_of_station , spatial_CV) %>% 
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

# temporal cross validation
for(k in 1:12){
  # partition
  training.data <- data_monthly %>% filter(month != k)
  testing.data <- data_monthly %>% filter(month == k)
  # train model
  model_train <- lm(
    formula = formula_SLR_final ,  # <-
    data = training.data
  )
  # prediction
  prediction_test <- testing.data %>% 
    select(month , Station_name , NO2 , Type_of_station) %>% 
    # prediction
    mutate(predicted_temporalCV = predict(model_train , newdata = testing.data)) # <-
  # prediction data.frame
  if(as.character(k) == "1"){
    lm_SLR_prediction_CV_tp <- prediction_test # <-
  }else{ # append
    lm_SLR_prediction_CV_tp <- bind_rows(lm_SLR_prediction_CV_tp , prediction_test) # <-
  }
  # clean environment
  rm(training.data , testing.data , model_train , prediction_test , k)
}

# combine the prediction of the three CV
lm_SLR_prediction_CV <- lm_SLR_prediction_CV %>% 
  left_join(lm_SLR_prediction_CV_sp , 
            by = c("month" , "Station_name" , "NO2" , "Type_of_station") , 
            suffix = c("_CV" , "_spatialCV")) %>% 
  left_join(lm_SLR_prediction_CV_tp , 
            by = c("month" , "Station_name" , "NO2" , "Type_of_station"))




# //////////////////////////////////////////////////////////////////////////
# model evaluation
# //////////////////////////////////////////////////////////////////////////
lm_SLR <- lm(formula(formula_SLR_final) , data = data_monthly)

# =====================================
# model summary
# =====================================
summary(lm_SLR)

# =====================================
# observed, predicted, residuals
# =====================================
lm_SLR_prediction <- data_monthly %>% 
  select(month , Station_name , NO2 , Type_of_station , X , Y) %>% 
  # prediction
  mutate(predicted = predict(lm_SLR , newdata = data_monthly)) %>% 
  # CV-prediction
  full_join(lm_SLR_prediction_CV , 
            by = c("Station_name" , "NO2" , "Type_of_station" , "month")) %>% 
  # residuals
  mutate(residual = NO2 - predicted , 
         residual_CV = NO2 - predicted_CV , 
         residual_spatialCV = NO2 - predicted_spatialCV, 
         residual_temporalCV = NO2 - predicted_temporalCV)

# =====================================
# performance indices
# =====================================
# model performance indices as a data.frame
lm_SLR_indices <- lm_SLR_prediction %>% 
  eval_performance_indices()

# =====================================
# visualization
# =====================================
out_dirpath_plots <- sprintf("3_results/output-graph/model_monthly/%s" , model_abbr)
if(!dir.exists(out_dirpath_plots)) dir.create(out_dirpath_plots , recursive = TRUE)

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

# residuals by month
plot_resid_month(lm_SLR_prediction , 
                 subtitle_text = sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/residuals-month_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 6 , base_height = 3
)

# =====================================
# spatial autocorrelation of the residuals
# =====================================
moran_month_df <- eval_resid_moran(lm_SLR_prediction , by_time = TRUE , col_time = "month")
moran_mean_df <- eval_resid_moran(lm_SLR_prediction , by_time = FALSE)

# visualization: mean residuals
plot_resid_map(lm_SLR_prediction , # <-
               sprintf("%s (%s)" , str_to_title(model_name) , SAT_product))
save_plot(
  sprintf("%s/residual-map_%s_%s.png" , out_dirpath_plots , model_abbr , SAT_product) , 
  plot = last_plot() , 
  base_width = 6 , base_height = 4
)


# =====================================
# temporal autocorrelation of the residuals
# =====================================


# =====================================
# export datasets
# =====================================
{
  # export the model summary
  out_dirpath_lmsummary <- "3_results/output-data/model_monthly/SLR_summary"
  if(!dir.exists(out_dirpath_lmsummary)) dir.create(out_dirpath_lmsummary , recursive = TRUE)
  lm_SLR %>%
    broom::tidy() %>%
    write_csv(sprintf("%s/%s.csv" , out_dirpath_lmsummary , SAT_product))
  
  # export the predicted values
  out_dirpath_predicted <- "3_results/output-data/model_monthly/observed-predicted"
  if(!dir.exists(out_dirpath_predicted)) dir.create(out_dirpath_predicted , recursive = TRUE)
  lm_SLR_prediction %>% # <-
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_predicted , model_abbr , SAT_product))
  
  # export the model performance indices
  out_dirpath_indices <- "3_results/output-data/model_monthly/indices"
  if(!dir.exists(out_dirpath_indices)) dir.create(out_dirpath_indices , recursive = TRUE)
  lm_SLR_indices %>% # <- 
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/%s_%s.csv" , out_dirpath_indices , model_abbr , SAT_product))
  
  # Moran's I
  out_dirpath_Moran <- "3_results/output-data/model_monthly/Moran"
  if(!dir.exists(out_dirpath_Moran)) dir.create(out_dirpath_Moran , recursive = TRUE)
  moran_month_df %>% 
    pivot_longer(cols = -month) %>% 
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/month_%s_%s.csv" , out_dirpath_Moran , model_abbr , SAT_product))
  moran_mean_df %>% 
    pivot_longer(cols = everything()) %>% 
    mutate(model = model_abbr , product = SAT_product) %>%
    write_csv(sprintf("%s/mean_%s_%s.csv" , out_dirpath_Moran , model_abbr , SAT_product))
}

# =====================================
# export model
# =====================================
{
  out_dirpath_model <- "3_results/output-model/model_monthly"
  if(!dir.exists(out_dirpath_model)) dir.create(out_dirpath_model , recursive = TRUE)
  saveRDS(lm_SLR , # <-
          file = sprintf("%s/%s_%s.rds" , out_dirpath_model , model_abbr , SAT_product))
}

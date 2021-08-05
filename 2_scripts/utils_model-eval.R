#####################################################################################################
# Masterarbeit
# Modeling
# Utilities functions for model evaluation
# 2021-07-09
#####################################################################################################

# =====================================
# required packages
# =====================================
library(dplyr) ; library(tidyr)
library(sf)
library(ggplot2) ; library(ggsci) ; library(ggthemes) 
library(cowplot)
library(lubridate) ; library(stringr)
library(spdep)
library(Metrics)

# =====================================
# input dataset: model prediction data.frame
# =====================================
# example: 
# > lm_SLR_prediction
# # A tibble: 100 x 13
#   Station_name            NO2 Type_of_station       X       Y predicted CV    predicted_CV spatial_CV predicted_spatialCV residual residual_CV residual_spatialCV
#   <chr>                 <dbl> <chr>             <dbl>   <dbl>     <dbl> <fct>        <dbl> <fct>                    <dbl>    <dbl>       <dbl>              <dbl>
#   1 Aarau-Buchenhof        22.2 traffic         2646140 1248741     25.8  4            25.8  5                         25.4  -3.61        -3.66             -3.21  
#   2 Aigle                  20.0 industry        2561330 1129900     20.1  1            20.2  5                         20.4  -0.0267      -0.148            -0.359 
#   3 Airolo                 17.6 traffic         2690021 1153641     21.4  4            22.4  4                         22.9  -3.81        -4.87             -5.34  

# =====================================
# performance indices
# =====================================
# model performance indices as a data.frame
eval_performance_indices <- function(model_prediction_df){
  model_prediction_df %>%   # <-
    # remove columns with missing values 
    drop_na() %>% 
    # R2, RMSE
    pivot_longer(cols = starts_with("predicted") , values_to = "predicted") %>% 
    separate(name , into = c("name" , "type") , sep = "_") %>% 
    mutate(type = ifelse(is.na(type) , "training" , type)) %>% 
    group_by(type) %>% 
    summarize(R2 = cor(NO2 , predicted)^2 , 
              RMSE = rmse(NO2 , predicted)) %>% 
    pivot_longer(cols = c(R2 , RMSE)) %>% 
    # min/max of CV-R2/RMSE
    left_join(
      model_prediction_df %>%  # <-
        # remove columns with missing values 
        drop_na() %>% 
        # subset columns
        select(Station_name , NO2  , any_of(c("CV" , "spatial_CV" , "month" , "date")) , starts_with("predicted_")) %>% 
        # clean column names
        setNames(colnames(.) %>% str_replace("month" , "temporalCV") %>% str_replace("spatial_CV" , "spatialCV")) %>% 
        # pivot_longer
        mutate(across(any_of(c("CV" , "spatialCV" , "temporalCV")) , as.integer)) %>% 
        pivot_longer(cols = c(-Station_name , -NO2 , -any_of("date"))) %>% 
        # pivot_wider
        separate(col = name , into = c("name" , "type") , sep = "_" , fill = "left") %>% 
        mutate(name = ifelse(is.na(name) , "CV_fold" , name)) %>% 
        pivot_wider(names_from = name , values_from = value) %>% 
        # CV_fold as character
        mutate(CV_fold = as.character(CV_fold)) %>% 
        # R2, RMSE for each CV fold
        group_by(type , CV_fold) %>% 
        summarize(R2 = cor(NO2 , predicted)^2 , 
                  RMSE = rmse(NO2 , predicted)) %>% 
        ungroup() %>% 
        pivot_longer(cols = c(R2 , RMSE)) %>% 
        # min, max
        group_by(type , name) %>% 
        summarize(min = min(value) , 
                  max = max(value)) %>% 
        ungroup() , 
      by = c("name" , "type")
    ) %>% 
    # slope, intercept
    bind_rows(
      model_prediction_df %>%  # <- 
        pivot_longer(cols = starts_with("predicted") , 
                     names_to = "type" , values_to = "predicted" , 
                     names_prefix = "predicted_") %>% 
        mutate(type = str_replace(type , "predicted" , "training")) %>% 
        group_by(type) %>% 
        # linear regression observed ~ predicted
        group_modify( ~ lm(NO2 ~ predicted , data = .) %>% broom::tidy()) %>% 
        ungroup() %>% 
        select(type , term , estimate , std.error) %>% 
        # tidy
        rename(name = term , value = estimate) %>% 
        mutate(name = str_remove_all(name , "[:punct:]") %>% str_to_lower()) %>% 
        mutate(name = ifelse(name == "predicted" , "slope" , name)) %>% 
        # standard error
        mutate(min = value - 1.96*std.error , 
               max = value + 1.96*std.error) %>% 
        select(-std.error)
    )
}

# =====================================
# visualization
# =====================================
# predicted <-> observed
plot_obs_pred <- function(model_prediction_df , subtitle_text = NULL){
  model_prediction_df %>% 
    pivot_longer(cols = starts_with("predicted") , 
                 names_to = "type" , values_to = "predicted" , 
                 names_prefix = "predicted_") %>% 
    # rename for visualization
    mutate(type = type %>% 
             str_replace("predicted" , "Full model") %>% 
             str_replace("^CV$" , "CV") %>% 
             str_replace("spatialCV" , "Spatial CV") %>% 
             str_replace("temporalCV" , "Temporal CV")) %>% 
    # re-order for visualization
    mutate(type = factor(type , levels = c("Full model" , "CV" , "Spatial CV" , "Temporal CV"))) %>% 
    # visualization
    ggplot(aes(x = predicted , y = NO2)) +
    geom_abline(intercept = 0 , slope = 1 , color = "azure3") +
    geom_point(aes(color = Type_of_station) , shape = 1 , alpha = 0.8) +
    geom_smooth(method = "lm") +
    facet_grid(~type) +
    labs(x = expression("Model-estimated NO"[2]) , y = expression("Observed NO"[2]) , 
         title = "Observed vs estimated" , 
         subtitle = subtitle_text , 
         color = "Monitoring station type") +
    coord_fixed() +
    theme_bw() +
    theme(legend.position = "bottom")
}

# three residual diagnostic plots at the same time (obs<->resid, histogram, QQ-plot)
plot_resid <- function(model_prediction_df , title_text){
  plot_input_df <- model_prediction_df %>% 
    pivot_longer(cols = c(starts_with("residual") , starts_with("predicted"))) %>% 
    separate(name , into = c("name" , "type") , sep = "_") %>% 
    mutate(type = ifelse(is.na(type) , "Full model" , type)) %>% 
    pivot_wider(names_from = name , values_from = value) %>% 
    # rename for visualization
    mutate(type = type %>% 
             str_replace("^CV$" , "CV") %>% 
             str_replace("spatialCV" , "Spatial CV") %>% 
             str_replace("temporalCV" , "Temporal CV")) %>% 
    # re-order for visualization
    mutate(type = factor(type , levels = c("Full model" , "CV" , "Spatial CV" , "Temporal CV")))
  plot_grid(
    plot_input_df %>% 
      # visualization
      ggplot(aes(x = predicted , y = residual)) +
      geom_hline(yintercept = 0 , linetype = 2 , color = "grey50") +
      geom_point(aes(color = Type_of_station) , shape = 1 , alpha = 0.8) +
      geom_smooth() + 
      facet_grid(type~.) +
      # scale_color_discrete(labels = c("Background" , "Industry" , "Traffic") , 
      #                      guide = guide_legend(direction = "vertical", title.position = "top",
      #                                           label.position="top", label.hjust = 0.5, label.vjust = 0.5,
      #                                           label.theme = element_text(angle = 90 , size = 8))) +
      scale_color_discrete(labels = c("BG" , "IND" , "TRAF")) +
      labs(x = expression("Model-estimated NO"[2]) , y = "Residuals" , 
           color = "" , 
           subtitle = "Residuals vs fitted") +
      coord_fixed(ylim = c(min(plot_input_df$residual) , max(plot_input_df$residual))) +
      theme_bw() +
      theme(legend.position = "bottom" , 
            legend.margin = margin(-1,0,0,0,"pt")) , 
    # Histogram of the residuals
    plot_input_df %>% 
      # visualization
      ggplot(aes(x = residual)) +
      geom_density(color = "azure4" , fill = "azure" , alpha = 0.6) +
      geom_histogram(aes(y = ..density..) , fill = "azure3") +
      geom_density(color = "azure4") +
      geom_vline(aes(xintercept = mean(residual)) , color = "dodgerblue3") +
      geom_vline(xintercept = 0 , linetype = 2 , color = "white" , alpha = 0.9) +
      coord_flip(xlim = c(min(plot_input_df$residual) , max(plot_input_df$residual))) +
      facet_grid(type~.) +
      labs(x = "Residuals" , y = "Density" , 
           subtitle = "Residual histogram") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90 , vjust = 1 , hjust = 1)), 
    # residual QQ-plot
    plot_input_df %>% 
      group_by(type) %>% 
      mutate(residual = scale(residual)) %>% 
      ungroup %>% 
      # visualization
      ggplot(aes(sample = residual)) +
      stat_qq(shape = 1) + 
      stat_qq_line(color = "dodgerblue3") +
      facet_grid(type~. ) +
      labs(x = "Theoretical quantiles" , y = "Standardized residuals" , 
           subtitle = "Residual QQ-plot") +
      coord_fixed() +
      theme_bw() , 
    ncol = 3 , axis = "tb" , align = "h" , rel_widths = c(1,0.6,0.7)
  ) %>% 
    plot_grid(
      ggdraw() + 
        draw_label(title_text , x = 0, hjust = 0) +
        theme(plot.margin = margin(0, 0, 0, 15)) , 
      . , 
      ncol = 1,
      rel_heights = c(0.05, 1)
    )
}

# residuals by  (for monthly or daily models)
plot_resid_month <- function(model_prediction_df , subtitle_text = NULL , daily = FALSE){
  if(!daily){
    # monthly 
    model_prediction_df %>% 
      pivot_longer(cols = c(residual , residual_temporalCV) , values_to = "residual") %>% # 
      mutate(name = ifelse(str_detect(name , "temporalCV") , "Temporal CV" , "Full model")) %>% # 
      ggplot(aes(x = factor(month) , y = residual)) +
      geom_violin(fill = "grey50" , color = "grey30" , alpha = 0.5 , draw_quantiles = 0.5) +
      facet_grid(~name) + # 
      labs(x = "Month" , y = "Residuals" , 
           title = "Distribution of the model residuals by month" , 
           subtitle = subtitle_text) +
      theme_bw()
  }else{
    # daily
    model_prediction_df %>% 
      mutate(month = month(date)) %>% 
      pivot_longer(cols = c(residual , residual_temporalCV) , values_to = "residual") %>% # 
      mutate(name = ifelse(str_detect(name , "temporalCV") , "Temporal CV" , "Full model")) %>% # 
      # group_by(month) %>% 
      # summarise(residual = mean(residual , na.rm = TRUE)) %>% 
      # ungroup() %>% 
      ggplot(aes(x = factor(month) , y = residual)) +
      geom_violin(fill = "grey50" , color = "grey30" , alpha = 0.5 , draw_quantiles = 0.5) +
      facet_grid(~name) + # 
      labs(x = "Month" , y = "Residuals" , 
           title = "Distribution of residuals by month" , 
           subtitle = subtitle_text) +
      theme_bw()
  }
}

# =====================================
# spatial autocorrelation of the residuals
# =====================================
# visualization 
plot_resid_map <- function(model_prediction_df , subtitle_text = NULL){
  # Switzerland shapefile
  CH <- st_read("1_data/raw/Switzerland_shapefile/CHE_adm0.shp") %>% 
    st_transform(st_crs(2056))
  # residual map
  model_prediction_df %>% 
    select(Station_name , residual , X , Y) %>% 
    # aggregate to annual mean residual (for monthly and daily)
    group_by(Station_name , X , Y) %>% 
    summarize(residual = mean(residual , na.rm = TRUE)) %>% 
    ungroup() %>% 
    # convert to points
    st_as_sf(coords = c("X" , "Y") , crs = st_crs(2056) , remove = FALSE) %>% 
    # visualization
    ggplot(aes(color = residual)) +
    geom_sf(data = CH , fill = "grey85" , color = NA) +
    geom_sf() +
    scale_color_gradient2(low = "dodgerblue3" , mid = "white" , high = "deeppink3") +
    labs(title = "Spatial distribution of the model residuals"  , 
         subtitle = subtitle_text , 
         color = "Residuals") +
    theme_bw()
}

# Moran's I test
eval_resid_moran <- function(model_prediction_df , by_time = FALSE , col_time = NULL){
  if(by_time){
    # Moran's I test for every time step
    moran_df <- model_prediction_df %>% 
      select(Station_name , residual , X , Y , all_of(col_time)) %>% 
      # remove columns with missing values
      drop_na() %>% 
      # convert to points
      st_as_sf(coords = c("X" , "Y") , crs = st_crs(2056) , remove = FALSE) %>% 
      # Moran's I test for every time step
      group_by_at(col_time) %>% 
      group_modify( ~ {
        moran_test <- moran.test(.x$residual , 
                                 listw = st_distance(.x) %>% # distance matrix
                                   as.numeric() %>%
                                   matrix(nrow = nrow(.x)) %>%
                                   mat2listw(style = "W") , 
                                 zero.policy = TRUE)
        c(moran_test$estimate , p = moran_test$p.value) %>%
          as.list() %>% as_tibble()
      } 
      ) %>% 
      ungroup() %>% 
      # clean column names
      janitor::clean_names(case = "parsed")
  }else{
    # one Moran's I test for the mean residual (over time)
    moran_df <- model_prediction_df %>% 
      select(Station_name , residual , X , Y) %>% 
      # aggregate to annual mean residual (for monthly and daily)
      group_by(Station_name , X , Y) %>% 
      summarize(residual = mean(residual , na.rm = TRUE)) %>% 
      ungroup() %>% 
      # convert to points
      st_as_sf(coords = c("X" , "Y") , crs = st_crs(2056) , remove = FALSE) %>% 
      # Moran's I test
      group_modify( ~ {
        moran_test <- moran.test(.x$residual , 
                                 listw = st_distance(.x) %>% # distance matrix
                                   as.numeric() %>%
                                   matrix(nrow = nrow(.x)) %>%
                                   mat2listw(style = "W") , 
                                 zero.policy = TRUE)
        c(moran_test$estimate , p = moran_test$p.value) %>%
          as.list() %>% as_tibble()
      } 
      ) %>% 
      # clean column names
      janitor::clean_names(case = "parsed")
  }
  return(moran_df)
}


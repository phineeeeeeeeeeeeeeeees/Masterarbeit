#####################################################################################################
# Masterarbeit
# Modeling
# Comparison of the annual models
# 2021-06-29
#####################################################################################################

# =====================================
# required packages
# =====================================
library(readr) 
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(ggsci) ; library(ggthemes)
library(lubridate) ; library(stringr)

# =====================================
# load data
# =====================================
in_filepath_indices <- list.files("3_results/output-data/model_annual/indices" , pattern = ".csv$" , full.names = TRUE)
model_indices <- lapply(in_filepath_indices , read_csv) %>% 
  bind_rows()
model_moran <- lapply(list.files("3_results/output-data/model_annual/moran" , pattern = ".csv$" , full.names = TRUE) , 
                      read_csv) %>% 
  bind_rows()

# =====================================
# tidy table for presentation and paper
# =====================================
model_compare_tidy <- model_indices %>% 
  # round values
  mutate(across(c(value , min , max) , round , 3)) %>% 
  # value format: with ranges
  mutate(value = ifelse(if_any(c(min,max) , is.na) , 
                        value , 
                        sprintf("%s (%s~%s)" , value , min , max))) %>% 
  # re-order columns
  select(model , product , type , name , value) %>% 
  pivot_wider(names_from = c(type , name) , names_sep = "_" , values_from = value) %>% 
  # re-order rows
  mutate(model = factor(model , levels = c("SLR" , "GWR" , "RF" , "GBM" , "NN")) , 
         product = factor(product , levels = c("spatial" , "OMI" , "TROPOMI"))) %>% 
  arrange(model , product) %>% 
  # re-order columns
  select(model , product , 
         training_R2 , training_RMSE , training_slope , training_intercept , 
         CV_R2 , CV_RMSE , CV_slope , CV_intercept) %>% 
  # Moran's I
  left_join(
    model_moran %>% 
      pivot_wider(names_from = name , values_from = value) %>% 
      rename(MoransI = `Moran I statistic` , 
             MoransI.p = p) %>% 
      select(-Expectation , -Variance) , 
    by = c("model" , "product")
  )


model_compare_tidy %>% 
  write_csv("3_results/output-data/model_annual/model_annual_indices.csv")
# =====================================
# visualization and comparison
# =====================================
model_indices %>% 
  filter(name %in% c("R2" , "RMSE")) %>% 
  mutate(model = factor(model , levels = c("SLR" , "GWR" , "RF"))) %>% 
  ggplot(aes(x = model , y = value , fill = type)) +
  geom_bar( stat = "identity" , position = position_dodge(width = 0.9)) +
  geom_pointrange(aes(ymin = min , ymax = max) , 
                  size = 0.3 , position = position_dodge(width = 0.9)) +
  facet_grid(name~product , scales = "free_y") +
  scale_fill_jco(labels = c("5-fold CV" , "without CV") , name = "") +
  labs(title = "Model performance indices") +
  theme_bw()

model_indices %>% 
  filter(name %in% c("intercept" , "slope")) %>% 
  ggplot(aes(x = model , y = value , fill = type)) +
  geom_crossbar(aes(ymin = min , ymax = max) , 
                position = position_dodge(width = 0.9)) +
  facet_grid(name~product , scales = "free_y") +
  scale_fill_jco(labels = c("10-fold CV" , "without CV") , name = "") +
  labs(title = "Observed versus predicted") +
  theme_bw()


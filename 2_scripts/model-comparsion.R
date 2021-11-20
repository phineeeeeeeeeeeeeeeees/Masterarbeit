#####################################################################################################
# Masterarbeit
# Modeling
# Comparison of models with only TROPOMI
# 2021-10-19
#####################################################################################################

# =====================================
# required packages
# =====================================
library(readr) 
library(dplyr) ; library(tidyr) 
library(ggplot2) ; library(ggsci) ; library(ggthemes) ; library(cowplot)
library(lubridate) ; library(stringr)

# =====================================
# load data
# =====================================
# model performance indices 
model_indices <- list.files("3_results/output-data/model_daily/indices" , 
                            pattern = ".csv$" , full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows() %>% 
  mutate(model = factor(model , levels = c("SLR" , "GWR" , "SLMER" , "RF" , "XGB" , "LGB" , "NN")) , 
         product = factor(product , levels = c("OMI" , "TROPOMI"))) %>% 
  mutate(res = "daily") %>% 
  bind_rows(
    list.files("3_results/output-data/model_monthly/indices" , 
               pattern = ".csv$" , full.names = TRUE) %>% 
      lapply(read_csv) %>% 
      bind_rows() %>% 
      mutate(model = factor(model , levels = c("SLR" , "GWR" , "SLMER" , "RF" , "XGB" , "LGB" , "NN")) , 
             product = factor(product , levels = c("OMI" , "TROPOMI"))) %>% 
      mutate(res = "monthly")
  ) %>% 
  bind_rows(
    list.files("3_results/output-data/model_annual/indices" , 
               pattern = ".csv$" , full.names = TRUE) %>% 
      lapply(read_csv) %>% 
      bind_rows() %>% 
      mutate(model = factor(model , levels = c("SLR" , "GWR" , "SLMER" , "RF" , "XGB" , "LGB" , "NN")) , 
             product = factor(product , levels = c("OMI" , "TROPOMI"))) %>% 
      mutate(res = "annual")
  ) %>% 
  mutate(type = str_replace(type , "training" , "Training (full model)") %>% 
           str_replace("^CV$" , "Random-split CV") %>% 
           str_replace("^spatialCV$" , "Spatially-blocked CV") %>% 
           str_replace("^temporalCV$" , "Temporally-blocked CV") %>% 
           factor(levels = c("Training (full model)" , "Random-split CV" , "Spatially-blocked CV" , "Temporally-blocked CV")))


# =====================================
# model performance indices
# only TROPOMI
# =====================================
plot_comparison <- model_indices %>% 
  filter(product == "TROPOMI" & name == "R2") %>% 
  mutate(res = factor(res , levels = c("annual" , "monthly" , "daily"))) %>% 
  ggplot(aes(x = type , y = value)) +
  geom_bar(aes(fill = model) , stat = "identity" , 
           position = position_dodge(width = 0.7) , width = 0.6) +
  geom_errorbar(aes(ymin = min , ymax = max , group = model) , width = 0.2 ,
                position = position_dodge(width = 0.7) ) +
  facet_grid(res ~ . , scales = "free_y") +
  coord_cartesian(ylim = c(0.25,1)) +
  scale_fill_simpsons() +
  labs(y = "Model R-squared" , fill = "Models") +
  theme_bw() +
  theme(legend.position = "bottom" , axis.title.x = element_blank()) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))
  
save_plot(
  "3_results/output-graph/model_comparison_TROPOMI.png" ,
  plot_comparison ,
  base_height = 6 , base_width = 7
)

# =====================================
# comparison OMI TROPOMI
# =====================================
plot_grid(
  plotlist = model_indices %>% 
    filter(product %in% c("OMI" , "TROPOMI") & name == "R2") %>% 
    mutate(res = factor(res , levels = c("annual" , "monthly" , "daily"))) %>% 
    group_by(res) %>% 
    group_map({
      ~ ggplot() +
        geom_bar(
          data = filter(.x , product == "TROPOMI") , 
          aes(x = model , y = value) , 
          stat = "identity" , 
          fill = "azure3" ,
          width = 0.8 , 
          alpha = 0.8
        ) +
        geom_bar(
          data = filter(.x , product == "OMI") , 
          aes(x = model , y = value) , 
          stat = "identity" , 
          fill = "azure4" ,
          width = 0.3 , 
          alpha = 1
        ) +
        facet_grid(res ~ type) +
        coord_cartesian(ylim = c(0.35,1)) +
        scale_fill_simpsons() +
        labs(y = expression("Model R"^2)) +
        theme_bw() +
        theme(legend.position = "none" , 
              axis.title.x = element_blank() , 
              axis.text.x = element_text(size = 8 , angle = 45 , vjust = 1 , hjust = 1))
    } , .keep = TRUE)  , 
  ncol = 1 , align = "v" , axis = "lr" , rel_widths = c(3,4,4)
) %>% 
  plot_grid(
    ggdraw() + 
      draw_label("OMI and TROPOMI comparison" , x = 0, hjust = 0) +
      theme(plot.margin = margin(0, 0, 0, 15)) , 
    . , 
    ncol = 1,
    rel_heights = c(0.05, 1)
  )
save_plot(
  "3_results/output-graph/comparison_OMI_TROPOMI.png" ,
  last_plot() ,
  base_height = 6 , base_width = 8.5
)

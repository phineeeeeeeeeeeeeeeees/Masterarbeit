# install.packages("triplot")

library(triplot)

input_df <- data_annual %>% 
  # exclude rows with missing predictor values
  filter(!if_any(everything() , is.na)) %>% 
  # drop non-predictor columns
  select(-c(Station_name , spatial_CV , Type_of_zone , Type_of_station , Altitude , Canton_ID , Canton_name , X , Y))

model_test <- lm(
  NO2 ~ . , 
  data = input_df %>% select(NO2 , all_of(included_var$variable))
)

explain_test <- DALEX::explain(
  model = model_test , 
  data = input_df %>% select(all_of(included_var$variable)) , 
  y = input_df$NO2 , 
  verbose = FALSE
)

tri_test <- model_triplot(explain_test)

plot(tri_test)

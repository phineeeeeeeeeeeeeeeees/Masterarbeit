library(keras); library(tensorflow)

NN_define <- function(hyperparm_vector , n_var){
  # | hyperparm_vector: a named vector with the hyperparameters 
  # | | layers, neurons, epochs, batch.size , regularization, regularization_factor, dropout_rate
  # | n_var: number of input variables (for the input layer); ncol(predictor_matrix)
  # define model 
  NN <<- keras_model_sequential()
  # regularization
  if(is.na(hyperparm_vector["regularization"])){
    NN_regularizer <- NULL
  }else if(hyperparm_vector["regularization"] == 1){
    NN_regularizer <- regularizer_l1(l = hyperparm_vector["regularization_factor"])
  }else if(hyperparm_vector["regularization"] == 2) {
    NN_regularizer <- regularizer_l2(l = hyperparm_vector["regularization_factor"])
  }
  # define model layers
  if(hyperparm_vector["layers"] == 1){
    NN %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  input_shape = n_var , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dropout(rate = hyperparm_vector["dropout_rate"]) %>% 
      layer_dense(units = 1 , activation = "linear")
  }
  if(hyperparm_vector["layers"] == 2){
    NN %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  input_shape = n_var , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dropout(rate = hyperparm_vector["dropout_rate"]) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dense(units = 1 , activation = "linear")
  }
  if(hyperparm_vector["layers"] == 3){
    NN %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  input_shape = n_var , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dropout(rate = hyperparm_vector["dropout_rate"]) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dense(units = 1 , activation = "linear")
  }
  if(hyperparm_vector["layers"] == 4){
    NN %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  input_shape = n_var , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dropout(rate = hyperparm_vector["dropout_rate"]) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dense(units = hyperparm_vector["neurons"] , activation = "relu" , 
                  kernel_regularizer = NN_regularizer) %>% 
      layer_dense(units = 1 , activation = "linear")
  }
  # define loss and optimizer
  NN %>% 
    compile(
      loss = "mae" , 
      optimizer = optimizer_adam() , 
      metrics = list("mean_absolute_error")
    )
}
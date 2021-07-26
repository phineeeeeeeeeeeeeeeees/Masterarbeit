#####################################################################################################
# Masterarbeit
# configure the environment for keras and TensorFlow
# 2021-06-08
#####################################################################################################

library(tensorflow)
tensorflow::install_tensorflow()
library(keras)
keras::install_keras()
library(reticulate)
# create a new environment 
conda_create("r-reticulate")
# install SciPy
conda_install("r-reticulate", "scipy")
# import SciPy (it will be automatically discovered in "r-reticulate")
scipy <- import("scipy")
py_config()

# test
mnist <- dataset_mnist()

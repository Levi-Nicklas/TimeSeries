# Author: Levi C Nicklas
# Build Date: April 4, 2020
# Last Update: April 4, 2020.

ga_fitness <- function(binary_vector, features_list, training_data, dependent_var, dependent_var_position, OOS_data){
  ### This function is a combination of the two functions sourced below.
  ### The goal is to produce a "fitness" function by which the population
  ### will be judged. So, take a binary string --> OOS_rmse. 
  
  ### INPUTS:
  ### Please read the sourced function's documentation.
  
  source("../TS&F-R-Pack/functions/model_from_binary_vector.R")
  source("../TS&F-R-Pack/functions/OOS_rmse.R")
  
  model_t <- model_from_binary_vector(binary_vector, features_list, training_data, dependent_var)
  
  metric <- OOS_rmse(model_t, OOS_data = OOS_data, dependent_var_position = dependent_var_position)
  
  # GA function will MAXIMIZE, so flip the function.
  metric <- -metric
  
  return(metric)
}
ga_fitness <- function(binary_vector){
  ### This function is a combination of the two functions sourced below.
  ### The goal is to produce a "fitness" function by which the population
  ### will be judged. So, take a binary string --> OOS_rmse. 
  
  ### INPUTS:
  ### Please read the sourced function's documentation.
  features_list = all_terms
  training_data = DIS.trans[1:3650,]
  dependent_var = "d.High"
  dependent_var_position = 10
  OOS_data = DIS.trans
  
  
  
  source("Functions/model_from_binary_vector.R")
  source("Functions/OOS_rmse.R")
  
  model_t <- model_from_binary_vector(binary_vector, all_terms, training_data, dependent_var)
  
  metric <- OOS_rmse(model_t, OOS_data = OOS_data, dependent_var_position = dependent_var_position)
  
  # GA function will MAXIMIZE, so flip the function.
  metric <- -metric
  
  return(metric)
}
# Author: Levi C Nicklas
# Build Date: April 4, 2020
# Last Update: April 4, 2020.

model_from_binary_vector <- function(binary_vector, features_list, training_data, dependent_var){
  ### This function constructs a model from a 
  ### binary string. It is to be used in conjuntion
  ### with the genetic algorithm for feature selection.
  
  ### INPUTS:
  ## binary_vector - a vector of binary values, length n.
  ## feature_list - a vector of strings that represent possible features.
  ## training_data - a data frame which the model will be fit with. 
  ## dependent_var - a string of the name of the dependent var.
  
  
  
  temp_model_formula <- features_list[as.logical(binary_vector)]
  dependent_var <- paste0(dependent_var," ~ ")
  
  temp_model_formula <- paste0(dependent_var, 
                               paste0(temp_model_formula, collapse = "+"))
  
  temp_model <- lm(formula = formula(temp_model_formula),
                   data = training_data)
  
  return(temp_model)
}

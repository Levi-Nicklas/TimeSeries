# Author: Levi C Nicklas
# Build Date: April 4, 2020
# Last Update: April 4, 2020.


OOS_rmse <- function(model_to_eval, OOS_data, dependent_var_position){
  ### This function simply calculates the oos rmse for 
  ### a model given new data.
  
  ### INPUTS:
  ### model_to_eval - model object you want to test.
  ### OOS_data -  a dataframe, of some construction as
  ## that of the training data, but WITH new data, 
  ## if time series, include old data.
  ### dependent_var - a string of the name of the 
  ## dependent variable.
  
  y_hat <- predict(model_to_eval, new_data = OOS_data)
  y_obs <-  as.matrix(OOS_data[(nrow(OOS_data)-length(y_hat)+1):nrow(OOS_data),dependent_var_position])[,1]
  
  rmse_val <- sqrt(mean((y_hat - y_obs)^2))
  
  return(rmse_val)
}

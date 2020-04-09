# Author: Levi C Nicklas
# Build Date: April 8, 2020
# Last Update: April 8, 2020.

RWRMSE <- function(binary_model, terms_vector, dataframe, 
                   window_size, dep_var_string, dep_var_position){
  ### This function caluclates RMSE for each window
  ### of data. 
  
  ### INPUTS
  # binary_model -  a binary vector representation of the model.
  # terms_vector -  a vector with all the terms that the binary model
  #                 represents.
  # dataframe - your df.
  # window size - your window size. Keep your window large enough to include lags.
  # dep_var_string - name of your dependent variable in the df.
  # dep_var_position - the position of your dependent variable.
  
  # How many iterations to reach end of data.
  num_windows <- nrow(dataframe) - window_size
  # Allocate Storage
  rwrmse_values <- rep(0,num_windows)
  
  # Calculate RMSE for each window.
  for(i in 1:num_windows){
    # Build window
    window_i <- dataframe[i:(window_size+i),]
    oos_window <- dataframe[i:(window_size+i+1),]
    
    # Calculate Model.
    model_tmp <- model_from_binary_vector(model_tmp_bin,
                                          features_list = terms_vector,
                                          training_data = window_i,
                                          dependent_var = dep_var_string)
    
    # Calculate OOS RMSE
    source("../TS&F-R-Pack/functions/OOS_rmse.R")
    value <- OOS_rmse(model_tmp,oos_window, dep_var_position)
    rwrmse_values[i] <- value
  }
  return(rwrmse_values)
}

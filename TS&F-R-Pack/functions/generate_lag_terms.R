# Author: Levi C Nicklas
# Build Date: April 3, 2020
# Last Update: April 3, 2020.

generate_lag_terms <- function(var_names, max_lags){
  ### The purpose of this function is to take the output from
  ### the `build_lag_terms.R` function, and reorganize that data
  ### into a neat dataframe that can be more easily utilized. 

  ### INPUTS:
  ## var_names - an vector of strings that will be the variable 
  # names.
  ## max_lags - an integer that represents the largest lag you
  # wish to consider.
  
  ### Dependencies
  ## None!
  
  num_vars <- length(var_names)
    
  # Allocate Space.
  reg_terms_df <- as.data.frame(matrix(data = rep(0, num_vars * max_lags),
                                       ncol = num_vars))
  
  # Get required functions.
  source("../TS&F-R-Pack/functions/build_lag_terms.R")
  
  # restructure and store the terms.
  for(i in 1:length(var_names)){
    # See build_lag_terms documentation.
    reg_terms_df[,(i)] <- build_lag_terms(var_names[i], max_lags)
    
  }
  # Return a *neat* dataframe.
  colnames(reg_terms_df) <- var_names
  return(reg_terms_df)
}
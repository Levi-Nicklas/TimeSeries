# Author: Levi C Nicklas
# Build Date: April 3, 2020
# Last Update: April 3, 2020.


build_lag_terms <- function(var_name, max_num_lag){
  ### This is a function which was constructed to 
  ### build out lag formulas to later be used in 
  ### regression. This CURRENTLY is constructed to 
  ### build out all formulas 1-max. At a later time
  ### build functionality to specify a minimum, and
  ### a step parameter (eg. every second lag).
  
  
  ### INPUTS:
  ## var_name - feed a single string which is the 
  #  vairable name in concern.
  ## max_num_lag - feed an integer that is the 
  # largest lag you wish to consider.
  
  ### Dependencies
  # None!
  
  
  # Allocate Storage.
  lag_store <- 1:max_num_lag
  
  # Build each lag term 1-max.
  for(i in 1:max_num_lag){
    # Create lag formula.
    lag_temp <- paste0("lag(", var_name, ",",
                       i, ")")
    
    #Store lag formula.
    lag_store[i] <- lag_temp
  }
  
  # Provide lag storage.
  return(lag_store)
}
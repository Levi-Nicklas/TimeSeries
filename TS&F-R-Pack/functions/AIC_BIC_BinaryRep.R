# Author: Levi C Nicklas
# Build Date: April 6, 2020
# Last Update: April 6, 2020.


AIC_BIC_BinaryRep <- function(binary_vector, lag_term_vector, all_term_vector, train_df, dependent_var_name){
  ### This function constructs a model from a 
  ### binary string, and then evaluates the AIC/BIC values.
  
  ### INPUTS:
  ## binary_vector - a vector of binary values, length n.
  ## lag_term_vector - a vector of only the lag terms.
  ## all_term_vector - a vector of all lags and seasonal indicators in the model.
  ## train_df - data frame with training data.
  ## dependent_var_name - a string of the name for the dependent variable.
  
  source("../TS&F-R-Pack/functions/model_from_binary_vector.R")
  
  # Get the number of terms.
  num_terms <- length(lag_term_vector)
  # Add the set seasonal indicators I want to the binary vector.
  binary_vector <- append(binary_vector, c(rep(T,11),F), after = num_terms)
  
  model_t <- model_from_binary_vector(binary_vector = binary_vector, 
                                      features_list = all_term_vector,
                                      training_data = train_df,
                                      dependent_var = dependent_var_name)

  metrics <- c(
    AIC(model_t),
    BIC(model_t)
  )

  
  
  return(metrics)
}

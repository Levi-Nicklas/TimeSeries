# Author: Levi C Nicklas
# Build Date: April 3, 2020
# Last Update: April 3, 2020.

build_seasonal_indicators <- function(seasonality){
  ### The only purpose of this function is so that
  ### I stop having to hard code these seasonality
  ### Indicators. This will later have to be paired
  ### with some sort of conditional which actually
  ### checks the date. May add tri-annual later.
  
  ### INPUTS:
  ## seasonality - "monthly", "quarterly", or 
  # "bi-annual".
  
  ### Dependencies
  ## None.
    
  if(seasonality == "monthly"){
    month_list <- c("Jan", "Feb", "Mar", "Apr",
                   "May", "Jun", "Jul", "Aug",
                   "Sep", "Oct", "Nov", "Dec")
    
    month_list <- data.frame(months = month_list)
    
    return(month_list)
    
  } else if(seasonality == "quarterly"){
    quarter_list <- c("Q1", "Q2", "Q3", "Q4")
    
    quarter_list <- data.frame(quarters = quarter_list)
    
    return(quarter_list)
    
  } else if(seasonality == "bi-annual"){
    biannual_list <- c("first_half","second_half")
    
    biannual_list <- data.frame(halfs = biannual_list)
    
    return(biannual_list)
  } else if(seasonality == "weekly"){
    weekly_list <- c("monday","tuesday","wednesday","thursday","friday")
    
    weekly_list <- data.frame(day_of_week = weekly_list)
  }
}
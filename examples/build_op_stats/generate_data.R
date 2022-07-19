library(dplyr)


generate_c03_data <- function( num_vars=10, 
                               num_obs=10,
                               replicates=1,
                               meas_min_val=0, 
                               meas_max_val=1 ){
  total_vals   <- num_vars * num_obs * replicates
  # generate data
  var_vals    <- sprintf(paste0("var%0", nchar(as.character(num_vars)), "d"), 1:num_vars)
  obs_vals    <- sort(rep(sprintf(paste0("obs%0", nchar(as.character(num_obs)), "d"), 1:num_obs), num_vars))
  
  data <- data.frame(Observation = obs_vals, 
                     Variable = var_vals, 
                     Measurement = runif(total_vals, meas_min_val, meas_max_val))
  
  return(data)
  
}

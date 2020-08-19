calculate_rmse <- function(target_name, actual_fn, predicted_fn) {
  
  # Setup data to be able to merge and retain columns
  
  actual_df <- read_csv(actual_fn, col_types = cols()) %>% 
    select(site_id, date, 
           obs_opti_hab = opti_hab, 
           obs_therm_hab = therm_hab,
           obs_opti_therm_hab = opti_therm_hab)
  
  predicted_df <- read_csv(predicted_fn, col_types = cols()) %>% 
    select(site_id, date, 
           pred_opti_hab = opti_hab, 
           pred_therm_hab = therm_hab,
           pred_opti_therm_hab = opti_therm_hab)
  
  # Join the data and then calculate rmse per habitat for each lake
  
  actual_df %>% 
    left_join(predicted_df, by = c("site_id", "date")) %>% 
    group_by(site_id, date) %>% 
    summarize(rmse_opti_hab = rmse(obs_opti_hab, pred_opti_hab),
              rmse_therm_hab = rmse(obs_therm_hab, pred_therm_hab),
              rmse_opti_therm_hab = rmse(obs_opti_therm_hab, pred_opti_therm_hab))  %>% 
    write_csv(target_name)
  
}

rmse <- function(obs, pred) {
  rmse_val <- sqrt(mean((pred - obs)^2, na.rm = TRUE))  
  if(is.nan(rmse_val)) {
    # If all vals of one vector were NA, then mean could be NaN
    # If that is the case, just return NA and don't calc sqrt
    rmse_val <- NA
  }
  return(rmse_val)
}


calculate_annual_metrics <- function(target_name, site_ids, file_pattern) {
  
  purrr::map(site_ids, function(id) {
    
    data_ready <- read_feather(sprintf(file_pattern, id)) %>% 
      select(site_id, date = DateTime, starts_with("temp_")) %>% 
      pivot_longer(cols = starts_with("temp_"), names_to = "depth", values_to = "wtr") %>% 
      mutate(depth = as.numeric(gsub("temp_", "", depth))) %>% 
      mutate(year = as.numeric(format(date, "%Y")))
    
    annual_metrics <- data_ready %>% 
      group_by(site_id, year) %>% 
      summarize(
        # winter_dur_0_4 = winter_dur_0_4(),
        # coef_var_30_60 = coef_var_30_60(),
        # coef_var_0_30 = coef_var_0_30(),
        # stratification_onset_yday = stratification_onset_yday(),
        # stratification_duration = stratification_duration(),
        # sthermo_depth_mean = sthermo_depth_mean(),
        
        peak_temp = peak_temp(date, wtr, depth),
        gdd_wtr_0c = calc_gdd(wtr, 0),
        gdd_wtr_5c = calc_gdd(wtr, 5),
        gdd_wtr_10c = calc_gdd(wtr, 10),
        
        # bottom_temp_at_strat = bottom_temp_at_strat(),
        # schmidt_daily_annual_sum = schmidt_daily_annual_sum(),
        
        mean_surf_jas = calc_monthly_summary_stat(date, wtr, depth, "mean", 0, "surf", month_nums_to_grp = 7:9),
        max_surf_jas = calc_monthly_summary_stat(date, wtr, depth, "max", 0, "surf", month_nums_to_grp = 7:9),
        mean_bot_jas = calc_monthly_summary_stat(date, wtr, depth, "mean", calc_lake_bottom(depth), "bot", month_nums_to_grp = 7:9),
        max_bot_jas = calc_monthly_summary_stat(date, wtr, depth, "max", calc_lake_bottom(depth), "bot", month_nums_to_grp = 7:9),
        
        # Per month calcs return a data.frame and get "unpacked" below 
        mean_surf_mon = calc_monthly_summary_stat(date, wtr, depth, "mean", 0, "surf"),
        max_surf_mon = calc_monthly_summary_stat(date, wtr, depth, "max", 0, "surf"),
        mean_bot_mon = calc_monthly_summary_stat(date, wtr, depth, "mean", calc_lake_bottom(depth), "bot"),
        max_bot_mon = calc_monthly_summary_stat(date, wtr, depth, "max", calc_lake_bottom(depth), "bot"),
        
        .groups = "keep" # suppresses message about regrouping
      ) %>% 
      unpack(cols = c(mean_surf_mon, max_surf_mon, mean_bot_mon, max_bot_mon,
                      mean_surf_jas, max_surf_jas, mean_bot_jas, max_bot_jas))
    
    message(sprintf("Completed annual metrics for %s/%s", which(site_ids == id), length(site_ids)))
    
    return(annual_metrics)
  }) %>% 
    bind_rows() %>% 
    readr::write_csv(target_name)
  
}

# Collection of functions to calculate annual thermal metrics
# All accept the same main data input, `date` and `wtr`.

# It is assumed that these functions only get passed one
# years worth of data.

#' @param date a vector (or one column) with the dates of class `Date`
#' @param wtr a vector (or one column) with water temperature values in deg C

# Some also have the following parameters:
#' @param depth vector (or column) with depth in meters

#' @description # of days, beginning October 30 of the previous year and running 
#' through June of the given year, that surface water is <4 degrees C
winter_dur_0_4 <- function(date, wtr) {
  
}

#' @description Coefficient of Variation of surface temperature from 30-60 days post ice off.
coef_var_30_60 <- function(date, wtr) {
  # ice dates are in 4_inputs in sciencebase
}

#' @description Coefficient of Variation of surface temperature from 0-30 days post ice off.
coef_var_0_30 <- function(date, wtr) {
  
}

#' @description Julian date at which stratification sets up (longest stratified period)
stratification_onset_yday <- function(date, wtr) {
  
}

#' @description Duration of stratified period
stratification_duration <- function(date, wtr) {
  
}

#' @description avg. thermocline depth in stratified period
sthermo_depth_mean <- function(date, wtr) {
  
}

#' @description Maximum observed surface temperature
peak_temp <- function(date, wtr, depth) {
  max(wtr[which(depth == 0)], na.rm = TRUE)
}

#' @description water temperature 0.1m from lake bottom on day of 
#' stratification (as defined in original stratification measure)
bottom_temp_at_strat <- function(date, wtr) {
  
}

#' @description Sum of daily Schmidt Stability values for calendar year.
schmidt_daily_annual_sum <- function(date, wtr) {
  
}

#' @description mean, max, or min of a specific depth's temperature for a month or group of months
calc_monthly_summary_stat <- function(date, wtr, depth, stat_type, depth_to_use, depth_prefix, month_nums_to_grp = NULL) {
  
  calc_summary_stat <- switch(
    stat_type,
    "mean" = mean,
    "min" = min,
    "max" = max,
    stop(sprintf("A `stat_type` of '%s' is not currently supported", stat_type))
  )
  
  data.frame(date, depth = depth, wtr = wtr) %>% 
    
    # Calculate water temp at the desired depth
    # If `depth_to_use` exists in `depth`, this method still works
    group_by(date) %>% 
    summarize(wtr_at_depth = find_wtr_at_depth(wtr, depth, depth_to_find = depth_to_use), .groups = "keep") %>% 
    ungroup() %>% 
    
    # Sort months in annual order
    mutate(month = tolower(format(date, "%b")),
           month_num = as.numeric(format(date, "%m"))) %>% 
    arrange(month_num) %>%
    
    # Group months if case that is desired
    { 
      if(!is.null(month_nums_to_grp))  
        # First, filter out any month that is not part of the group 
        filter(., month_num %in% month_nums_to_grp) %>% 
        # Then, make them all the same group by replacing "month"
        # with a string that is the made up month abbreviations
        mutate(month = paste(month.abb[sort(month_nums_to_grp)], collapse=""))  
      else .
    } %>% 
    
    # Then find the mean for each month
    group_by(month) %>% 
    summarize(stat_surf = calc_summary_stat(wtr_at_depth, na.rm = TRUE), .groups = "keep") %>% 
    pivot_wider(names_from = month, values_from = stat_surf, names_prefix = sprintf("%s_%s_", stat_type, depth_prefix)) %>% 
    ungroup()
}

## Helper functions for the above

#' @description Cumulative sum of degrees >base degrees for entire year
calc_gdd <- function(wtr, base = 0) {
  sum(wtr[wtr > base])
}

# Uses 0.1 m from the bottom as the "bottom"
calc_lake_bottom <- function(depth) {
  lake_actual_bottom <- tail(unique(sort(depth)), 1)
  return(lake_actual_bottom - 0.1)
}

# Doesn't consider dates at all
# Assumes linear interpolation for depth-wtr relationship
find_wtr_at_depth <- function(wtr, depth, depth_to_find) {
  approx(depth, wtr, depth_to_find)$y
}

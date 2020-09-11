
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
        gdd_wtr_0c = gdd_wtr_0c(date, wtr),
        gdd_wtr_5c = gdd_wtr_5c(date, wtr),
        gdd_wtr_10c = gdd_wtr_10c(date, wtr),
        
        # bottom_temp_at_strat = bottom_temp_at_strat(),
        # schmidt_daily_annual_sum = schmidt_daily_annual_sum(),
        
        mean_surf_jas = mean_surf_jas(date, wtr, depth),
        max_surf_jas = max_surf_jas(date, wtr, depth),
        mean_bot_jas = mean_bot_jas(date, wtr, depth),
        max_bot_jas = max_bot_jas(date, wtr, depth),
        
        # Per month calcs return a data.frame and get "unpacked" below 
        mean_surf_mon = mean_surf_mon(date, wtr, depth),
        max_surf_mon = max_surf_mon(date, wtr, depth),
        mean_bot_mon = mean_bot_mon(date, wtr, depth),
        max_bot_mon = max_bot_mon(date, wtr, depth),
        
        .groups = "keep" # suppresses message about regrouping
      ) %>% 
      unpack(cols = c(mean_surf_mon, max_surf_mon, 
                      mean_bot_mon, max_bot_mon))
    
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

#' @description Cumulative sum of degrees > 0 for entire year
gdd_wtr_0c <- function(date, wtr) {
  calc_gdd(wtr, 0)
}

#' @description Cumulative sum of degrees > 5 for entire year
gdd_wtr_5c <- function(date, wtr) {
  calc_gdd(wtr, 5)
 }

#' @description Cumulative sum of degrees > 10 for entire year
gdd_wtr_10c <- function(date, wtr) {
  calc_gdd(wtr, 10)
}

#' @description water temperature 0.1m from lake bottom on day of 
#' stratification (as defined in original stratification measure)
bottom_temp_at_strat <- function(date, wtr) {
  
}

#' @description Sum of daily Schmidt Stability values for calendar year.
schmidt_daily_annual_sum <- function(date, wtr) {
  
}

#' @description mean of surface temperature from July, August, September
mean_surf_jas <- function(date, wtr, depth) {
  mean(wtr[which(is_jas(date) & depth == 0)], na.rm = TRUE)
}

#' @description max of surface temperature from July, August, September
max_surf_jas <- function(date, wtr, depth) {
  max(wtr[which(is_jas(date) & depth == 0)], na.rm = TRUE)
}

#' @description mean of bottom temperature (0.1 m from bottom) from July, August, September
mean_bot_jas <- function(date, wtr, depth) {
  purrr::map(unique(date), function(date_i) {
    if(is_jas(date_i)) {
      wtr_bot <- find_wtr_at_lake_bottom(wtr[date == date_i], depth[date == date_i])
    } else {
      wtr_bot <- NA
    }
    return(wtr_bot)
  }) %>% 
    purrr::reduce(c) %>% 
    mean(na.rm = TRUE)
}

#' @description max of bottom temperature (0.1 m from bottom) from July, August, September
max_bot_jas <- function(date, wtr, depth) {
  purrr::map(unique(date), function(date_i) {
    if(is_jas(date_i)) {
      wtr_bot <- find_wtr_at_lake_bottom(wtr[date == date_i], depth[date == date_i])
    } else {
      wtr_bot <- NA
    }
    return(wtr_bot)
  }) %>% 
    purrr::reduce(c) %>% 
    max(na.rm = TRUE)
}

## Need to do the following fxns for each month.
# TODO: think strategically about how to implement for each month.

#' @description mean of surface temperature for a month
mean_surf_mon <- function(date, wtr, depth) {
  data.frame(date, month = tolower(format(date, "%b")), depth = depth, wtr = wtr) %>% 
    # Get just surface temps
    filter(depth == 0) %>% 
    
    # Sort months in annual order
    arrange(as.numeric(format(date, "%m"))) %>%
    
    # Find mean for each month
    group_by(month) %>% 
    summarize(mean_surf = mean(wtr, na.rm = TRUE), .groups = "keep") %>% 
    pivot_wider(names_from = month, values_from = mean_surf, names_prefix = "mean_surf_")
}

#' @description max of surface temperature for each month
max_surf_mon <- function(date, wtr, depth) {
  data.frame(date, month = tolower(format(date, "%b")), depth = depth, wtr = wtr) %>% 
    # Get just surface temps
    filter(depth == 0) %>% 
    
    # Sort months in annual order
    arrange(as.numeric(format(date, "%m"))) %>%
    
    # Find max for each month
    group_by(month) %>% 
    summarize(max_surf = max(wtr, na.rm = TRUE), .groups = "keep") %>% 
    pivot_wider(names_from = month, values_from = max_surf, names_prefix = "max_surf_") %>% 
    ungroup()
}

#' @description mean of bottom temperature for each month
mean_bot_mon <- function(date, wtr, depth) {
  data.frame(date, month = tolower(format(date, "%b")), depth = depth, wtr = wtr) %>% 
    
    # Calculate water temp at the bottom
    group_by(date) %>% 
    mutate(wtr_bot = find_wtr_at_lake_bottom(wtr, depth)) %>% 
    ungroup() %>% 
    
    # Sort months in annual order
    arrange(as.numeric(format(date, "%m"))) %>% 
    
    # Then find the mean for each month
    group_by(month) %>% 
    summarize(mean_bot = mean(wtr_bot, na.rm = TRUE), .groups = "keep") %>% 
    pivot_wider(names_from = month, values_from = mean_bot, names_prefix = "mean_bot_") %>% 
    ungroup()
}

#' @description max of bottom temperature for each month
max_bot_mon <- function(date, wtr, depth) {
  data.frame(date, month = tolower(format(date, "%b")), depth = depth, wtr = wtr) %>% 
    # Calculate water temp at the bottom
    group_by(date) %>% 
    mutate(wtr_bot = find_wtr_at_lake_bottom(wtr, depth)) %>% 
    ungroup() %>%  
    
    # Sort months in annual order
    arrange(as.numeric(format(date, "%m"))) %>% 
    
    # Then find the mean for each month
    group_by(month) %>% 
    summarize(max_bot = max(wtr_bot, na.rm = TRUE), .groups = "keep") %>% 
    pivot_wider(names_from = month, values_from = max_bot, names_prefix = "max_bot_") %>% 
    ungroup()
}

## Helper functions for the above

calc_gdd <- function(wtr, base = 0) {
  sum(wtr[wtr > base])
}

# Identify if dates are in July, August, and September
is_jas <- function(date) {
  as.numeric(format(date, "%m")) %in% c(7, 8, 9)
}

# Doesn't consider dates at all
# Assumes linear interpolation for depth-wtr relationship
# Uses 0.1 m from the bottom as the "bottom"
find_wtr_at_lake_bottom <- function(wtr, depth) {
  lake_actual_bottom <- tail(unique(sort(depth)), 1)
  wtr_bot <- approx(depth, wtr, lake_actual_bottom - 0.1)$y
  return(wtr_bot)
}

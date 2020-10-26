
calculate_annual_metrics_per_lake <- function(site_id, site_file, ice_file, morphometry) {
  
  start_tm <- Sys.time()
    
  data_ready <- read_feather(site_file) %>% 
    select(site_id, date = DateTime, starts_with("temp_")) %>% 
    pivot_longer(cols = starts_with("temp_"), names_to = "depth", values_to = "wtr") %>% 
    mutate(depth = as.numeric(gsub("temp_", "", depth))) %>% 
    mutate(year = as.numeric(format(date, "%Y")))
  
  # Get hypso for this site
  hypso <- data.frame(H = morphometry$H, A = morphometry$A) %>% 
    mutate(depths = max(H) - H, areas = A) %>% 
    arrange(depths) %>% 
    select(depths, areas)
  
  data_stratification <- data_ready %>% 
    group_by(date) %>% 
    summarize(wtr_surf_daily = wtr[depth == 0],
              wtr_bot_daily = find_wtr_at_depth(wtr, depth, calc_lake_bottom(depth)),
              .groups = "keep") %>% 
    mutate(stratified = is_stratified(wtr_surf_daily, wtr_bot_daily, force_warm = TRUE)) %>% 
    ungroup() %>% 
    # Add year back in because it is dropped in `group_by` above
    mutate(year = as.numeric(format(date, "%Y"))) %>% 
    group_by(year) %>% 
    # For each year, add a column to say whether a day is within the longest chunk of consecutive stratified days
    mutate(in_stratified_period = is_in_longest_consective_chunk(stratified)) %>% 
    ungroup() %>% 
    select(-year)
  
  data_ready_with_flags <- data_ready %>% 
    # Read and join ice flags for this site
    left_join(read_csv(ice_file, col_types = cols()), by = "date") %>% 
    arrange(date, depth) %>% # Data was coming in correct, but just making sure 
    # Add flag to say if the day is stratified or not. Doing this here because it will be used
    # by multiple annual metrics below.
    left_join(data_stratification, by = "date") # these were calculated by day and by adding into a long format data set, they will be duplicated
  
  annual_metrics <- data_ready_with_flags %>%
    group_by(site_id, year) %>% 
    summarize(
      
      # Maximum observed surface temperature & corresponding date
      peak_temp = max(wtr_surf_daily, na.rm = TRUE),
      peak_temp_dt = date[which.max(wtr_surf_daily)],
      
      ice_on_date = get_ice_onoff(date, ice, peak_temp_dt, "on"),
      ice_off_date = get_ice_onoff(date, ice, peak_temp_dt, "off"),
      
      winter_dur_0_4 = winter_dur_0_4(date, wtr, depth, prev_yr_data=get_last_years_data(unique(year), data_ready)),
      coef_var_31_60 = coef_var(date, wtr_surf_daily, ice_off_date = ice_off_date, c(31,60)),
      coef_var_1_30 = coef_var(date, wtr_surf_daily, ice_off_date = ice_off_date, c(1,30)),
      
      # Metrics that deal with the stratified period
      stratification_onset_yday = stratification_onset_yday(date, in_stratified_period),
      stratification_duration = stratification_duration(date, in_stratified_period),
      sthermo_depth_mean = sthermo_depth_mean(date, depth, wtr, in_stratified_period, ice_on_date, ice_off_date),
      bottom_temp_at_strat = bottom_temp_at_strat(date, wtr_bot_daily, unique(year), stratification_onset_yday),

      gdd_wtr_0c = calc_gdd(date, wtr_surf_daily, 0),
      gdd_wtr_5c = calc_gdd(date, wtr_surf_daily, 5),
      gdd_wtr_10c = calc_gdd(date, wtr_surf_daily, 10),
      schmidt_daily_annual_sum = schmidt_daily_annual_sum(date, depth, wtr, ice_on_date, ice_off_date, hypso),

      # The following section of metrics return a data.frame per summarize command and
      #   are unpacked into their real columns after using `unpack`

      mean_surf_jas = calc_monthly_summary_stat(date, wtr_surf_daily, "surf", "mean", month_nums_to_grp = 7:9),
      max_surf_jas = calc_monthly_summary_stat(date, wtr_surf_daily, "surf", "max", month_nums_to_grp = 7:9),
      mean_bot_jas = calc_monthly_summary_stat(date, wtr_bot_daily, "bot", "mean", month_nums_to_grp = 7:9),
      max_bot_jas = calc_monthly_summary_stat(date, wtr_bot_daily, "bot", "max", month_nums_to_grp = 7:9),

      mean_surf_mon = calc_monthly_summary_stat(date, wtr_surf_daily, "surf", "mean"),
      max_surf_mon = calc_monthly_summary_stat(date, wtr_surf_daily, "surf", "max"),
      mean_bot_mon = calc_monthly_summary_stat(date, wtr_bot_daily, "bot", "mean"),
      max_bot_mon = calc_monthly_summary_stat(date, wtr_bot_daily, "bot", "max"),

      # Hansen metrics
      # See https://github.com/USGS-R/necsc-lake-modeling/blob/d37377ea422b9be324e8bd203fc6eecc36966401/data/habitat_metrics_table_GH.csv

      days_height_vol_in_range = calc_days_height_vol_within_range(date, depth, wtr, hypso,
                                                                   temp_low = c(12, 10.6, 18.2, 18, 19.3, 19, 20.6, 20, 22, 23, 25, 26.2, 26, 26, 28, 28, 29, 30),
                                                                   temp_high = c(28, 11.2, 28.2, 22, 23.3, 23, 23.2, 30, 23, 31, 29, 32, 28, 30, 29, 32, 100, 31)),

      spring_days_in_10.5_15.5 = spring_days_incub(date, wtr_surf_daily, c(10.5, 15.5)),
      post_ice_warm_rate = post_ice_warm_rate(date, wtr_surf_daily, ice_off_date),
      date_over_temps = calc_first_day_above_temp(date, wtr_surf_daily, temperatures = c(8.9, 16.7, 18, 21)), # Returns a df and needs to be unpacked below

      .groups = "keep" # suppresses message about regrouping
    ) %>% 
    unpack(cols = c(mean_surf_mon, max_surf_mon, mean_bot_mon, max_bot_mon,
                    mean_surf_jas, max_surf_jas, mean_bot_jas, max_bot_jas,
                    date_over_temps, days_height_vol_in_range)) %>%
    ungroup()
  
  message(sprintf("Completed annual metrics for %s in %s min", site_id, 
                  round(as.numeric(Sys.time() - start_tm, units = "mins"), 2)))
  
  return(annual_metrics)

}

get_filenames_from_ind <- function(ind_file) {
  names(yaml::read_yaml(ind_file))
}

# Collection of functions to calculate annual thermal metrics
# All accept the same main data input, `date` and `wtr`.

# It is assumed that these functions only get passed one
# years worth of data.

#' @param date a vector (or one column) with the dates of class `Date`
#' @param wtr a vector (or one column) with water temperature values in deg C
#' @param depth vector (or column) with depth in meters

#' @description # of days, beginning October 30 of the previous year and running 
#' through June of the given year, that surface water is <4 degrees C
winter_dur_0_4 <- function(this_yr_date, this_yr_wtr, this_yr_depth, prev_yr_data) {
  
  if(nrow(prev_yr_data) == 0) {
    # TODO: should this allow partial data? For example, if the dates started in 
    #   May, it may return 0 for that year. May need to skip and return NA.
    start_date <- min(this_yr_date)
  } else {
    start_date <- unique(sprintf("%s-10-30", format(prev_yr_data$date, "%Y")))
  }
  end_date <- unique(sprintf("%s-06-30", format(this_yr_date, "%Y")))
  
  data.frame(date = c(prev_yr_data$date, this_yr_date), 
             wtr = c(prev_yr_data$wtr, this_yr_wtr),
             depth = c(prev_yr_data$depth, this_yr_depth)) %>% 
    filter(
      date >= start_date & date <= end_date, # Oct 30 prev year to June 30 this year
      depth == 0, # surface water
      wtr < 4 # TODO: What about negatives? Should it be 0-4 or just less than 4?
    ) %>% 
    nrow()
   
}

#' @description Coefficient of Variation of surface temperature from a range of days 
#' post ice off. This calculation is the same as Winslow 2017, but that one wasn't 
#' named intuitively (`coef_var_0_30` instead of `coef_var_1_30`)
coef_var <- function(date, wtr_surf, ice_off_date, day_post_range) {
  
  # Need one dat & wtr_surf per day
  date_unique <- unique_day(date)
  wtr_surf_unique <- unique_day_data(date, wtr_surf)
  
  wtr_post_ice <- get_wtr_post_ice_off(date_unique, wtr_surf_unique, ice_off_date, day_post_range)
  
  sd(wtr_post_ice)/mean(wtr_post_ice) # TODO: Use `na.rm = TRUE`?
}

#' @description Julian date at which stratification sets up (longest stratified period)
stratification_onset_yday <- function(date, stratified_period) {
  # https://stackoverflow.com/questions/37447114/find-the-longest-continuous-chunk-of-true-in-a-boolean-vector
  date_unique <- unique_day(date)
  strat_unique <- unique_day_data(date, stratified_period)
  i_strat_start <- head(which(strat_unique), 1)
  as.numeric(format(date_unique[i_strat_start], "%j"))
}

#' @description Duration of stratified period
stratification_duration <- function(date, stratified_period) {
  strat_unique <- unique_day_data(date, stratified_period) # Need one T/F per day for this sum method to work
  sum(strat_unique) # count how many are in the longest stratified chunk
}

#' @description avg. thermocline depth in stratified period
sthermo_depth_mean <- function(date, depth, wtr, stratified_period, ice_on_date, ice_off_date) {
  tibble(date, depth, wtr, stratified_period) %>% 
    filter(stratified_period) %>% 
    # Only use days with open water (no ice)
    filter(date >= ice_off_date & date <= ice_on_date) %>%
    group_by(date) %>%
    summarize(daily_thermocline = rLakeAnalyzer::thermo.depth(wtr[!is.na(wtr)], depth[!is.na(wtr)]), .groups = "keep") %>%
    pull(daily_thermocline) %>%
    mean(na.rm = TRUE)
}

#' @description water temperature 0.1m from lake bottom on day of 
#' stratification (as defined in original stratification measure)
bottom_temp_at_strat <- function(date, wtr_bot, year, stratification_onset_yday) {
  date_strat_onset <- as.Date(stratification_onset_yday-1, origin = sprintf("%s-01-01", year))
  i_strat_onset <- which(date == date_strat_onset)
  unique(wtr_bot[i_strat_onset])
}

#' @description Sum of daily Schmidt Stability values for calendar year.
schmidt_daily_annual_sum <- function(date, depth, wtr, ice_on_date, ice_off_date, hypso) {
  tibble(date, depth, wtr) %>% 
    # Only use days with open water (no ice) and handle situations
    # where one of the `ice_off` or `ice_on` dates is NA
    {if(!is.na(ice_off_date)) filter(., date >= ice_off_date) else . } %>%
    {if(!is.na(ice_on_date)) filter(., date <= ice_on_date) else . } %>%
    group_by(date) %>%
    summarize(daily_ss = rLakeAnalyzer::schmidt.stability(wtr, depth, hypso$areas, hypso$depths), .groups = "keep") %>%
    pull(daily_ss) %>%
    sum(na.rm = TRUE)
}

#' @description mean, max, or min of a specific depth's temperature for a month or group of months
calc_monthly_summary_stat <- function(date, wtr_at_depth, depth_prefix, stat_type, month_nums_to_grp = NULL) {
  
  calc_summary_stat <- switch(
    stat_type,
    "mean" = mean,
    "min" = min,
    "max" = max,
    stop(sprintf("A `stat_type` of '%s' is not currently supported", stat_type))
  )
  
  data.frame(date, wtr_at_depth = wtr_at_depth) %>% 
    
    # Make sure we have a unique set
    unique() %>% 
    
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
    summarize(stat_at_depth = calc_summary_stat(wtr_at_depth, na.rm = TRUE), .groups = "keep") %>% 
    pivot_wider(names_from = month, values_from = stat_at_depth, names_prefix = sprintf("%s_%s_", stat_type, depth_prefix)) %>% 
    ungroup()
}

#' days in which there is any part of water column in a temperature range
calc_days_height_vol_within_range <- function(date, depth, wtr, hypso, temp_low, temp_high) {
  stopifnot(length(temp_low) == length(temp_high))
  
  grpd_data <- tibble(date, depth, wtr) %>% 
    group_by(date)
  
  purrr::map(seq_along(temp_low), function(i, grpd_data, temp_low, temp_high, hypso) {
    grpd_data %>% 
      summarize(Z1_Z2 = find_Z1_Z2(wtr, depth, temp_high[i], temp_low[i]), .groups = "keep") %>% 
      ungroup() %>% 
      unpack(cols = Z1_Z2) %>% 
      mutate(daily_height_in_range = Z2 - Z1,
             daily_volume_in_range = calc_volume(Z1, Z2, hypso),
             day_has_wtr_in_range = !is.na(daily_height_in_range)) %>% 
      summarize(!!sprintf("height_%s_%s", temp_low[i], temp_high[i]) := sum(daily_height_in_range, na.rm = TRUE),
                !!sprintf("vol_%s_%s", temp_low[i], temp_high[i]) := sum(daily_volume_in_range, na.rm = TRUE),
                !!sprintf("days_%s_%s", temp_low[i], temp_high[i]) := sum(day_has_wtr_in_range))
  }, grpd_data, temp_low, temp_high, hypso) %>% 
    bind_cols()

}

#' duration of surface temperature between two temperatures degrees C (Spring only)
#' Duration of optimal incubation period
spring_days_incub <- function(date, wtr_surf, wtr_range) {
  # TODO: does it need to be longest consecutive days?
  
  # Need one dat & wtr_surf per day
  date_unique <- unique_day(date)
  wtr_surf_unique <- unique_day_data(date, wtr_surf)
  
  is_spring <- as.numeric(format(date_unique, "%m")) <= 5 # Before June 1 = spring
  sum(wtr_surf_unique[is_spring] >= wtr_range[1] & wtr_surf_unique[is_spring] <= wtr_range[2])
}

#' rate of warming post ice off
#' average change in surface temp (degrees C per day) 30 days post ice off
post_ice_warm_rate <- function(date, wtr_surf, ice_off_date) {
  
  if(is.na(ice_off_date)) {
    return(NA) # TODO: is it right to return NA when there is no ice off for this year?
  }
  
  # Need one dat & wtr_surf per day
  date_unique <- unique_day(date)
  wtr_surf_unique <- unique_day_data(date, wtr_surf)
  
  day_post_range <- c(1,30)
  wtr_post_ice <- get_wtr_post_ice_off(date_unique, wtr_surf_unique, ice_off_date, day_post_range)
  
  days_for_lm <- day_post_range[1]:day_post_range[2]
  deg_per_day <- lm(wtr_post_ice ~ days_for_lm)$coefficients[2]
  names(deg_per_day) <- NULL
  
  return(deg_per_day)
}

#' date where average surface temperature reaches a certain temperature in deg C
#' returns the Julian day
calc_first_day_above_temp <- function(date, wtr_surf, temperatures) {
  
  # Need one dat & wtr_surf per day
  date_unique <- unique_day(date)
  wtr_surf_unique <- unique_day_data(date, wtr_surf)
  
  date_above_df <- lapply(temperatures, function(temp) {
    first_day_above_temp <- date_unique[wtr_surf_unique >= temp] %>% head(1) %>% format("%j") %>% as.numeric()
    if(length(first_day_above_temp) == 0) first_day_above_temp <- NA # Return NA if that temp was never exceeded
    return(first_day_above_temp)
  }) %>% data.frame()
  names(date_above_df) <- sprintf("date_over_%s", temperatures)
  
  return(date_above_df)
}

## Helper functions for the above

get_last_years_data <- function(this_year, dat_all) {
  dat_all %>% 
    filter(year == this_year - 1)
}



#' @description Cumulative sum of degrees >base degrees for entire year
calc_gdd <- function(date, wtr, base = 0) {
  wtr_diff <- unique_day_data(date, wtr) - base
  sum(wtr_diff[wtr_diff > 0], na.rm=TRUE)
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

#' @description Determines if a day is stratified by comparing the
#' difference between the surface and bottom temperatures. Uses the
#' `force_warm` flag to either count only warm stratified periods
#' or count both warm and cool stratified periods.
is_stratified <- function(wtr_surface, wtr_bottom, force_warm = FALSE) {
  # If either top or bottom wtr is missing, then we can't determine if it is stratified.
  if(is.na(wtr_surface) | is.na(wtr_bottom)) return(FALSE) 
  t_diff <- wtr_surface - wtr_bottom
  if(!force_warm) t_diff <- abs(t_diff) # Count both cold and warm periods
  t_diff >= 1 # Stratified means that the difference between top and bottom > 1 deg
}

is_in_longest_consective_chunk <- function(bool_vec) {
  # Added check for duplicates to prevent counting more than one period
  # if they are equal in length.
  continuous_sections <- rle(bool_vec)
  
  # Identify any periods that are duplicated, so that we can pick the first one
  continuous_sections$is_duplicated <- NA
  continuous_sections$is_duplicated[continuous_sections$values] <- duplicated(continuous_sections$lengths[continuous_sections$values])
  continuous_sections$is_duplicated[!continuous_sections$values] <- FALSE
  
  with(continuous_sections, rep(!is_duplicated & lengths == max(lengths[values]) & values, lengths))
}

get_ice_onoff <- function(date, ice, peak_temp_dt, on_or_off) {
  
  # TODO: does this need to involve past and future year info like the function
  #   from mda.lakes? https://github.com/USGS-R/mda.lakes/blob/afb436e047d2a9ca30dfdeae13745d2ee5109455/R/get_ice_onoff.R#L17
  
  stopifnot(on_or_off %in% c("on", "off"))
  
  # Need one date & ice value per day (not per depth & day)
  date_unique <- unique_day(date)
  ice_unique <- unique_day_data(date, ice)
  

  if(on_or_off == "on") {
    # Second half of year, which would be when ice forms ("ice on")
    date_second_half <- date_unique[date_unique > peak_temp_dt]
    ice_second_half <- ice_unique[date_unique > peak_temp_dt]
    
    # If there is no ice present during that part of the year, there is no ice off date because it wasn't there
    if(!any(ice_second_half)) return(NA) #TODO: or what should we return?
    
    # Find start of longest ice period during second half of the year
    ice_on <- head(which(is_in_longest_consective_chunk(ice_second_half)), 1)
    return(date_second_half[ice_on])
    
  } else if(on_or_off == "off") {
    # First half of year, which would be when ice melts ("ice off")
    date_first_half <- date_unique[date_unique <= peak_temp_dt]
    ice_first_half <- ice_unique[date_unique <= peak_temp_dt]
    
    # If there is no ice present during that part of the year, there is no ice off date because it wasn't there
    if(!any(ice_first_half)) return(NA) #TODO: or do we return the first day of the year?
    
    # Find end of longest ice period during first half of the year
    ice_off <- tail(which(is_in_longest_consective_chunk(ice_first_half)), 1)
    return(date_first_half[ice_off])
  }
  
}

get_wtr_post_ice_off <- function(date, wtr, ice_off_date, day_post_range) {
  wtr[date >= ice_off_date + day_post_range[1] & date <= ice_off_date + day_post_range[2]]
}

# This is based on the code in calculate_toha, but it was challenging to create
#   a shared function to do this, so kept it separate.
find_Z1_Z2 <- function(wtr, depth, wtr_upper_bound, wtr_lower_bound) {
  
  if(length(unique(wtr[!is.na(wtr)])) > 1) {
    z_surface <- 0
    z_max <- max(depth) #TODO: include hypso bc max might be different than wtr depths
    
    if(length(unique(wtr)) == 1) {
      # In a well-mixed lake, it is possible for all wtr values to be the same
      # which causes approx to throw an error. Return NAs for the Zs and let
      # checks below determine if benth area is all (or partially) above or 
      # below the lake and set Zs based on that.
      Z1_Z2 <- c(NA,NA)
    } else {
      # Had to explicitly add `ties=mean` to suppress warning
      # https://community.rstudio.com/t/conditionally-interpolate-values-for-one-data-frame-based-on-another-lookup-table-per-group-solved/40922/5
      Z1_Z2 <- approx(wtr, depth, xout=c(wtr_upper_bound, wtr_lower_bound), ties=mean)$y
    }
    
    Z1 <- Z1_Z2[1]
    Z2 <- Z1_Z2[2]
    
    wtr_surface <- wtr[which.min(depth)] # wtr_surface will be whatever the top-most wtr is
    wtr_bottom <- wtr[which.max(depth)] # wtr_bottom will be whatever the bottom-most wtr is
    
    # Adjust thermal range depths based on a few different edge-case scenarios
    
    completely_below_lake <- wtr_bottom > wtr_upper_bound # if THA is completely below lake, benthic area is 0 (too hot)
    completely_above_lake <- wtr_surface < wtr_lower_bound # if THA is completely above lake, benthic area is 0 (too cold)
    
    # if THA extends above lake, use the surface as Z1 to calc THA
    extends_above_lake <- wtr_surface < wtr_upper_bound & !completely_above_lake
    Z1[extends_above_lake] <- z_surface
    
    # if THA extends below lake, use the bottom as Z2 to calc THA
    extends_below_lake <- wtr_bottom > wtr_lower_bound & !completely_below_lake
    Z2[extends_below_lake] <- z_max
  } else {
    # If there is only one non-NA wtr, then we can't figure out where Z1 and Z2 would be
    Z1 <- NA
    Z2 <- NA
  }
  return(tibble(Z1 = Z1, Z2 = Z2))
}

unique_day_data <- function(date, vec) {
  # Need one unique val per day
  # Duplicated because a column of wtr_surf and 
  # wtr_bot were added to a dataset with one 
  # row per day per depth.
  i_unique_day <- which(!duplicated(date))
  return(vec[i_unique_day])
}

unique_day <- function(date) {
  # Unique day dates only
  i_unique_day <- which(!duplicated(date))
  return(date[i_unique_day])
}

# Needs `resample_hypso` from source("2_process/src/calculate_toha.R")
calc_volume <- function(Z1, Z2, hypso) {
  A1 <- resample_hypso(hypso, Z1)$areas
  A2 <- resample_hypso(hypso, Z2)$areas
  abs(A2-A1)*(Z2-Z1)/3
}

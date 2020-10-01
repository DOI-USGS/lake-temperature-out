
calculate_annual_metrics <- function(target_name, site_files, ice_files) {
  
  purrr::map(site_files, function(fn) {
    
    data_ready <- read_feather(fn) %>% 
      select(site_id, date = DateTime, starts_with("temp_")) %>% 
      pivot_longer(cols = starts_with("temp_"), names_to = "depth", values_to = "wtr") %>% 
      mutate(depth = as.numeric(gsub("temp_", "", depth))) %>% 
      mutate(year = as.numeric(format(date, "%Y")))
    
    data_stratification <- data_ready %>% 
      group_by(date) %>% 
      summarize(wtr_surf_daily = wtr[depth == 0],
                wtr_bot_daily = find_wtr_at_depth(wtr, depth, calc_lake_bottom(depth)),
                .groups = "keep") %>% 
      mutate(stratified = is_stratified(wtr_surf_daily, wtr_bot_daily)) %>% 
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
      left_join(read_csv(ice_files[grepl(unique(data_ready$site_id), ice_files)]), by = "date") %>% 
      arrange(date, depth) %>% # Data was coming in correct, but just making sure 
      # Add flag to say if the day is stratified or not. Doing this here because it will be used
      # by multiple annual metrics below.
      left_join(data_stratification, by = "date") # these were calculated by day and by adding into a long format data set, they will be duplicated
    
    annual_metrics <- data_ready_with_flags %>%
      group_by(site_id, year) %>% 
      summarize(
        
        winter_dur_0_4 = winter_dur_0_4(date, wtr, depth, prev_yr_data=get_last_years_data(unique(year), data_ready)),
        coef_var_30_60 = coef_var_30_60(wtr, depth, ice),
        # coef_var_0_30 = coef_var_0_30(),
        stratification_onset_yday = stratification_onset_yday(date, in_stratified_period),
        stratification_duration = stratification_duration(date, in_stratified_period),
        sthermo_depth_mean = sthermo_depth_mean(date, depth, wtr, in_stratified_period),
        
        peak_temp = peak_temp(wtr_surf_daily),
        gdd_wtr_0c = calc_gdd(wtr, 0),
        gdd_wtr_5c = calc_gdd(wtr, 5),
        gdd_wtr_10c = calc_gdd(wtr, 10),
        
        bottom_temp_at_strat = bottom_temp_at_strat(date, wtr_bot_daily, unique(year), stratification_onset_yday),
        # schmidt_daily_annual_sum = schmidt_daily_annual_sum(),
        
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
        
        .groups = "keep" # suppresses message about regrouping
      ) %>% 
      unpack(cols = c(mean_surf_mon, max_surf_mon, mean_bot_mon, max_bot_mon,
                      mean_surf_jas, max_surf_jas, mean_bot_jas, max_bot_jas))
    
    message(sprintf("Completed annual metrics for %s/%s", which(site_files == fn), length(site_files)))
    
    return(annual_metrics)
  }) %>% 
    bind_rows() %>% 
    readr::write_csv(target_name)
  
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

#' @description Coefficient of Variation of surface temperature from 30-60 days post ice off.
coef_var_30_60 <- function(wtr, depth, ice) {
  browser()
  is_surface <- which(depth == 0)
  wtr[is_surface]
  ice[is_surface]
}

#' @description Coefficient of Variation of surface temperature from 0-30 days post ice off.
coef_var_0_30 <- function(date, wtr) {
  
}

#' @description Julian date at which stratification sets up (longest stratified period)
stratification_onset_yday <- function(date, stratified_period) {
  # https://stackoverflow.com/questions/37447114/find-the-longest-continuous-chunk-of-true-in-a-boolean-vector
  i_strat_start <- head(which(stratified_period), 1)
  as.numeric(format(date[i_strat_start], "%j"))
}

#' @description Duration of stratified period
stratification_duration <- function(date, stratified_period) {
  i_unique_day <- which(!duplicated(date)) # Need one T/F per day for this sum method to work
  sum(stratified_period[i_unique_day]) # count how many are in the longest stratified chunk
}

#' @description avg. thermocline depth in stratified period
sthermo_depth_mean <- function(date, depth, wtr, stratified_period) {
  tibble(date, depth, wtr, stratified_period) %>% 
    filter(stratified_period) %>% 
    group_by(date) %>% 
    summarize(daily_thermocline = rLakeAnalyzer::thermo.depth(wtr, depth), .groups = "keep") %>% 
    pull(daily_thermocline) %>% 
    mean(na.rm = TRUE)
}

#' @description Maximum observed surface temperature
peak_temp <- function(wtr_surf) {
  max(wtr_surf, na.rm = TRUE)
}

#' @description water temperature 0.1m from lake bottom on day of 
#' stratification (as defined in original stratification measure)
bottom_temp_at_strat <- function(date, wtr_bot, year, stratification_onset_yday) {
  date_strat_onset <- as.Date(stratification_onset_yday-1, origin = sprintf("%s-01-01", year))
  i_strat_onset <- which(date == date_strat_onset)
  unique(wtr_bot[i_strat_onset])
}

#' @description Sum of daily Schmidt Stability values for calendar year.
schmidt_daily_annual_sum <- function(date, wtr) {
  # TODO: need bathymetry data to do this.
  # See https://github.com/USGS-R/mda.lakes/blob/afb436e047d2a9ca30dfdeae13745d2ee5109455/R/necsc_thermal_metrics_core.R#L121-L123
  # rLakeAnalyzer::ts.schmidt.stability
  # rLakeAnalyzer::schmidt.stability
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

## Helper functions for the above

get_last_years_data <- function(this_year, dat_all) {
  dat_all %>% 
    filter(year == this_year - 1)
}



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

is_stratified <- function(wtr_surface, wtr_bottom) {
  # difference between top and bottom > 1 deg
  abs(wtr_surface - wtr_bottom) > 1
}

is_in_longest_consective_chunk <- function(bool_vec) {
  with(rle(bool_vec), rep(lengths == max(lengths[values]) & values, lengths))
}

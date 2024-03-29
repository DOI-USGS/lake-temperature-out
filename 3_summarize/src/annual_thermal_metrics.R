
calculate_annual_metrics_per_lake <- function(out_ind, site_id, site_file, ice_file, temp_ranges_file, morphometry_ind, model_id = NULL, model_id_colname = NULL, verbose = FALSE) {
  
  if(!file.exists(site_file)) stop("File does not exist. If running summaries for GCM output, try changing `caldera_access_date` and build again.")
  
  start_tm <- Sys.time()
  
  if(tools::file_ext(site_file) == "feather") {
    # Feathers created with `arrow` fail when reading in with `feather::read_feather()`
    # But feathers created with `feather` don't fail when reading in with `arrow:read_feather()`
    wtr_data <- arrow::read_feather(site_file) %>% 
      {
        # Add the `site_id` column if it isn't present using the 
        if(!"site_id" %in% names(.))
          mutate(., site_id = site_id)
        else .
      } %>% 
      # Different outputs for modeled lake temp have different colnames for the date
      rename(date = matches("DateTime|time"))
  } else if(tools::file_ext(site_file) == "csv") {
    wtr_data <- read_csv(site_file) %>% 
      mutate(site_id = site_id) 
  } else {
    stop(sprintf("Error with %s: file type not supported", site_file))
  }
  
  if(!is.null(ice_file)) {
    ice_data <- read_csv(ice_file, col_types = cols())
  } else {
    # If there is no `ice_file` provided, we assume that the `ice` column already exists
    # in the data (as it should with the output from GLM projection output from Feb 2022)
    # Extract that info and make a separate ice df
    ice_data <- wtr_data %>% select(date, ice)
  }
  
  data_ready <- wtr_data %>% 
    # If there is an `ice` column in wtr_data, then it will be removed
    select(site_id, date, starts_with("temp_"), -matches("ice")) %>% 
    pivot_longer(cols = starts_with("temp_"), names_to = "depth", values_to = "wtr") %>% 
    mutate(depth = as.numeric(gsub("temp_", "", depth))) %>% 
    mutate(year = as.numeric(format(date, "%Y"))) %>% 
    filter_out_partial_yrs() # Only keep years with at least 365 days
  
  stopifnot(nrow(data_ready) > 0) # There should be data for each site file
  
  # Get hypso for this site
  morphometry <- readRDS(as_data_file(morphometry_ind))
  hypso <- data.frame(H = morphometry$H, A = morphometry$A) %>% 
    mutate(depths = max(H) - H, areas = A) %>% 
    arrange(depths) %>% 
    select(depths, areas)
  
  # Read in temp ranges to use
  temp_ranges <- readRDS(temp_ranges_file)
  
  data_stratification_ice <- data_ready %>% 
    group_by(date) %>% 
    summarize(wtr_surf_daily = wtr[depth == 0],
              wtr_bot_daily = find_wtr_at_depth(wtr, depth, calc_lake_bottom(depth)),
              .groups = "keep") %>% 
    ungroup() %>% 
    # Read and join ice flags for this site (can join here because there is only one per day)
    left_join(ice_data, by = "date") %>% 
    mutate(stratified = is_stratified(wtr_surf_daily, wtr_bot_daily, ice, force_warm = TRUE)) %>% 
    # Add year back in because it is dropped in `group_by` above
    mutate(year = as.numeric(format(date, "%Y"))) %>% 
    group_by(year) %>% 
    # For each year, add a column to say whether a day is within the longest chunk of consecutive stratified days
    mutate(in_stratified_period = is_in_longest_consective_chunk(stratified)) %>% 
    ungroup() %>% 
    select(-year)
  
  data_ready_with_flags <- data_ready %>% 
    arrange(date, depth) %>% # Data was coming in correct, but just making sure 
    # Add flag to say if the day is stratified or not + one for ice. Doing this here because it will be used
    # by multiple annual metrics below and these were calculated as per day metrics, but need
    # to be joined to the per day per depth format of `data_ready`
    left_join(data_stratification_ice, by = "date") # these were calculated by day and by adding into a long format data set, they will be duplicated
  
  # Need these summaries to be used by functions in the next one. Biggest reason is that other functions
  # need access to the following year's ice_on_date and can't use lead/lag unless outside of `summarize`
  pre_summary_data <- data_ready_with_flags %>% 
    # `site_id` included here just so it exists in the final output; there should only be one site_id
    # in the file passed to this function
    group_by(site_id, year) %>% 
    summarize(
      # Maximum observed surface temperature & corresponding date
      peak_temp = max(wtr_surf_daily, na.rm = TRUE),
      peak_temp_dt = date[which.max(wtr_surf_daily)],
      
      # Find ice on and ice off at the same time since both need to use the longest period of ice calc.
      # Also, pass in last years data so that ice on captures potential December (or earlier dates from that year's winter).
      ice_onoff_date = get_ice_onoff(date, ice, peak_temp_dt, prev_yr_ice = get_last_years_data(unique(year), data_ready_with_flags)),
      
      .groups = "keep" # suppresses message about regrouping
    ) %>% 
    unpack(ice_onoff_date) %>% 
    ungroup() %>% 
    # Now use ice_on_date and ice_off_date to add a column for the next winter's ice_on_date to use for open water calcs
    mutate(ice_on_date_next = lead(ice_on_date)) %>% 
    # Open water duration will be NA if either ice_on_date or ice_off_date is NA
    # Needs to be calculated after ungrouping so we can use `lead`
    mutate(open_water_duration = as.numeric(ice_on_date_next - ice_off_date))
  
  # Separately summarize height, volume, days for desired temperature ranges in order
  # to speed things up using `data.table` methods (this is the slowest part of the function)
  # Hansen metrics
  # See https://github.com/USGS-R/necsc-lake-modeling/blob/d37377ea422b9be324e8bd203fc6eecc36966401/data/habitat_metrics_table_GH.csv
  annual_height_vol_days <- data_ready %>%  
    calc_days_height_vol_within_range(
      hypso, 
      temp_low = temp_ranges$Temp_Low, 
      temp_high = temp_ranges$Temp_High)
  
  annual_metrics <- data_ready_with_flags %>%
    # Join in pre-summarized data by year
    left_join(pre_summary_data, by = c("site_id", "year")) %>% 
    group_by(site_id, year) %>% 
    summarize(
      
      # Filter pre-summarized data to be just that one value per year since joining with
      # the daily data would have created multiples
      peak_temp = unique(peak_temp), peak_temp_dt = unique(peak_temp_dt), 
      ice_on_date = unique(ice_on_date), ice_off_date = unique(ice_off_date),
      ice_on_date_next = unique(ice_on_date_next), open_water_duration = unique(open_water_duration),
      
      winter_dur_0_4 = winter_dur_0_4(date, wtr, depth, prev_yr_data=get_last_years_data(unique(year), data_ready)),
      coef_var_31_60 = coef_var(date, wtr_surf_daily, ice_off_date = ice_off_date, c(31,60)),
      coef_var_1_30 = coef_var(date, wtr_surf_daily, ice_off_date = ice_off_date, c(1,30)),

      # Metrics that deal with the stratified period
      stratification_onset_yday = stratification_onset_yday(date, in_stratified_period),
      stratification_duration = stratification_duration(date, in_stratified_period),
      sthermo_depth_mean = sthermo_depth_mean(date, depth, wtr, in_stratified_period, stratification_duration),
      bottom_temp_at_strat = bottom_temp_at_strat(date, wtr_bot_daily, unique(year), stratification_onset_yday, stratification_duration),

      gdd_wtr_0c = calc_gdd(date, wtr_surf_daily, 0),
      gdd_wtr_5c = calc_gdd(date, wtr_surf_daily, 5),
      gdd_wtr_10c = calc_gdd(date, wtr_surf_daily, 10),
      schmidt_daily_annual_sum = schmidt_daily_annual_sum(date, depth, wtr, ice_on_date_next, ice_off_date, hypso),

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

      spring_days_in_10.5_15.5 = spring_days_incub(date, wtr_surf_daily, c(10.5, 15.5)),
      post_ice_warm_rate = post_ice_warm_rate(date, wtr_surf_daily, ice_off_date),
      date_over_temps = calc_first_day_above_temp(date, wtr_surf_daily, temperatures = c(8.9, 16.7, 18, 21)), # Returns a df and needs to be unpacked below
      date_below_temps = calc_first_day_below_temp(date, wtr_surf_daily, temperatures = c(5), peak_temp_dt),
      metalimnion_derivatives = calc_metalimnion_derivatives(date, depth, wtr, in_stratified_period, stratification_duration, hypso),

      .groups = "keep" # suppresses message about regrouping
    ) %>% 
    unpack(cols = c(mean_surf_mon, max_surf_mon, mean_bot_mon, max_bot_mon,
                    mean_surf_jas, max_surf_jas, mean_bot_jas, max_bot_jas,
                    date_over_temps, date_below_temps,
                    metalimnion_derivatives)) %>%
    
    ungroup() %>% 
    # Remove next year's ice_on_date since it can be found by using `lead()`
    select(-ice_on_date_next) %>%
    # Move open_water_duration to end to match previous output
    relocate(open_water_duration, .after = last_col()) %>% 
    left_join(annual_height_vol_days, by = "year") %>% {
      # Only add this column when it is needed
      if(!is.null(model_id_colname) & !is.null(model_id)) 
        mutate(., model_id) %>% 
        select(site_id, !!model_id_colname := model_id, everything())
      else .
    }
  
  if(verbose) {
    message(sprintf("Completed annual metrics for %s in %s min", site_id, 
                    round(as.numeric(Sys.time() - start_tm, units = "mins"), 2)))
  }
  
  data_file <- as_data_file(out_ind)
  saveRDS(annual_metrics, data_file)
  sc_indicate(ind_file = out_ind, data_file = data_file)

}

get_filenames_from_ind <- function(ind_file) {
  names(yaml::read_yaml(ind_file))
}

# Assumes that df has a column called `depth` and one called `year`
# and that each row = 1 day / 1 depth combo.
filter_out_partial_yrs <- function(df) {
  n_depths <- df %>% pull(depth) %>% unique() %>% length()
  df %>% 
    add_count(year) %>% 
    mutate(ndays = n/n_depths) %>% 
    filter(ndays >= 365) %>% 
    select(-n, -ndays)
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
    # Return NA if there is not enough data from the previous year
    # This is usually an issue for the first year in a dataset 
    return(NA)
  } else {
    
    start_date <- unique(sprintf("%s-10-30", format(prev_yr_data$date, "%Y")))
    
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

#' @description avg. thermocline depth in stratified period that is >= 30 days
#' `strat_duration` has one value per year, so there should only be one unique value going in
sthermo_depth_mean <- function(date, depth, wtr, stratified_period, strat_duration, strat_dur_min = 30) {
  
  if(unique(strat_duration) >= strat_dur_min) {
    # Only return results if the stratification duration is at least 30 days
    tibble(date, depth, wtr, stratified_period) %>%
      group_by(date) %>%
      summarize(daily_thermocline = rLakeAnalyzer::thermo.depth(wtr[!is.na(wtr)], depth[!is.na(wtr)]), .groups = "keep", 
                stratified = unique(stratified_period)) %>% 
      ungroup() %>% 
      filter(stratified) %>% 
      pull(daily_thermocline) %>%
      mean(na.rm = TRUE)
  } else {
    return(NA) # If the stratified period is < 30 days, return NA
  }
  
}

#' @description water temperature 0.1m from lake bottom on day of 
#' stratification (as defined in original stratification measure)
bottom_temp_at_strat <- function(date, wtr_bot, year, stratification_onset_yday, strat_duration, strat_dur_min = 30) {
  if(unique(strat_duration) >= strat_dur_min) {
    date_strat_onset <- as.Date(stratification_onset_yday-1, origin = sprintf("%s-01-01", year))
    i_strat_onset <- which(date == date_strat_onset)
    unique(wtr_bot[i_strat_onset])
  } else {
    return(NA) # If the stratified period is < 30 days, return NA
  }
}

#' @description Sum of daily Schmidt Stability values for calendar year.
#' ice_on_date here refers to the upcoming winter ice_on (which could be
#' in Jan of the next year).
schmidt_daily_annual_sum <- function(date, depth, wtr, ice_on_date, ice_off_date, hypso) {
  tibble(date, depth, wtr) %>% 
    # Only use days with open water (no ice) and handle situations
    # where one of the `ice_off` or `ice_on` dates is NA
    {if(!is.na(ice_off_date)) filter(., date >= ice_off_date) else . } %>%
    {if(!is.na(ice_on_date)) filter(., date <= ice_on_date) else . } %>%
    group_by(date) %>%
    summarize(daily_ss = rLakeAnalyzer::schmidt.stability(wtr[!is.na(wtr)], depth[!is.na(wtr)], hypso$areas, hypso$depths), .groups = "keep") %>%
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
calc_days_height_vol_within_range <- function(daily_data, hypso, temp_low, temp_high) {
  stopifnot(length(temp_low) == length(temp_high))
  
  # Create a vector of names in the order we want them (height_[temp range 1], vol_[temp range 1], days_[temp range 1], etc)
  temp_range_str <- sprintf("%s_%s", temp_low, temp_high)
  out_colnames <- apply(expand.grid(c("height", "vol", "days"), temp_range_str), 1, paste, collapse = "_")
  
  setDT(daily_data)
  
  dt <- daily_data[, find_Z1_Z2(wtr, depth, temp_high, temp_low), by = list(year,date)][, `:=` (
    height = as.numeric(Z2 - Z1),
    vol = as.numeric(calc_volume(Z1, Z2, hypso))
  )][, days := as.numeric(height > 0)
     ][, c("Z1", "Z2") := NULL
       ][, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("height", "vol", "days"), by = list(year, temp_low, temp_high)]
  
  dt_wide <- dcast(dt, year ~ temp_low + temp_high, value.var = c("height", "vol", "days"))
  setcolorder(dt_wide, c("year", out_colnames))
  
  setDF(dt_wide)
  out <- as_tibble(dt_wide)
  
  return(out)
  
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

#' date where surface temperature drops below a certain temperature in deg C
#' after the summer peak; returns the Julian day
calc_first_day_below_temp <- function(date, wtr_surf, temperatures, peak_temp_dt) {
  
  # Need one date & wtr_surf per day
  date_unique <- unique_day(date)
  wtr_surf_unique <- unique_day_data(date, wtr_surf)
  
  # Only keep dates after peak summer date because we want to know the fall date specifically
  post_summer_peak_i <- which(date_unique > peak_temp_dt)
  dates_post_summer <- date_unique[post_summer_peak_i]
  water_surf_post_summer <- wtr_surf_unique[post_summer_peak_i]
  
  date_below_df <- lapply(temperatures, function(temp) {
    first_day_below_temp <- dates_post_summer[water_surf_post_summer < temp] %>% head(1) %>% format("%j") %>% as.numeric()
    if(length(first_day_below_temp) == 0) first_day_below_temp <- NA # Return NA if the surface temp never dropped below the temp during the year
    return(first_day_below_temp)
  }) %>% data.frame()
  names(date_below_df) <- sprintf("date_below_%s", temperatures)
  
  return(date_below_df)
}

#' @description Calculates the top and bottom depths of the metalimnion in a stratified lake in order
#' to determine the annual mean metalimnion top and bottom depths, the annual mean volume of the epilimnion,
#' and the annual mean volume of the hypolimnion. Uses the rLakeAnalyzer function, meta.depths
calc_metalimnion_derivatives <- function(date, depth, wtr, stratified_period, strat_duration, hypso) {
  
  if(unique(strat_duration) >= 30) {
    # Only return results if the stratification duration is at least 30 days
    tibble(date, depth, wtr, stratified_period) %>%
      group_by(date) %>%
      summarize(daily_metalimnion = paste(rLakeAnalyzer::meta.depths(wtr[!is.na(wtr)], depth[!is.na(wtr)]), collapse="_"), 
                stratified = unique(stratified_period), .groups = "keep") %>% 
      ungroup() %>% 
      separate(daily_metalimnion, into = c("meta_top", "meta_bottom"), sep="_") %>% 
      mutate(meta_top = as.numeric(meta_top), meta_bottom = as.numeric(meta_bottom)) %>% 
      mutate(epi_vol = calc_volume(0, meta_top, hypso)) %>% # Volume of layer above the metalimnion to the surface
      mutate(hyp_vol = calc_volume(meta_bottom, max(hypso$depths), hypso)) %>% # Volume of layer below the metalimnion to the bottom
      filter(stratified) %>% 
      summarize(SmetaTopD_mean = mean(meta_top, na.rm = TRUE),
                SmetaBotD_mean = mean(meta_bottom, na.rm = TRUE),
                mean_epi_vol = mean(epi_vol, na.rm = TRUE),
                mean_hyp_vol = mean(hyp_vol, na.rm = TRUE)) %>% 
      mutate(mean_epi_hypo_ratio = mean_epi_vol / mean_hyp_vol)
  } else {
    # If the stratified period is < 30 days, return NA
    return(tibble(SmetaTopD_mean = NA, SmetaBotD_mean = NA, mean_epi_vol = NA, 
                  mean_hyp_vol = NA, mean_epi_hypo_ratio = NA)) 
  }
  
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
  if(length(na.omit(wtr)) > 1) {
    # Can only linearly interpolate if there are at least 2 values to use
    # Otherwise, this throws an error. Will still return NA if the depth value
    # to find is outside of the values available.
    
    wtr_at_depth <- approx(depth, wtr, depth_to_find)$y
    
    # If the values at the bottom are NA, use the bottom-most non-NA wtr value instead
    if(is.na(wtr_at_depth)) {
      wtr_at_depth <- wtr[depth == max(depth[!is.na(wtr)])]
    }
    return(wtr_at_depth)
  } else {
    return(NA)
  }
}

#' @description Determines if a day is stratified by comparing the
#' difference between the surface and bottom temperatures. Uses the
#' `force_warm` flag to either count only warm stratified periods
#' or count both warm and cool stratified periods.
is_stratified <- function(wtr_surface, wtr_bottom, ice, force_warm = FALSE) {
  
  t_diff <- wtr_surface - wtr_bottom
  if(!force_warm) t_diff <- abs(t_diff) # Count both cold and warm periods
  
  # Stratified means that the difference between top and bottom > 1 deg
  # Also, cannot be considered stratified if there is ice
  stratified <- t_diff >= 1 & !ice 
  
  # If either top or bottom wtr is missing, then we can't determine if it is stratified.
  stratified[is.na(wtr_surface) | is.na(wtr_bottom)] <- FALSE
  
  return(stratified)
}

is_in_longest_consective_chunk <- function(bool_vec) {
  # Added check for duplicates to prevent counting more than one period
  # if they are equal in length.
  continuous_sections <- rle(bool_vec)
  
  # Identify any periods that are duplicated, so that we can pick the first one
  continuous_sections$is_duplicated <- NA
  continuous_sections$is_duplicated[continuous_sections$values] <- duplicated(continuous_sections$lengths[continuous_sections$values])
  continuous_sections$is_duplicated[!continuous_sections$values] <- FALSE
  
  # Handle situation where there are no stratified days and therefore none in the longest consecutive period
  if(any(continuous_sections$values)) {
    with(continuous_sections, rep(!is_duplicated & lengths == max(lengths[values]) & values, lengths))
  } else {
    rep(FALSE, length(bool_vec))
  }
}

get_ice_onoff <- function(date, ice, peak_temp_dt, prev_yr_ice) {
  
  # Uses the current year's data plus some from the previous year
  # similar to the function from mda.lakes:
  #   https://github.com/USGS-R/mda.lakes/blob/afb436e047d2a9ca30dfdeae13745d2ee5109455/R/get_ice_onoff.R#L17
  
  # Add in last year's data that counts towards this year's winter which is post-peak 
  #   temp. However, we can't easily pass in peak of previous year (`lag` doesn't work 
  #   that way within `summarize`) so we are using July 1 as a safe date to capture all
  #   dates with ice from the previous year since it likely hasn't started to form yet.
  date_last <- prev_yr_ice$date
  ice_last <- prev_yr_ice$ice
  yr_last <- unique(format(date_last, "%Y"))
  assumed_post_peak_dt_last <- as.Date(sprintf("%s-07-01", yr_last))
  date_last_winter <- date_last[date_last >= assumed_post_peak_dt_last]
  ice_last_winter <- ice_last[date_last >= assumed_post_peak_dt_last]
  
  # Now keep only dates from this year before the peak temperature
  #   which would be when ice melts ("ice off")
  date_first_half <- date[date <= peak_temp_dt]
  ice_first_half <- ice[date <= peak_temp_dt]
  
  # Combine last year's dates with this year to get all the possible
  #   values for ice data in this year's winter
  date_all <- c(date_last_winter, date_first_half)
  ice_all <- c(ice_last_winter, ice_first_half)
  
  # Need one date & ice value per day (not per depth & day)
  date_unique <- unique_day(date_all)
  ice_unique <- unique_day_data(date_all, ice_all)
  
  # If there is no ice present during that part of the year, there is no ice on or ice off date because it wasn't there
  if(!any(ice_unique)) {
    ice_on <- as.Date(NA)
    ice_off <- as.Date(NA)
  } else {
    # Find longest ice period during winter dates for this year
    dates_in_ice_period <- date_unique[which(is_in_longest_consective_chunk(ice_unique))]
    ice_on <- head(dates_in_ice_period, 1)
    ice_off <- tail(dates_in_ice_period, 1)
    
    # ice_off should not be in the year before, so should be NA 
    # if there are no ice off dates in the current year.
    if(as.numeric(format(ice_off, "%Y")) != unique(as.numeric(format(date, "%Y")))) {
      ice_off <- as.Date(NA)
    }
  }
  
  if(nrow(prev_yr_ice) == 0) {
    # Return NA for ice_on if there was not any data from the previous year
    # This is usually an issue for the first year in a dataset 
    ice_on <- as.Date(NA)
  }
  
  return(data.frame(ice_on_date = ice_on, ice_off_date = ice_off))
  
}

get_wtr_post_ice_off <- function(date, wtr, ice_off_date, day_post_range) {
  wtr[date >= ice_off_date + day_post_range[1]-1 & date <= ice_off_date + day_post_range[2]-1]
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
      Z1 <- rep(NA, length(wtr_upper_bound))
      Z2 <- rep(NA, length(wtr_lower_bound))
    } else {
      # Had to explicitly add `ties=mean` to suppress warning
      # https://community.rstudio.com/t/conditionally-interpolate-values-for-one-data-frame-based-on-another-lookup-table-per-group-solved/40922/5
      Z1 <- approx(wtr, depth, xout=wtr_upper_bound, ties=mean)$y
      Z2 <- approx(wtr, depth, xout=wtr_lower_bound, ties=mean)$y
    }
    
    wtr_surface <- wtr[which.min(depth)] # wtr_surface will be whatever the top-most wtr is
    wtr_bottom <- wtr[which.max(depth)] # wtr_bottom will be whatever the bottom-most wtr is
    
    # Adjust thermal range depths based on a few different edge-case scenarios
    
    completely_below_lake <- wtr_bottom > wtr_upper_bound # if THA is completely below lake, benthic area is 0 (too hot)
    completely_above_lake <- wtr_surface < wtr_lower_bound # if THA is completely above lake, benthic area is 0 (too cold)
    
    # If completely above or completely below, the Zs need to reflect that
    Z1[completely_below_lake] <- z_max
    Z2[completely_below_lake] <- z_max
    Z1[completely_above_lake] <- z_surface
    Z2[completely_above_lake] <- z_surface
    
    # if THA extends above lake, use the surface as Z1 to calc THA
    extends_above_lake <- wtr_surface < wtr_upper_bound & !completely_above_lake
    Z1[extends_above_lake] <- z_surface
    
    # if THA extends below lake, use the bottom as Z2 to calc THA
    extends_below_lake <- wtr_bottom > wtr_lower_bound & !completely_below_lake
    Z2[extends_below_lake] <- z_max
  } else {
    # If there is only one non-NA wtr, then we can't figure out where Z1 and Z2 would be
    Z1 <- rep(NA, length(wtr_upper_bound))
    Z2 <- rep(NA, length(wtr_lower_bound))
  }
  return(data.table(temp_low = wtr_lower_bound, 
                    temp_high = wtr_upper_bound, 
                    Z1 = as.numeric(Z1), 
                    Z2 = as.numeric(Z2)))
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

# Collection of functions to calculate annual thermal metrics
# All accept the same main data input, `date` and `wtr`.

#' @param date a vector (or one column) with the dates of class `Date`
#' @param wtr a vector (or one column) with water temperature values in deg C


#' @description # of days, beginning October 30 of the previous year and running 
#' through June of the given year, that surface water is <4 degrees C
winter_dur_0_4 <- function(date, wtr) {
  
}

#' @description Coefficient of Variation of surface temperature from 30-60 days post ice off.
coef_var_30_60 <- function(date, wtr) {
  
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
peak_temp <- function(date, wtr) {
  
}

#' @description Cumulative sum of degrees > 0 for entire year
gdd_wtr_0c <- function(date, wtr) {
  
}

#' @description Cumulative sum of degrees > 5 for entire year
gdd_wtr_5c <- function(date, wtr) {
  
 }

#' @description Cumulative sum of degrees > 10 for entire year
gdd_wtr_10c <- function(date, wtr) {
  
}

#' @description water temperature 0.1m from lake bottom on day of 
#' stratification (as defined in original stratification measure)
bottom_temp_at_strat <- function(date, wtr) {
  
}

#' @description Sum of daily Schmidt Stability values for calendar year.
schmidt_daily_annual_sum <- function(date, wtr) {
  
}

#' @description mean of surface temperature from July, August, September
mean_surf_jas <- function(date, wtr) {
  
}

#' @description max of surface temperature from July, August, September
max_surf_jas <- function(date, wtr) {
  
}

#' @description mean of bottom temperature (0.1 m from bottom) from July, August, September
mean_bot_jas <- function(date, wtr) {
  
 }

#' @description max of bottom temperature (0.1 m from bottom) from July, August, September
max_bot_jas <- function(date, wtr) {
  
}

## Need to do the following fxns for each month.

#' @description mean of surface temperature for a month
mean_surf_mon <- function(date, wtr) {
  
}

#' @description max of surface temperature for each month
max_surf_mon <- function(date, wtr) {
  
}

#' @description mean of bottom temperature for each month
mean_bot_mon <- function(date, wtr) {
  
}

#' @description max of bottom temperature for each month
max_bot_mon <- function(date, wtr) {
  
}

# Functions to calculate daily TOHA for each lake
# Edited `mda.lakes` functions.

calculate_toha_per_lake <- function(target_name, site_data_fn, morphometry) {
  
  site_data <- feather::read_feather(site_data_fn)
  wtr_cols <- grep("temp_", names(site_data))
  
  hypsos <- data.frame(H = morphometry$H, A = morphometry$A) %>% 
    mutate(depths = max(H) - H, areas = A) %>% 
    arrange(depths) %>% 
    select(depths, areas)
  
  toha_out <- purrr::map(1:nrow(site_data), function(r) {
      opti_thermal_habitat_subdaily(
        current_date = site_data$DateTime[r],
        wtr = site_data[r, wtr_cols], 
        io = site_data$io[r], 
        kd = site_data$kd[r], 
        lat = morphometry$latitude, 
        lon = morphometry$longitude, 
        hypsos = hypsos, 
        irr_thresh = c(0.0762, 0.6476), 
        wtr_thresh = c(11,25))
    }) %>% 
    purrr::reduce(bind_rows) %>% 
    mutate(date = site_data$DateTime) %>% # Add the date column
    select(date, everything()) %>% # Move it so it's first
    readr::write_csv(path = target_name)
}

#' @title Calculate OHA, THA, and TOHA within table.
#' 
#' @description This function assumes that you are doing one day at a time and
#'   interpolating the day into minutes.
#'   
#' @param current_date a single POSIXct value indicating the date
#' @param wtr a data.frame with one row for the day and a column for each depth, 
#' filled with daily water temperature values
#' @param io the daily irradiance value
#' @param kd the daily clarity value
#' @param lat the latitude value (one value)
#' @param lon the longitude value (one value)
#' @param hypsos 
#' @param irr_thresh
#' @param wtr_thresh
#' 
#' @return data.frame (1 row, 3 columns); columns are `opti_hab`, `therm_hab`, and `opti_therm_hab` 
#' for optical habitat, thermal habitat, and optical thermal habitat, respectively.
#' 
opti_thermal_habitat_subdaily <- function(current_date, wtr, io, kd, lat, lon, hypsos, irr_thresh, wtr_thresh) {
  
  # Remove missing depth profiles from wtr
  na_depth_profiles <- sapply(wtr, function(x) { all(is.na(x)) })
  wtr_rmNA <- wtr[, !na_depth_profiles] # remove any temp profiles that are NA
  
  io <- mda.lakes::create_irr_day_cycle(lat,lon, dates=current_date, irr_mean = io, by='min')
  
  habitat_shared <- area_light_temp_threshold_shared(wtr_rmNA, kd, io[[2]], irr_thresh, wtr_thresh, hypsos)
  
  return(habitat_shared)
}

# A function that combines the separate ones in order to share code for 
#   calculating each vol map and then the areas.
area_light_temp_threshold_shared <- function(wtr, kd, light_incident, irr_thresh=c(0,2000), wtr_thresh=c(0,25), hypso) {
  
  # Upsample both hypsography & wtr so that they share depths
  updated_hypso <- interp_hypso_to_match_temp_profiles(wtr, hypso)
  updated_wtr <- interp_temp_profiles_to_match_hypso(wtr, hypso)
  
  depth_area_rel <- benthic_areas(updated_hypso$depths, updated_hypso$areas) # Calculate the depth to benthic area relationship
  
  light_map <- vol_light_map(kd, light_incident, irr_thresh, updated_hypso$depths)
  wtr_map <- vol_temp_map(updated_wtr, wtr_thresh) # wtr is just daily here; upsampled below to be able to compare to light
  
  # Upsample wtr (repeat values) to match dimensions of light_map, then compare light & temp
  wtr_upsampled_map <- matrix(wtr_map, ncol = ncol(wtr_map), nrow = length(light_incident), byrow=TRUE)
  both_map <- light_map & wtr_upsampled_map  #only where both apply
  
  light_only_average_area <- calc_area_from_vol(light_map, depth_area_rel)
  temp_only_average_area <- calc_area_from_vol(wtr_map, depth_area_rel)
  light_temp_average_area <- calc_area_from_vol(both_map, depth_area_rel)
  
  return(data.frame(opti_hab = light_only_average_area, 
                    therm_hab = temp_only_average_area, 
                    opti_therm_hab = light_temp_average_area))
}

interp_hypso_to_match_temp_profiles <- function(wtr, hypso) {
  
  # Match hypso depths to water temperature profile depths
  matched_depths <- c(hypso$depths, rLakeAnalyzer::get.offsets(wtr)) %>% unique() %>% sort
  
  # Linear relationship between depth and radius
  hypso$radii <- sqrt(hypso$areas / pi)
  matched_radii <- approx(hypso$depths, hypso$radii, xout=matched_depths)$y
  
  # Now calculate area from new radii
  matched_areas <- pi * matched_radii^2
  
  matched_hypso <- list(depths = matched_depths, areas = matched_areas)
  
  return(matched_hypso)
}

# Assumes that `wtr` colnames start with `temp_`
interp_temp_profiles_to_match_hypso <- function(wtr, hypso) {
  
  wtr_depths <- rLakeAnalyzer::get.offsets(wtr)
  matched_depths <- c(hypso$depths, wtr_depths) %>% unique() %>% sort
  
  # Figure out which will need to have interpolated wtr values
  new_depths_i <- which(!matched_depths %in% wtr_depths)
  
  # Add new empty columns for new wtr depths & sort the data frame
  wtr_matched <- wtr
  wtr_matched[, paste0("temp_", matched_depths[new_depths_i])] <- NA
  wtr_matched <- wtr_matched[, order(rLakeAnalyzer::get.offsets(wtr_matched))]
  
  # Interpolate wtr for each new column & then fill into the real data.frame
  wtr_matched[, new_depths_i] <- purrr::map_dfc(new_depths_i, function(i) {
    if(i == 1) {
      # If the current new depth is the first depth in the profile, match the next wtr
      wtr_new <- wtr_matched[[i+1]]
    } else {
      depth_next <- matched_depths[i+1]
      wtr_prev <- wtr_matched[[i-1]]
      if(is.na(depth_next)) {
        # If the current new depth is the last depth in the profile, match the previous wtr
        wtr_new <- wtr_prev
      } else {
        depth_prev <- matched_depths[i-1]
        depth_ratio <- (matched_depths[i] - depth_prev) / (depth_next - depth_prev)
        wtr_new <- wtr_prev + depth_ratio*(wtr_matched[[i+1]] - wtr_prev)
      }
    }
    return(wtr_new)
  }) 
  
  return(wtr_matched)
}

# Produces a vector of length n-1
#  of benthic areas between each depth
benthic_areas <- function(depths, areas){
  
  # Verify that depths & areas are listed from top to bottom
  # Our "depths" are actually "heights" above sea level. So the 
  # smaller the height, the deeper that profile.
  corrected_order <- order(depths)
  depths <- depths[corrected_order]
  areas <- areas[corrected_order]
  
  # Verify that our assumptions are correct
  stopifnot(tail(depths, 1) == max(depths)) # bottom of lake is last in order
  stopifnot(tail(areas, 1) == min(areas)) # bottom of lake is smallest area
  
  areas_lead <- c(areas[-1], NA)
  depths_lead <- c(depths[-1], NA)
  
  trap_length <- sqrt(areas*pi) + sqrt(areas_lead*pi)
  trap_height <- sqrt( (depths_lead - depths)^2 + (sqrt(areas/pi) - sqrt(areas_lead/pi))^2 )
  benth_areas <- (trap_length * trap_height)
  benth_areas_rmna <- benth_areas[!is.na(benth_areas)]
  
  # add in area for the bottom of the lake (if comes to point, then adds 0 and doesn't change)
  last_cone <- length(benth_areas_rmna)
  benth_areas_rmna[last_cone] <- benth_areas_rmna[last_cone] + tail(areas, 1) 
  
  return(benth_areas_rmna)
}

vol_light_map <- function(kd, light_incident, thresholds, depths){
  
  irr_mat <- matrix(light_incident, nrow = length(light_incident), ncol = 1)
  depths_mat <- matrix(depths, nrow = 1, ncol = length(depths))
  
  light_profile <- irr_mat %*% exp(-1* kd * depths_mat) # multiple matrices together
  
  light_map = light_profile >= thresholds[1] & light_profile <= thresholds[2]
  
  #Now we need to turn it to a volumetric light map, where TRUE means 
  # that layer (not just slice) is within the thresholds
  light_map_shifted <- light_map[,-1] # Effectively "moves" values of light_map over 1 column
  light_map_to_compare <- light_map[,-ncol(light_map), drop=FALSE] # Removes last column
  vol_light_map <- light_map_shifted & light_map_to_compare # Compares each value to the value of the next column over
  
  return(vol_light_map)
}

# wtr is only the columns with temp profiles, no datetimes
vol_temp_map <- function(wtr, thresholds) {
  
  # Using `drop=FALSE` to maintain matrix when there is only 1 row (otherwise becomes vector)
  wtr_map <- wtr >= thresholds[1] & wtr <= thresholds[2]
  wtr_map_shifted <- wtr_map[,-1, drop=FALSE] # Effectively "moves" values of wtr_map over 1 column
  wtr_map_to_compare <- wtr_map[,-ncol(wtr_map), drop=FALSE] # Removes last column
  vol_wtr_map <- wtr_map_shifted & wtr_map_to_compare # Compares each value to the value of the next column over
  
  return(vol_wtr_map)
}

# Using the T/F map for where either light, temp, or both
#   are available, calculate the area that it equates to
calc_area_from_vol <- function(vol_map, depth_area_rel) {
  map_collapsed <- colSums(vol_map, na.rm=TRUE) # Sum number of timesteps that the condition was TRUE
  average_area <- sum(depth_area_rel * map_collapsed, na.rm=TRUE)/nrow(vol_map) # Divide by number of timesteps to get average area per day
  return(average_area)
}

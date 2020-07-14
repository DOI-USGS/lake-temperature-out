# Functions to calculate daily TOHA for each lake
# Edited `mda.lakes` functions.

calculate_toha_per_lake <- function(target_name, site_data_fn, morphometry) {
  
  site_data <- feather::read_feather(site_data_fn)
  wtr_cols <- grep("temp_", names(site_data))
  
  # When adding obs data, some sites don't make it past the filtering criteria
  # so we need to skip them or toha calculations error weirdly
  if(nrow(site_data) == 0) {
    # Can't use `site_data` since there aren't any observations
    site_id <- sub(pattern = ".*(nhdhr_.*)\\..*$", replacement = "\\1", target_name)
    warning(sprintf("Insufficient data to calculate TOHA for %s", site_id))
    data.frame() %>% readr::write_csv(path = target_name)
  } else {
    
    hypsos <- data.frame(H = morphometry$H, A = morphometry$A) %>% 
      mutate(depths = max(H) - H, areas = A) %>% 
      arrange(depths) %>% 
      select(depths, areas)
    
    # precalculate benthic areas for each known slice, rather than doing
    # this for each time timestep
    cum_benthic_area <- cumsum(calc_benthic_area(
      hypsos$depths[-length(hypsos$depths)],
      hypsos$depths[-1],
      hypsos$areas[-length(hypsos$areas)],
      hypsos$areas[-1]))
    
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
        wtr_thresh = c(11,25),
        cum_ba = cum_benthic_area)
    }) %>% 
      bind_rows() %>% 
      mutate(date = site_data$DateTime, .before = 1) %>% # Add the date column first (need dplyr > 1.0.0)
      mutate(site_id = site_data$site_id, .before = 1) %>% # Add the site column first (need dplyr > 1.0.0)
      readr::write_csv(path = target_name)
  }
  
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
#' @param cum_ba cumulative sum of benthic areas from known hypsography
#' 
#' @return data.frame (1 row, 3 columns); columns are `opti_hab`, `therm_hab`, and `opti_therm_hab` 
#' for optical habitat, thermal habitat, and optical thermal habitat, respectively.
#' 
opti_thermal_habitat_subdaily <- function(current_date, wtr, io, kd, lat, lon, hypsos, irr_thresh, wtr_thresh, cum_ba) {
  
  na_depth_profiles <- sapply(wtr, function(x) { all(is.na(x)) })
  wtr_rmNA <- wtr[, !na_depth_profiles] # remove any temp profiles that are NA
  
  io <- mda.lakes::create_irr_day_cycle(lat,lon, dates=current_date, irr_mean = io, by='min')
  
  oha_df <- optical_habitat_area(io[[2]], kd, hypsos, irr_thresh[1], irr_thresh[2], cum_ba)
  tha_df <- thermal_habitat_area(wtr_rmNA, hypsos, wtr_thresh[1], wtr_thresh[2], cum_ba)
  
  # Upsample tha (repeat values) to match the number of oha values (subdaily)
  # Will even work if `nrow(wtr_rmNA) > 1`
  tha_upsampled <- tha_df %>% slice(rep(1:n(), each = nrow(oha_df)))
  toha <- thermal_optical_habitat_area(tha_upsampled, oha_df, hypsos, cum_ba)
  
  # Divide by number of timesteps to get average area per day
  oha_daily <- sum(oha_df$habitat)/nrow(oha_df) 
  tha_daily <- tha_df$habitat/nrow(tha_df) # should be the same before/after division since tha_df is already daily
  toha_daily <- sum(toha)/length(toha)
  
  # Output depths for where habitat exists (get average depth per day)
  oha_Z1_daily <- sum(oha_df$Z1, na.rm = TRUE)/nrow(oha_df) 
  oha_Z2_daily <- sum(oha_df$Z2, na.rm = TRUE)/nrow(oha_df) 
  tha_Z1_daily <- sum(tha_df$Z1, na.rm = TRUE)/nrow(tha_df) 
  tha_Z2_daily <- sum(tha_df$Z2, na.rm = TRUE)/nrow(tha_df) 
  
  return(data.frame(opti_hab = oha_daily, 
                    therm_hab = tha_daily, 
                    opti_therm_hab = toha_daily,
                    opti_Z1 = oha_Z1_daily,
                    opti_Z2 = oha_Z2_daily,
                    therm_Z1 = tha_Z1_daily,
                    therm_Z2 = tha_Z2_daily))
  
}

# Vectorized function
optical_habitat_area <- function(I_0, Kd, hypso, I_lower, I_upper, cum_ba) {
  
  stopifnot(all(hypso$depths == cummax(hypso$depths))) # Stop if hypso is not in order
  # Expecting only one Kd per day but if not, needs to match length of I_0
  stopifnot(length(Kd) == 1 || length(Kd) == length(I_0))
  
  z_max <- tail(hypso$depths, 1)
  z_surface <- 0 # always 0, might need to resample hypso
  
  ##### Find exact depths of irr thresholds 
  
  Z1 <- log( I_upper / I_0) / -Kd
  Z2 <- log( I_lower / I_0) / -Kd
  
  ##### Checks before calculating areas
  completely_below_lake <- Z1 > z_max # if OHA is completely below lake, benthic area is 0 (too bright)
  completely_above_lake <- I_0 < I_lower # if OHA is completely above lake, benthic area is 0 (too dark)
  benth_0 <- completely_below_lake | completely_above_lake
  
  # If I_0 == 0, then Z1 and Z1 become -Inf. They should be
  #   be 0 before we return to a user. 
  Z1[I_0 == 0] <- 0
  Z2[I_0 == 0] <- 0
  
  # if OHA extends above lake, use the surface as Z1 to calc OHA
  extends_above_lake <- Z1 < z_surface & !completely_above_lake
  Z1[extends_above_lake] <- z_surface
  
  # if OHA extends below lake, use the bottom as Z2 to calc OHA
  extends_below_lake <- Z2 >= z_max & !completely_below_lake # also include those with Z2 == z_max
  Z2[extends_below_lake] <- z_max
  
  ##### Create hypso at these different depths
  A1 <- resample_hypso(hypso, Z1)[["areas"]]
  A2 <- resample_hypso(hypso, Z2)[["areas"]]
  
  ##### Use areas and depths at thresholds to calculate benthic area
  
  benthic_area <- calc_benthic_area_incl_hypso(Z1, Z2, A1, A2, hypso, cum_ba)
  
  ##### Final benthic area checks
  
  # Add lake bottom if bottom of optical habitat 
  benthic_area[extends_below_lake] <- benthic_area[extends_below_lake] + tail(hypso$areas, 1)
  
  # Set benthic areas to zero based on above criteria
  benthic_area[benth_0] <- 0 
  
  return(data.frame(Z1 = Z1, Z2 = Z2, habitat = benthic_area))
}

# Now vectorized (except for one apply fxn)
thermal_habitat_area <- function(wtr_df, hypso, wtr_lower, wtr_upper, cum_ba) {
  
  stopifnot(!any(is.na(wtr_df))) # NA wtr columns should be removed before this fxn
  stopifnot(all(hypso$depths == cummax(hypso$depths))) # Stop if hypso is not in order
  
  z_wtr <- rLakeAnalyzer::get.offsets(wtr_df)
  z_max <- max(hypso$depths, z_wtr) # bottom could be in hypso or wtr
  z_surface <- 0 # always 0, might be in hypso, might not
  
  wtr_surface <- wtr_df[[which.min(z_wtr)]] # wtr_surface will be whatever the top-most wtr is
  wtr_bottom <- wtr_df[[which.max(z_wtr)]] # wtr_bottom will be whatever the bottom-most wtr is
  
  ##### Find exact depths of wtr thresholds 
  
  Z1_Z2 <- apply(wtr_df, MARGIN = 1, function(wtr_row) {
    # Using apply since these two actions need to be done per row
    wtr <- as.vector(t(wtr_row))
    if(length(unique(wtr)) == 1) {
      # In a well-mixed lake, it is possible for all wtr values to be the same
      # which causes approx to throw an error. Return NAs for the Zs and let
      # checks below determine if benth area is all (or partially) above or 
      # below the lake and set Zs based on that.
      Z1_Z2 <- c(NA,NA)
    } else {
      Z1_Z2 <- approx(wtr, z_wtr, xout=c(wtr_upper, wtr_lower))$y
    }
    return(Z1_Z2)
  })
  Z1 <- Z1_Z2[1,]
  Z2 <- Z1_Z2[2,]
  
  ##### Checks before calculating areas
  completely_below_lake <- wtr_bottom > wtr_upper # if THA is completely below lake, benthic area is 0 (too hot)
  completely_above_lake <- wtr_surface < wtr_lower # if THA is completely above lake, benthic area is 0 (too cold)
  benth_0 <- completely_below_lake | completely_above_lake
  
  # if THA extends above lake, use the surface as Z1 to calc THA
  extends_above_lake <- wtr_surface < wtr_upper & !completely_above_lake
  Z1[extends_above_lake] <- z_surface
  
  # if THA extends below lake, use the bottom as Z2 to calc THA
  extends_below_lake <- wtr_bottom > wtr_lower & !completely_below_lake
  Z2[extends_below_lake] <- z_max
  
  ##### Create hypso at these different depths
  A1 <- resample_hypso(hypso, Z1)[["areas"]]
  A2 <- resample_hypso(hypso, Z2)[["areas"]]
  
  ##### Use areas and depths at thresholds to calculate benthic area
  
  benthic_area <- calc_benthic_area_incl_hypso(Z1, Z2, A1, A2, hypso, cum_ba)
  
  ##### Final benthic area checks
  
  # Add lake bottom if bottom of optical habitat is along lake bottom (or extends below)
  benthic_area[extends_below_lake] <- benthic_area[extends_below_lake] + tail(hypso$areas, 1)
  
  # Set benthic areas to zero based on above criteria
  benthic_area[benth_0] <- 0
  
  return(data.frame(Z1 = Z1, Z2 = Z2, habitat = benthic_area))
}

# Returns vector, not data.frame
thermal_optical_habitat_area <- function(tha_df, oha_df, hypso, cum_ba) {
  
  stopifnot(nrow(tha_df) == nrow(oha_df))
  
  # Make a placeholder TOHA vector
  toha <- rep(NA, nrow(oha_df))
  
  ##### Find instances where TOHA is zero #####
  
  # Check to see if TOHA is zero because either OHA or THA is zero
  # Meaning either OHA or THA (or both) are above/below the lake
  i_THA_OHA_zero <- which(tha_df$habitat == 0 | oha_df$habitat == 0)
  
  # Check to see if THA and OHA do not overlap
  i_THA_above_OHA <- which(tha_df$Z1 <= oha_df$Z1 & tha_df$Z2 <= oha_df$Z1)
  i_OHA_above_THA <- which(oha_df$Z1 <= tha_df$Z1 & oha_df$Z2 <= tha_df$Z1)
  
  # Combine those indices that should have zero TOHA
  i_zero_toha <- unique(c(i_THA_OHA_zero, i_THA_above_OHA, i_OHA_above_THA))
  
  # Add zeros to TOHA vector
  toha[i_zero_toha] <- 0
  
  ##### Find instances where TOHA doesn't need to be calculated #####
  # This would be times where TOHA = THA or TOHA = OHA
  
  # THA inside of OHA, therefore TOHA = THA
  i_THA_inside_OHA <- which(tha_df$Z1 >= oha_df$Z1 & tha_df$Z2 <= oha_df$Z2)
  toha[i_THA_inside_OHA] <- tha_df$habitat[i_THA_inside_OHA]
  
  # OHA inside of THA, therefore TOHA = OHA
  i_OHA_inside_THA <- which(oha_df$Z1 >= tha_df$Z1 & oha_df$Z2 <= tha_df$Z2)
  toha[i_OHA_inside_THA] <- oha_df$habitat[i_OHA_inside_THA]
  
  ##### Combine all of the indices where TOHA could be determined without calculating #####
  i_noncalc <- c(i_zero_toha, i_THA_inside_OHA, i_OHA_inside_THA)
  to_calc <- which(!seq_len(length(toha)) %in% i_noncalc) # handles instance where i_noncalc is empty
  
  if(length(to_calc) > 0) {
    ##### Calculate benthic area where TOHA couldn't already be determined
    # The following is only done for those instances where TOHA has yet to 
    # be determined.
    
    # Resuling matrix is 4 x nrow(oha_df[to_calc]):
    #   Columns: one column per timestep that still needs TOHA calculated
    #   Row 1: OHA Z1s
    #   Row 2: OHA Z2s
    #   Row 3: THA Z1s
    #   Row 4: THA Z2s
    
    all_Zs_to_calc <- t(matrix(c(oha_df$Z1[to_calc], oha_df$Z2[to_calc], 
                                 tha_df$Z1[to_calc], tha_df$Z2[to_calc]),
                               ncol = 4))
    all_Zs_sorted <- apply(all_Zs_to_calc, MARGIN = 2, sort) # sort each column's depths
    
    # Find the new Z's - should be the two middle values when you sort each column
    # So i = 2, and i = 3 (there are 4 columns, Z1 & Z2 for both THA & OHA)
    Z1 <- as.vector(all_Zs_sorted[2,])
    Z2 <- as.vector(all_Zs_sorted[3,])
    
    A1 <- resample_hypso(hypso, Z1)[["areas"]]
    A2 <- resample_hypso(hypso, Z2)[["areas"]]
    
    ##### Use areas and depths at thresholds to calculate benthic area for TOHA
    
    benthic_area <- calc_benthic_area_incl_hypso(Z1, Z2, A1, A2, hypso, cum_ba)
    
    toha[to_calc] <- benthic_area
  }
  
  stopifnot(!any(is.na(toha)))
  
  return(toha)
}

resample_hypso <- function(hypso, new_depths) {
  
  # `rule = 2`: repeats top or bottom known hypso for any depths outside of known hypso
  hypso$radii <- sqrt(hypso$areas / pi)
  new_radii <- approx(hypso$depths, hypso$radii, xout=new_depths, rule = 2)$y
  
  # Now calculate area from new radii
  new_areas <- pi * new_radii^2
  
  # Create hypso at these new depths
  return(list(depths = new_depths, areas = new_areas))
}

calc_benthic_area <- function(Z1, Z2, A1, A2) {
  trap_length <- sqrt(A2 * pi) + sqrt(A1 * pi)
  trap_height <- sqrt( (Z1 - Z2)^2 + (sqrt(A2/pi) - sqrt(A1/pi))^2 )
  benthic_area <- trap_length * trap_height
  
  return(benthic_area)
}

calc_benthic_area_incl_hypso <- function(Z1, Z2, A1, A2, hypso, cum_ba) {
  # Include hypsographic information to take known depth/areas relationships
  #   that are between Z1 and Z2 into account. Using `sapply` at the end to be able
  #   to keep Z1 and Z2 vectorized and get benthic area for each combo returned. 
  
  # This function assumes hypso$depths and hypso$areas are sorted top to bottom
  # and Z1<Z2, A1>A2
  
  stopifnot(length(Z1) == length(Z2))
  
  # Find which hypso depths are just above the Z1s & Z2s
  top_interval <- findInterval(Z1, hypso$depths)
  bottom_interval <- findInterval(Z2, hypso$depths)
  
  # Create vector of known BA for known depths between Z1 & Z2
  middle_benthic_areas <- rep(0, length(Z1))
  ba_to_use <- top_interval + 1 < bottom_interval
  middle_benthic_areas[ba_to_use] <- 
    cum_ba[bottom_interval[ba_to_use]-1] - 
    cum_ba[top_interval[ba_to_use]]
  
  # If there are no known depths between Z1 & Z2, then there is
  # no middle area and we should set bottom & top interval to NA
  no_middle_area <- bottom_interval == top_interval 
  bottom_interval[no_middle_area] <- NA
  top_interval[no_middle_area] <- NA
  
  # Combine the Z1s and Z2s. Note: `length(bZ1) == 2*length(Z1)`
  # Setting up vectors to calculate BA between Z1 and the next 
  #   known depth using bottom interval & between Z2 and known 
  #   depth just above Z2.
  bZ1 <- c(Z1, hypso$depths[bottom_interval])
  bZ2 <- c(hypso$depths[top_interval+1], Z2)
  bA1 <- c(A1, hypso$areas[bottom_interval])
  bA2 <- c(hypso$areas[top_interval+1], A2)
  
  # Replace bottom interval with Z2 & A2 if no middle area to
  # use Z1 & Z2 together.
  bZ2[no_middle_area] <- Z2[no_middle_area]
  bA2[no_middle_area] <- A2[no_middle_area]
  
  # Calculate all of these benthic areas. Because of how the 
  # vectors were set up above, `length(ba) == 2*length(Z1)`
  ba <- calc_benthic_area(bZ1, bZ2, bA1, bA2)
  
  stopifnot(length(ba) == 2*length(Z1))
  
  # `ba` has 2 BA values for each Z1 and we can assume that the
  #   second values are just appended to the end of the first vector. 
  # We need to sum each combination of BA values, so creating a column
  #   for each combination and then summing.
  colSums(matrix(ba, nrow = 2, ncol = length(Z1), byrow=TRUE), na.rm = TRUE) +
    middle_benthic_areas # total benthic area
  
}



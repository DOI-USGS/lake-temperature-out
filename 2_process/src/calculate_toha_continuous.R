
opti_thermal_habitat_subdaily_continuous <- function(current_date, wtr, io, kd, lat, lon, hypsos, irr_thresh, wtr_thresh) {
  
  na_depth_profiles <- sapply(wtr, function(x) { all(is.na(x)) })
  wtr_rmNA <- wtr[, !na_depth_profiles] # remove any temp profiles that are NA
  
  io <- mda.lakes::create_irr_day_cycle(lat,lon, dates=current_date, irr_mean = io, by='min')
  
  oha_df <- optical_habitat_area(io[[2]], kd, hypsos, irr_thresh[1], irr_thresh[2])
  tha_df <- thermal_habitat_area(wtr_rmNA, hypsos, wtr_thresh[1], wtr_thresh[2])
  
  # Upsample tha (repeat values) to match the number of oha values (subdaily)
  # Will even work if `nrow(wtr_rmNA) > 1`
  tha_upsampled <- tha_df %>% slice(rep(1:n(), each = nrow(oha_df)))
  toha <- thermal_optical_habitat_area(tha_upsampled, oha_df, hypsos)
  
  # Divide by number of timesteps to get average area per day
  oha_daily <- sum(oha_df$habitat)/nrow(oha_df) 
  tha_daily <- tha_df$habitat/nrow(tha_df) # should be the same before/after division since tha_df is already daily
  toha_daily <- sum(toha)/length(toha)
  
  return(data.frame(opti_hab = oha_daily, 
                    therm_hab = tha_daily, 
                    opti_therm_hab = toha_daily))
  
}

# Vectorized function
optical_habitat_area <- function(I_0, Kd, hypso, I_lower, I_upper) {
  
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
  
  benthic_area <- calc_benthic_area(Z1, Z2, A1, A2)
  
  ##### Final benthic area checks
  
  # Add lake bottom if bottom of optical habitat 
  benthic_area[extends_below_lake] <- benthic_area[extends_below_lake] + tail(hypso$areas, 1)
  
  # Set benthic areas to zero based on above criteria
  benthic_area[benth_0] <- 0 
  
  return(data.frame(Z1 = Z1, Z2 = Z2, habitat = benthic_area))
}

# Now vectorized (except for one apply fxn)
thermal_habitat_area <- function(wtr_df, hypso, wtr_lower, wtr_upper) {
  
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
    approx(wtr, z_wtr, xout=c(wtr_upper, wtr_lower))$y
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
  
  benthic_area <- calc_benthic_area(Z1, Z2, A1, A2)
  
  ##### Final benthic area checks
  
  # Add lake bottom if bottom of optical habitat is along lake bottom (or extends below)
  benthic_area[extends_below_lake] <- benthic_area[extends_below_lake] + tail(hypso$areas, 1)
  
  # Set benthic areas to zero based on above criteria
  benthic_area[benth_0] <- 0 
  
  return(data.frame(Z1 = Z1, Z2 = Z2, habitat = benthic_area))
}

# Returns vector, not data.frame
thermal_optical_habitat_area <- function(tha_df, oha_df, hypso) {
  
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
    
    benthic_area <- calc_benthic_area(Z1, Z2, A1, A2)
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

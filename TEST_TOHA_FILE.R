# Work on TOHA calc (this will get moved into pipeline later)

i0_df <- readRDS("2_process/out/combined_irradiance_grp_01.rds")
temp_df <- readRDS("2_process/out/combined_temp_pred_grp_01.rds")

toha_ready_data <- bind_cols(i0_df, select(temp_df,-date))

# Now what ...
# https://github.com/USGS-R/mda.lakes/blob/master/R/opti_thermal_habitat.R
# https://github.com/USGS-R/mda.lakes/blob/afb436e047d2a9ca30dfdeae13745d2ee5109455/R/volume_light_threshold.R

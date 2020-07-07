
filter_observed_data <- function(obs_fn) {
  
  observed_dat <- read_feather(obs_fn)
  
  # Filter so that there is more than 1 temperature value 
  # per date (more than one depth)
  dates_to_keep <- observed_dat %>% 
    group_by(date) %>%
    summarize(count = n()) %>% 
    filter(count > 1) %>% 
    pull(date)
  
  observed_dat %>% 
    filter(date %in% dates_to_keep)
  
}

reformat_observed_data <- function(obs_data) {
  
  obs_data %>% 
    # This is the date column name that `calculate_toha_per_lake` expects
    rename(DateTime = date) %>% 
    mutate(col_name = paste0("temp_", depth)) %>%  
    arrange(depth) %>% 
    # Reformat to be `temp_[depth]` as columns with one row per date
    tidyr::pivot_wider(id_cols = c("site_id", "DateTime"), 
                       names_from = col_name, 
                       values_from = temp) 
  
}

join_observed_data <- function(target_name, obs_data, irr_data_fn, k0_data_fn) {
  
  # Reformat to be `temp_[depth]` as columns with one row per date
  obs_data %>% 
    left_join(read_csv(irr_data_fn, col_types = cols()), by = c("DateTime" = "date"), progress = FALSE) %>%
    rename(io = rad_0) %>% 
    left_join(read_csv(k0_data_fn, col_types = cols()), by = c("DateTime" = "date"), progress = FALSE) %>% 
    select(site_id, DateTime, io, kd, everything()) %>% 
    write_feather(target_name)
  
}


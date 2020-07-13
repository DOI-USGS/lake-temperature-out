
munge_observed_data <- function(target_name, obs_data_fn, irr_data_fn, kd_data_fn) {
  
  options(dplyr.summarise.inform = FALSE) # new dplyr 1.0 messaging is annoying so turn it off
  
  observed_dat <- read_feather(obs_data_fn) %>% 
    filter_observed_data() %>% 
    reformat_observed_data() %>% 
    join_observed_data(irr_data_fn, kd_data_fn) %>% 
    write_feather(target_name)
  
}

filter_observed_data <- function(obs_data) {

  # The modeled data's temperature values were in a column called
  # `pred` and were character, but the observed data had a column
  # called `temp`
  if(!"temp" %in% names(obs_data)) obs_data$temp <- as.numeric(obs_data$pred)
  
  # Filter so that there is more than 1 temperature value 
  # per date (more than one depth)
  dates_to_keep <- obs_data %>% 
    filter(!is.na(temp)) %>% 
    group_by(date) %>%
    summarize(count = n()) %>% 
    filter(count > 1) %>% 
    pull(date)
  
  obs_data %>% 
    filter(date %in% dates_to_keep) %>% 
    # Discovered during `reformat_observed_data` that some sites did
    # not have unique combinations of date & depth which causes the
    # pivoted data to have lists instead of single values. To fix, I
    # am averaging the temperature values if there are more than 1.
    group_by(site_id, date, depth) %>% 
    summarize(temp = mean(temp)) %>% 
    ungroup()
  
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

join_observed_data <- function(obs_data, irr_data_fn, kd_data_fn) {
  
  # Reformat to be `temp_[depth]` as columns with one row per date
  obs_data %>% 
    left_join(read_csv(irr_data_fn, col_types = cols()), by = c("DateTime" = "date"), progress = FALSE) %>%
    rename(io = rad_0) %>% 
    left_join(read_csv(kd_data_fn, col_types = cols()), by = c("DateTime" = "date"), progress = FALSE) %>% 
    select(site_id, DateTime, io, kd, everything())
  
}

split_observed_data <- function(target_name, obs_zipfile, split_file_prefix) {
  
  # Unzip the file
  unzipped_obs_file <- unzip(zipfile = obs_zipfile, overwrite = TRUE, exdir = tempdir())
  
  # Read in the csv & split into a feather file per lake
  local_filenames <- readr::read_csv(unzipped_obs_file, col_types = 'cDddc') %>% 
    split(.$site_id) %>% 
    purrr::map(function(data) {
      split_fn <- sprintf("2_process/tmp/%s_%s.feather", split_file_prefix, unique(data$site_id))
      write_feather(data, split_fn)
      return(split_fn)
    }) %>% 
    purrr::reduce(c)
  
  scipiper::sc_indicate(target_name, data_file = local_filenames)
}

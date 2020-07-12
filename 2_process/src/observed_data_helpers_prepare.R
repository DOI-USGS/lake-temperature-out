unzip_and_split_observed_data <- function(target_name, obs_zipfile, split_file_prefix) {
  
  # Unzip the file
  unzipped_obs_file <- unzip(zipfile = obs_zipfile, overwrite = TRUE, exdir = tempdir())
  
  # Read in the csv & split into a feather file per lake
  local_filenames <- readr::read_csv(unzipped_obs_file, col_types = cols()) %>% 
    split(.$site_id) %>% 
    purrr::map(function(data) {
      split_fn <- sprintf("%s_%s.feather", split_file_prefix, unique(data$site_id))
      write_feather(data, split_fn)
      return(split_fn)
    }) %>% 
    unlist()
  
  scipiper::sc_indicate(target_name, data_file = local_filenames)
}

unzip_data <- function(target_name, data_file, out_dir) {
  # Sometimes (as is the case with irradiance and clarity), the incoming
  # file has multiple zip files that need to be unpacked and saved
  unzipped_data_files <- lapply(
    names(yaml::yaml.load_file(data_file)), 
    unzip, 
    overwrite = TRUE, exdir = out_dir) %>% 
    unlist()
  
  scipiper::sc_indicate(target_name, data_file = unzipped_data_files)
}

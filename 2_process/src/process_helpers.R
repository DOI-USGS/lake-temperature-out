
get_group_ids <- function(group_info) {
  sort(unique(group_info$group_id))
}

extract_morphometry <- function(config_fn) {
  
  config <- fromJSON(config_fn)
  morphometry <- purrr::map(config, "morphometry")
  
  # Only keep specific elements of morphometry
  morphometry_out <- purrr::map(morphometry, `[`, c("latitude", "longitude", "H", "A"))
  
  return(morphometry_out)
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

subset_yml <- function(target_name, full_yml, regex) {
  full_yml_list <- yaml::yaml.load_file(full_yml)
  yaml::write_yaml(full_yml_list[grep(regex, names(full_yml_list))], target_name)
}

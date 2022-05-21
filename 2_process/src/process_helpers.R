
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

#' Pass in vectors of site ids to `...` and those will be used to subset
#' the NML list to only the sites we need.
nml_to_morphometry_subset <- function(in_ind, ...) {
  site_ids_unique <- c(...) %>% unique()
  morph_all <- purrr::map(readRDS(as_data_file(in_ind)), `[`, c("latitude", "longitude", "H", "A"))
  morph_subset <- morph_all[names(morph_all) %in% site_ids_unique]
  return(morph_subset)
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

#' `regex_grp_nms` should be the same length as the number of groups
#' represented in the `regex`. At least one of the `regex_grp_nms`
#' needs to be `site_id`
extract_site_ids <- function(file_ind, regex, regex_grp_nms) {
  site_files <- get_filenames_from_ind(file_ind)
  site_ids <- tibble(site_fl = site_files) %>% 
    extract(site_fl, regex_grp_nms, regex) %>%
    pull(site_id) %>% 
    unique()
  return(site_ids)
}

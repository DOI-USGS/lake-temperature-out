
get_group_ids <- function(group_info) {
  sort(unique(group_info$group_id))
}

extract_morphometry <- function(config_fn) {
  
  config <- fromJSON(config_fn)
  morphometry <- purrr::map(config, "morphometry")
  
  return(morphometry)
}


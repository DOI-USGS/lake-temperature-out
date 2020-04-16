rezip <- function(target_name, sb_group_xwalk, sb_group_ids, toha_lake_ind, toha_dir = "2_process/out") {
  
  toha_lake_files <- names(yaml::yaml.load_file(toha_lake_ind))
  
  # Need to change working directory to directory with files that you are zipping
  cd <- getwd()
  on.exit(setwd(cd))
  
  zipped_files <- purrr::map(sb_group_ids, function(id) {
    lakes_in_group <- filter(sb_group_xwalk, group_id == id) %>% pull(site_id)
    files_to_zip <- toha_lake_files[basename(toha_lake_files) %in% sprintf("toha_%s.csv", lakes_in_group)]
    if(length(files_to_zip) == 0) { 
      return(sprintf("Group %s: no files to zip", id)) 
    } else {
      setwd(toha_dir)
      
      # Windows users: note that this may silently fail for you
      #   Please see: https://stackoverflow.com/a/52014909
      zip_fn <- sprintf("toha_%s.zip", id)
      if(file.exists(zip_fn)) file.remove(zip_fn)
      zip(zip_fn, files = basename(files_to_zip))
      
      unlink(toha_dir, recursive = TRUE)
      setwd(cd)
      return(file.path(toha_dir, zip_fn))
    }
  }) %>% unlist()
  
  saveRDS(zipped_files, target_name)
  
}

get_group_ids <- function(group_info) {
  sort(unique(group_info$group_id))
}

extract_morphometry <- function(config_fn) {
  
  config <- fromJSON(config_fn)
  morphometry <- purrr::map(config, "morphometry")
  
  return(morphometry)
}


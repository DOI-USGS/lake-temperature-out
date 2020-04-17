rezip <- function(target_name, sb_group_xwalk, sb_group_ids, files_to_zip_ind, dest_dir) {
  
  zip_files_all <- names(yaml::yaml.load_file(files_to_zip_ind))
  
  # Need to change working directory to directory with files that you are zipping
  cd <- getwd()
  on.exit(setwd(cd))
  
  zipped_files <- purrr::map(sb_group_ids, function(id) {
    lakes_in_group <- filter(sb_group_xwalk, group_id == id) %>% pull(site_id)
    files_to_zip <- zip_files_all[grep(paste(lakes_in_group, collapse="|"), zip_files_all, perl=TRUE)]
    
    if(length(files_to_zip) == 0) { 
      message(sprintf("Group %s: no files to zip", id))
      return() 
    } else {
      setwd(dest_dir)
      
      # Windows users: note that this may silently fail for you
      #   Please see: https://stackoverflow.com/a/52014909
      zip_fn <- sprintf("toha_%s.zip", id)
      if(file.exists(zip_fn)) file.remove(zip_fn)
      zip(zip_fn, files = basename(files_to_zip))
      
      unlink(dest_dir, recursive = TRUE)
      setwd(cd)
      return(file.path(dest_dir, zip_fn))
    }
  }) %>% unlist()
  
  scipiper::sc_indicate(target_name, data_file = zipped_files)
  
}

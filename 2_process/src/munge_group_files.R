unzip_and_merge_files <- function(lake_ids, irradiance_zipfile, clarity_zipfile, temp_zipfile, fn_out_template) {
  
  unzipped_irradiance_files <- unzip(zipfile = irradiance_zipfile, overwrite = TRUE, exdir = tempdir())
  unzipped_clarity_files <- unzip(zipfile = clarity_zipfile, overwrite = TRUE, exdir = tempdir())
  unzipped_temp_files <- unzip(zipfile = temp_zipfile, overwrite = TRUE, exdir = tempdir())
  
  merged_files <- purrr::map(lake_ids, function(site_id) {
    
    irr_fn <- unzipped_irradiance_files[grep(site_id, unzipped_irradiance_files, perl=TRUE)]
    kw_fn <- unzipped_clarity_files[grep(site_id, unzipped_clarity_files, perl=TRUE)]
    temp_fn <- unzipped_temp_files[grep(site_id, unzipped_temp_files, perl=TRUE)]
    
    # Catch when a lake is missing some data and record what it is missing.
    if(length(c(irr_fn, kw_fn, temp_fn)) < 3) {
  
      site_id_fn <- ""
      
    } else {
      site_id_fn <- sprintf(fn_out_template, site_id)
      merged_data <- read_csv(irr_fn, col_types = cols(), progress = FALSE) %>% 
        left_join(read_csv(kw_fn, col_types = cols()), by = "date", progress = FALSE) %>%
        left_join(read_csv(temp_fn, col_types = cols()), by = "date", progress = FALSE) %>%
        mutate(site_id = site_id) %>% 
        select(site_id, DateTime = date, io = rad_0, kd, everything()) %>% 
        filter(!is.na(temp_0))
      write_feather(merged_data, site_id_fn)
      
    }
    
    return(tibble(filename = site_id_fn, hash = tools::md5sum(site_id_fn)))
  }) %>% 
    purrr::reduce(bind_rows) %>% filter(!filename == "") #remove empty
  
  merged_files
}

get_lakes_from_group <- function(sb_groups, grp_id) {
  filter(sb_groups, group_id == grp_id) %>% pull(site_id)
}

indicate_file_dataframes <- function(target_name, ...) {
  # save as yml with format with file:hash
  bind_rows(...) %>% # combine all data frames from each group
    tidyr::unite(formatted, sep = ": ") %>% 
    pull(formatted) %>% 
    writeLines(con = target_name)
}

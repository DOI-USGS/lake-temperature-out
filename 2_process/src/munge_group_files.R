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
      
      missing_message <- sprintf("%s is missing %s", site_id, 
                                 paste(stringi::stri_remove_empty(c(
                                   ifelse(length(irr_fn) == 0, "irradiance", ""),
                                   ifelse(length(kw_fn) == 0, "clarity", ""),
                                   ifelse(length(temp_fn) == 0, "temperature", ""))), collapse = " & "))
      message(missing_message)
      site_id_fn <- ""
      
    } else {
      
      site_id_fn <- sprintf(fn_out_template, site_id)
      merged_data <- read_csv(irr_fn, col_types = cols()) %>% 
        left_join(read_csv(kw_fn, col_types = cols()), by = "date") %>%
        left_join(read_csv(temp_fn, col_types = cols()), by = "date") %>%
        mutate(site_id = site_id) %>% 
        select(site_id, DateTime = date, io = rad_0, kd, everything())
      write_feather(merged_data, site_id_fn)
      
    }
    
    return(tibble(filename = site_id_fn, hash = tools::md5sum(site_id_fn)))
  }) %>% 
    purrr::reduce(bind_rows)
  
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

unzip_and_combine_files <- function(target_name, keyword, all_zipfiles_file, grp) {
  
  all_zipfilenames <- readRDS(all_zipfiles_file)
  zipfile <- all_zipfilenames[grepl(sprintf("%s_%s", keyword, grp), all_zipfilenames)]
  
  unzipped_files <- unzip(zipfile = zipfile, overwrite = TRUE, exdir = tempdir())
  purrr::map(unzipped_files, function(fn) {
    read_csv(fn, col_types = cols())
  }) %>% 
    purrr::reduce(bind_rows) %>% 
    saveRDS(target_name)
  
}

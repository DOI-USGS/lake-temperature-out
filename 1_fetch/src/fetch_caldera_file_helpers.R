
#' Indicate all files from a single directory into a single hash table
indicate_dir_files <- function(out_ind, file_dir, file_pattern) {
  files_to_indicate <- list.files(path = file_dir, 
                                  pattern = file_pattern, 
                                  full.names = TRUE)
  sc_indicate(out_ind, data_file = files_to_indicate)
}

#' Use `out_dir` if you are copying multiple files and saving
#' a hash table as the ind. Leave as `NULL` if you are only 
#' copying and saving one file.
copy_caldera_files <- function(out_ind, files_in, out_dir = NULL) {
  
  if(is.null(out_dir)) {
    files_out <- as_data_file(out_ind)
  } else {
    files_out <- file.path(out_dir, basename(files_in))
  }
  
  file.copy(from = files_in, to = files_out)
  sc_indicate(out_ind, data_file = files_out)
  
}

#' @title Extract filenames from a table 
#' @description Using an output table of files from successful GLM
#' runs, extract the filenames and hashes and save into a yml.
#' @param glm_run_output_file filepath of a CSV file with at least
#' the columns, `export_fl` and `export_fl_hash`
extract_glm_files <- function(out_ind, glm_run_output_file) {
  
  files_to_indicate <- file.path(glm_run_output_file) %>% 
    readr::read_csv() %>% 
    ## TODO: DELETE THIS FILTER ##
    filter(state == "MN") %>% 
    ## ^^ DELETE THAT^^
    mutate(filepath = file.path('../lake-temperature-process-models', export_fl)) %>% 
    pull(filepath)
  
  sc_indicate(out_ind, data_file = files_to_indicate)
} 

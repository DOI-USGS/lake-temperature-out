
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

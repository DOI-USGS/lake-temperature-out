# Split morphometry into separate files per lake to avoid too many processes trying to access the same file
# Doing this so that these are only done once per lake rather than once per task (lake + driver)

split_morphometry <- function(final_target, dir, morphometry) {
  
  # Construct filepaths for saving the individual site morphometry files
  morph_file_inds <- sprintf("%s/%s_morphometry.rds.ind", dir, names(morphometry))
  
  # Map over the morphometry list & the ind filenames to save each as a separate file
  purrr::map2(morph_file_inds, morphometry, save_morphometry)
  
  # Create a single ind file with all the individual files and hashes 
  sc_indicate(final_target, data_file = morph_file_inds)
}

save_morphometry <- function(out_ind, morphometry_subset) {
  data_file <- as_data_file(out_ind)
  saveRDS(morphometry_subset, data_file)
  sc_indicate(out_ind, data_file)
}

list_sb_files <- function(target_name, sb_id, keyword) {
  
  sb_files <- item_list_files(sb_id)[["fname"]]
  keyword_sb_files <- sb_files[grepl(keyword, sb_files)]

  saveRDS(object = keyword_sb_files, file = target_name)
}

download_sb_files <- function(target_name, sb_id, sb_filenames_file, tmp_folder) {
  
  sb_filenames <- readRDS(sb_filenames_file)
  
  local_filenames <- item_file_download(
    sb_id, 
    names = sb_filenames, 
    destinations = file.path(tmp_folder, sb_filenames))
  
  saveRDS(object = local_filenames, file = target_name)
}

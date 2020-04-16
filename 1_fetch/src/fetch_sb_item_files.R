
download_sb_files <- function(target_name, sb_id, keyword, dest_folder) {
  
  sb_check_login()
  
  sb_filenames <- list_sb_files(sb_id, keyword)
  
  local_filenames <- item_file_download(
    sb_id, 
    names = sb_filenames, 
    destinations = file.path(dest_folder, sb_filenames),
    overwrite_file = TRUE)
  
  saveRDS(object = local_filenames, file = target_name)
}

download_sb_single_file <- function(target_name, sb_id, sb_filename) {
  
  sb_check_login()
  
  local_filename <- item_file_download(
    sb_id, 
    names = sb_filename, 
    destinations = target_name, 
    overwrite_file = TRUE)
  
}

sb_check_login <- function() {
  if (!sbtools::is_logged_in()){
    sb_secret <- dssecrets::get_dssecret("cidamanager-sb-srvc-acct")
    sbtools::authenticate_sb(username = sb_secret$username, password = sb_secret$password)
  }
}

list_sb_files <- function(sb_id, keyword) {
  sb_files <- item_list_files(sb_id)[["fname"]]
  keyword_sb_files <- sb_files[grepl(keyword, sb_files)]
  return(keyword_sb_files)
}

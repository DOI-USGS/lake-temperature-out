
calculate_max_benthic_area <- function(target_name, all_hypsos) {
  
  purrr::map(all_hypsos, function(hypso) {
    sum(benthic_areas(hypso$H, hypso$A))
  }) %>%
    # `enframe` required me to update tibble (I had `2.1.3` and now have `3.0.1`)
    tibble::enframe(name = "site_id", value = "max_benthic_area") %>% 
    tidyr::unnest(max_benthic_area) %>% 
    readr::write_csv(target_name)
  
}

extract_hypsography <- function(morphometry) {
  # Only keep depths (H) and areas (A)
  purrr::map(morphometry, `[`, c("H", "A"))
}

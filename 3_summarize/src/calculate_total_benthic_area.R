
calculate_total_benthic_area <- function(target_name, morphometry) {
  
  purrr::map(morphometry, function(m) {
    # Order of depths & areas will be fixed in benthic_areas
    sum(benthic_areas(depths = max(m$H) - m$H, areas = m$A))
  }) %>%
    # `enframe` required me to update tibble (I had `2.1.3` and now have `3.0.1`)
    # Takes a list and creates a two-column data.frame (one column is the names, one is the values)
    tibble::enframe(name = "site_id", value = "max_benthic_area") %>% 
    tidyr::unnest(max_benthic_area) %>% 
    readr::write_csv(target_name)
  
}

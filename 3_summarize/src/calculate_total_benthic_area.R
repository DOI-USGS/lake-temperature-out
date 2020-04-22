
calculate_total_benthic_area <- function(target_name, all_hypsos) {
  
  purrr::map(all_hypsos, function(hypso) {
    sum(benthic_areas(hypso$depths, hypso$areas))
  }) %>%
    # `enframe` required me to update tibble (I had `2.1.3` and now have `3.0.1`)
    # Takes a list and creates a two-column data.frame (one column is the names, one is the values)
    tibble::enframe(name = "site_id", value = "max_benthic_area") %>% 
    tidyr::unnest(max_benthic_area) %>% 
    readr::write_csv(target_name)
  
}

extract_hypsography <- function(morphometry) {
  # Only keep depths (converted from Heights, H) and areas (A)
  purrr::map(morphometry,  function(m) {
    hypsos <- data.frame(H = m$H, A = m$A) %>% 
      mutate(depths = max(H) - H, areas = A) %>% 
      arrange(depths) %>% 
      select(depths, areas)
  })
  
}

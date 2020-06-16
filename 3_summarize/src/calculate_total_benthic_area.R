
calculate_total_benthic_area <- function(target_name, morphometry) {
  
  purrr::map(morphometry, function(m) {
    hypso <- data.frame(depths = max(m$H) - m$H, areas = m$A) %>% arrange(depths)
    sum(calc_benthic_area(Z1 = head(hypso$depths, -1), Z2 = tail(hypso$depths, -1),
                          A1 = head(hypso$areas, -1), A2 = tail(hypso$areas, -1))) + 
      # Add lake bottom:
      tail(hypso$areas, 1)
  }) %>%
    # `enframe` required me to update tibble (I had `2.1.3` and now have `3.0.1`)
    # Takes a list and creates a two-column data.frame (one column is the names, one is the values)
    tibble::enframe(name = "site_id", value = "total_benthic_area") %>% 
    tidyr::unnest(total_benthic_area) %>% 
    readr::write_csv(target_name)
  
}

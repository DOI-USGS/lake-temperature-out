# Visualize output

plot_annual_metric_summaries <- function(target_name, in_file, target_dir, model_id_colname) {
  
  if(!dir.exists(target_dir)) dir.create(target_dir)
  
  annual_metrics_data <- readr::read_csv(in_file)
  
  cols2plot <- names(annual_metrics_data)[which(!grepl(sprintf("site_id|year|_dt|_date|%s", model_id_colname), names(annual_metrics_data)))]
  
  annual_metrics_long <- annual_metrics_data %>% 
    select(-matches("_dt|_date")) %>% 
    pivot_longer(all_of(cols2plot), names_to = "metric")
  
  plot_groups <- split(cols2plot, ceiling(seq_along(cols2plot)/10))
  files_out <- purrr::map(plot_groups, function(i) {
    p <- annual_metrics_long %>% 
      filter(metric %in% i) %>% 
      ggplot(aes(x = year, y = value)) + 
      facet_grid(metric ~ ., scales = "free_y") +
      geom_point(alpha = 0.2, stroke = 0, shape = 16, size = 2)
    ggsave(sprintf("%s/%s_THROUGH_%s.png", target_dir, i[1], i[10]),
           width = 5, height = 8)
  })
  
  scipiper::sc_indicate(target_name, data_file = unlist(files_out))
}




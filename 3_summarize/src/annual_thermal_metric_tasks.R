
do_annual_metrics_multi_lake <- function(final_target, site_files, ice_files, ...) {
  
  # Define task table rows
  tasks <- tibble(wtr_filename = site_files) %>% 
    extract(wtr_filename, c('prefix','site_id','suffix'), "(pb0|pball|pgdl)_data_(.*)(.feather)", remove = FALSE) %>% 
    left_join(extract(tibble(ice_filename = ice_files), ice_filename, c('site_id'), "pb0_(.*)_ice_flags.csv", remove = FALSE), by = "site_id") %>% 
    select(site_id, wtr_filename, ice_filename, prefix)
  
  model_type <- pull(tasks, prefix) %>% unique()
  
  # Define task table columns
  
  # I'm doing this because each toha task is slow running
  # and if `morphometry` changes, we don't want to have to re-run all of them,
  # just the ones where _their_ morphometry changed. So we are subsetting first. 
  split_morphometry <- scipiper::create_task_step(
    step_name = 'split_morphometry',
    target_name = function(task_name, step_name, ...){
      
      sprintf("%s_morphometry", task_name)
    },
    command = function(task_name, ...){
      sprintf("morphometry[[I('%s')]]", task_name)
    } 
  )
  
  calc_annual_metrics <- create_task_step(
    step_name = 'calc_annual_metrics',
    target_name = function(task_name, step_name, ...) {
      sprintf("%s_annual_thermal_metrics", task_name)
    },
    command = function(..., task_name, steps) {
      task_info <- filter(tasks, site_id == task_name)
      psprintf("calculate_annual_metrics_per_lake(", 
               "site_id = I('%s')," = task_name,
               "site_file = '%s'," = task_info$wtr_filename,
               "ice_file = '%s'," = task_info$ice_filename,
               "morphometry = `%s`)" = steps[["split_morphometry"]]$target_name
      )
    }
  )
  
  # Create the task plan
  task_plan <- create_task_plan(
    task_names = tasks$site_id,
    task_steps = list(split_morphometry, calc_annual_metrics),
    final_steps = c('calc_annual_metrics'),
    add_complete = FALSE)
  
  # Create the task remakefile
  task_makefile <- sprintf('3_summarize_%s_metric_tasks.yml', model_type)
  create_task_makefile(
    task_plan = task_plan,
    makefile = task_makefile,
    include = 'remake.yml',
    sources = c(...),
    packages = c('tidyverse', 'purrr', 'readr'),
    final_targets = final_target,
    finalize_funs = 'combine_thermal_metrics',
    as_promises = TRUE,
    tickquote_combinee_objects = TRUE)
  
  # Build the tasks
  loop_tasks(task_plan = task_plan,
             task_makefile = task_makefile,
             num_tries = 1)
  
  # Now return the file name of the final the combined CSV
  return(remake::fetch(sprintf("%s_promise", basename(final_target)), remake_file=task_makefile))
  
}

combine_thermal_metrics <- function(target_name, ...) {
  purrr::reduce(list(...), bind_rows) %>% readr::write_csv(target_name)
}

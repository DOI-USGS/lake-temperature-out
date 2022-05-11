
do_annual_metrics_multi_lake <- function(final_target, site_file_yml, ice_file_yml, n_cores, morphometry, temp_ranges, ...,
                                         site_file_regex = NULL, ice_file_regex = NULL, tmpdir_suffix = "", 
                                         model_id_colname = NULL, suffix_as_model_id = TRUE) {
  
  site_files <- get_filenames_from_ind(site_file_yml)
  
  if(!is.null(ice_file_yml)) {
    ice_files <- get_filenames_from_ind(ice_file_yml) 
  } else {
    ice_files <- ""
  }
  
  # Each node on a Yeti normal partition has a max of 20 cores; nodes on Yeti UV partition do not have that same limit
  if(n_cores > 20) message("If using a node on the Yeti normal partition, you need to decrease n_cores to 20 or less")
  
  if(is.null(site_file_regex)) {
    site_file_regex <- "(pb0|pball|pgdl)_data_(.*)(.feather)"
  }
  if(is.null(ice_file_regex)) {
    ice_file_regex <- "pb0_(.*)_ice_flags.csv"
  }
  
  # Having `remake.yml` listed in "include" for these large task plans
  # is expensive. In order to decouple the task makefile from `remake.yml`
  # to simplify the dependencies and complexity, we need to save some of the 
  # target objects used as files. Each task will read these new files but
  # we don't think that will be an issue, even when doing it in parallel.
  morph_file <- sprintf("tasks_all_morphometry.rds")
  saveRDS(morphometry, morph_file)
  
  temp_ranges_file <- sprintf("temp_ranges.rds")
  saveRDS(temp_ranges, temp_ranges_file)
  
  # Define task table rows
  tasks <- tibble(wtr_filename = site_files) %>% 
    extract(wtr_filename, c('prefix','site_id','suffix'), site_file_regex, remove = FALSE) %>% 
    left_join(extract(tibble(ice_filename = ice_files), ice_filename, c('site_id'), ice_file_regex, remove = FALSE), by = "site_id") %>% 
    select(site_id, wtr_filename, ice_filename, model_id = matches(ifelse(suffix_as_model_id, "suffix", "prefix")))
  
  model_type <- pull(tasks, model_id) %>% unique()
  
  # Define task table columns
  
  # I'm doing this because each toha task is slow running
  # and if `morphometry` changes, we don't want to have to re-run all of them,
  # just the ones where _their_ morphometry changed. So we are subsetting first. 
  split_morphometry <- scipiper::create_task_step(
    step_name = 'split_morphometry',
    target_name = function(task_name, step_name, ...){
      
      sprintf("3_summarize/tmp%s/%s_morphometry.rds.ind", tmpdir_suffix, task_name)
    },
    command = function(task_name, ...){
      sprintf("split_and_save_morphometry(target_name, '%s', I('%s'))", morph_file, task_name)
    } 
  )
  
  # Depending on how many models per site, this will be a list of one or more steps
  # Doing multiple here rather than another task table per model so that the morphometry files can be shared
  calc_annual_metrics <- purrr::map(model_type, function(model_type_i) { create_task_step(
    step_name = sprintf('calc_annual_metrics_%s', model_type_i),
    target_name = function(task_name, step_name, ...) {
      sprintf("3_summarize/tmp%s/%s_%s_annual_thermal_metrics.rds.ind", tmpdir_suffix, model_type_i, task_name)
    },
    command = function(..., task_name, steps) {
      task_info <- filter(tasks, site_id == task_name, model_id == model_type_i)
      # Return empty string (not `NULL` to avoid warnings) if there is no data for this 
      # site_id + model_id combo (not all combos were successful runs)
      if(nrow(task_info) == 0) return("") 
      psprintf("calculate_annual_metrics_per_lake(", 
               "out_ind = target_name,",
               "site_id = I('%s')," = task_name,
               "site_file = '%s'," = task_info$wtr_filename,
               "ice_file = %s," = ifelse(is.na(task_info$ice_filename), 'I(NULL)', sprintf("'%s'", task_info$ice_filename)),
               "temp_ranges_file = '%s'," = temp_ranges_file,
               "morphometry_ind = '%s'," = steps[["split_morphometry"]]$target_name,
               # Doesn't actually print to console with `loop_tasks` but let's you see if you are troubleshooting individual files
               "verbose = I(TRUE))" 
      )
    }
  )})
  
  # If there is more than one model type, we should combine them per task
  # so that each task has the same "final" step that we can use for `final_steps`
  if(length(calc_annual_metrics) > 1) {
    combine_model_thermal_metrics <- scipiper::create_task_step(
      step_name = 'combine_model_thermal_metrics',
      target_name = function(task_name, step_name, ...){
        sprintf("3_summarize/tmp%s/%s_annual_thermal_metrics.rds.ind", tmpdir_suffix, task_name)
      },
      command = function(..., task_name, steps){
        calc_annual_metrics_steps <- steps[grepl('calc_annual_metrics_', names(steps))]
        calc_annual_metrics_targets_str <- paste(sprintf("'%s'", sapply(calc_annual_metrics_steps, `[[`, "target_name")), collapse = ",")
        psprintf("combine_model_thermal_metrics(", 
                 "out_ind = target_name,",
                 "model_id_colname = I('%s')," = model_id_colname,
                 "%s)" = calc_annual_metrics_targets_str
        )
      } 
    )
    
    task_steps_list <- c(list(split_morphometry), calc_annual_metrics, list(combine_model_thermal_metrics))
    final_step_name <- "combine_model_thermal_metrics"
  } else {
    task_steps_list <- list(split_morphometry, calc_annual_metrics)
    final_step_name <- "calc_annual_metrics"
  }
  
  # Create the task plan
  task_plan_all_combos <- create_task_plan(
    task_names = unique(tasks$site_id),
    task_steps = task_steps_list,
    final_steps = final_step_name,
    add_complete = FALSE)
  
  # Remove any steps with empty commands (there wasn't a matching site_id + model_id combo)
  task_plan <- lapply(task_plan_all_combos, function(task_def) {
    missing_command <- sapply(task_def$steps, function(step_def) nchar(step_def$command) == 0)
    missing_command_target_names <- sapply(task_def$steps[missing_command], `[[`, "target_name")
    
    # Remove the steps with missing commands
    task_def$steps[missing_command] <- NULL
    
    # Also remove in the final step for combining if necessary by removing the filepaths to 
    # target names that don't exist
    task_final_step <- task_def$steps[[final_step_name]]
    if(!is.null(task_final_step)) {
      command_drop_target_names <- gsub(paste(missing_command_target_names, collapse="|"), "", task_final_step$command)
      command_drop_target_names <- gsub(",''|'',", "", command_drop_target_names) # Remove leftover string syntax
      task_def$steps[[final_step_name]]$command <- command_drop_target_names
    }
    
    return(task_def)
  })
  
  # Need to make sure `task_plan` attributes make it back
  attributes(task_plan) <- attributes(task_plan_all_combos)
  
  
  # Create the task remakefile
  task_makefile <- sprintf('3_summarize_%s_metric_tasks.yml', paste(model_type, collapse = ".")) # Collapse string if doing tasks for more than one model type
  create_task_makefile(
    task_plan = task_plan,
    makefile = task_makefile,
    sources = c(...),
    packages = c('tidyverse', 'purrr', 'readr', 'scipiper', 'arrow', 'data.table'),
    final_targets = final_target,
    finalize_funs = 'combine_thermal_metrics',
    as_promises = TRUE,
    tickquote_combinee_objects = TRUE)
  
  # Build the tasks
  loop_tasks(task_plan = task_plan,
             task_makefile = task_makefile,
             task_names = unique(tasks$site_id),
             num_tries = 1,
             n_cores = n_cores)
  
  # Clean up files created
  
  # Remove the temporary target from remake's DB; it won't necessarily be a unique  
  #   name and we don't need it to persist, especially since killing the task yaml
  scdel(sprintf("%s_promise", basename(final_target)), remake_file=task_makefile)
  # Delete task makefile since it is only needed internally for this function and  
  #   not needed at all once loop_tasks is complete. Also delete temporary files
  #   saved in order to decouple task makefile from `remake.yml`.
  file.remove(task_makefile, morph_file, temp_ranges_file)
  
}

combine_thermal_metrics <- function(target_name, ...) {
  purrr::map(list(...), function(ind) readRDS(as_data_file(ind))) %>% purrr::reduce(bind_rows) %>% readr::write_csv(target_name)
}

combine_model_thermal_metrics <- function(out_ind, model_id_colname, ...) {
  data_file <- as_data_file(out_ind)
  purrr::map(list(...), function(ind) {
    rds_file <- as_data_file(ind)
    file_pattern <- "(?<modelid>.*)_(?<sitenum>nhdhr_.*)_annual_thermal_metrics.rds" # based on target_name of `calc_annual_metrics` step
    readRDS(rds_file) %>% 
      mutate(model_id = str_match(basename(rds_file), file_pattern)[2]) %>% 
      select(site_id, !!model_id_colname := model_id, everything())
  }) %>% 
    purrr::reduce(bind_rows) %>% 
    saveRDS(data_file)
  sc_indicate(ind_file = out_ind, data_file = data_file)
}

split_and_save_morphometry <- function(out_ind, morphometry_file, site_id) {
  data_file <- as_data_file(out_ind)
  morphometry <- readRDS(morphometry_file)
  saveRDS(morphometry[[site_id]], data_file)
  sc_indicate(ind_file = out_ind, data_file = data_file)
}

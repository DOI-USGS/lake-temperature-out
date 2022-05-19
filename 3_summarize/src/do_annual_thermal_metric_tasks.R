
do_annual_metrics_multi_lake <- function(final_target, site_file_yml, ice_file_yml, n_cores, morphometry, temp_ranges, ...,
                                         site_file_regex = NULL, ice_file_regex = NULL, tmpdir_suffix = "", 
                                         model_id_colname = NULL, suffix_as_model_id = TRUE, max_group_size = NULL) {
  
  # Each node on a Yeti normal partition has a max of 20 cores; nodes on Yeti UV partition do not have that same limit
  if(n_cores > 20) message("If using a node on the Yeti normal partition, you need to decrease n_cores to 20 or less")
  
  site_files <- get_filenames_from_ind(site_file_yml)
  
  # Define task table rows - one task per lake + driver
  task_files <- tibble(wtr_filename = site_files) %>% 
    extract(wtr_filename, c('prefix','site_id','suffix'), site_file_regex, remove = FALSE)
  
  if(!is.null(ice_file_yml)) {
    ice_files <- get_filenames_from_ind(ice_file_yml) 
  } else {
    ice_files <- ""
  }
  
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
  temp_ranges_file <- sprintf("temp_ranges.rds")
  saveRDS(temp_ranges, temp_ranges_file)
  
  # Nest a makefile within this step to generate the separated morph files using `do_split_morph()`
  # Doing this so that these are only done once per lake rather than once per task (lake + driver)
  morph_file_ind <- sprintf('3_summarize/tmp%s/split_morph_files.ind', tmpdir_suffix)
  do_split_morph_tasks(
    final_target = morph_file_ind,
    site_ids = unique(task_files$site_id),
    tmpdir_suffix = tmpdir_suffix,
    morphometry = morphometry,
    n_cores = 1,
    '3_summarize/src/do_split_morph_tasks.R')
  morph_files <- get_filenames_from_ind(morph_file_ind)
  morph_file_regex <- sprintf("3_summarize/tmp%s/(.*)_morphometry.rds.ind", tmpdir_suffix)
  
  # Add additional config information for the tasks
  tasks <- task_files %>%
    left_join(extract(tibble(ice_filename = ice_files), ice_filename, c('site_id'), ice_file_regex, remove = FALSE), by = "site_id") %>% 
    left_join(extract(tibble(morph_filename = morph_files), morph_filename, c('site_id'), morph_file_regex, remove = FALSE), by = "site_id") %>% 
    select(site_id, wtr_filename, ice_filename, morph_filename, model_id = matches(ifelse(suffix_as_model_id, "suffix", "prefix"))) %>% 
    mutate(task_id = sprintf("%s_%s", model_id, site_id))
  
  # Define task table columns
  
  calc_annual_metrics <- create_task_step(
    step_name = 'calc_annual_metrics',
    target_name = function(task_name, step_name, ...) {
      sprintf("3_summarize/tmp%s/%s_annual_thermal_metrics.rds.ind", tmpdir_suffix, task_name)
    },
    command = function(..., task_name, steps) {
      task_info <- filter(tasks, task_id == task_name)
      psprintf("calculate_annual_metrics_per_lake(",
               "out_ind = target_name,",
               "site_id = I('%s')," = task_info$site_id,
               "site_file = '%s'," = task_info$wtr_filename,
               "ice_file = %s," = ifelse(is.na(task_info$ice_filename), 'I(NULL)', sprintf("'%s'", task_info$ice_filename)),
               "temp_ranges_file = '%s'," = temp_ranges_file,
               "morphometry_ind = '%s'," = task_info$morph_filename,
               "model_id = %s," = ifelse(is.null(task_info$model_id), 'I(NULL)', sprintf("I('%s')", task_info$model_id)),
               "model_id_colname = %s," = ifelse(is.null(model_id_colname), 'I(NULL)', sprintf("I('%s')", model_id_colname)),
               # Doesn't actually print to console with `loop_tasks` but let's you see if you are troubleshooting individual files
               "verbose = I(TRUE))"
      )
    }
  )
  
  # To minimize the number of files being checked for each build, we are going to create separate 
  # task makefiles by grouping the tasks into manageable chunks. Note that changing the max group
  # size doesn't cause the individual targets for each lake-driver combo to rebuild since they are 
  # tracked as being complete by their build/ & ind files, not which group makefile they are in. 
  # If no max_group_size is passed in, assume all tasks are in one makefile.
  if(is.null(max_group_size)) max_group_size <- length(unique(tasks$task_id))
  task_groups <- select(tasks, task_id) %>% 
    unique() %>% 
    mutate(row_n = row_number()) %>% 
    mutate(task_grp = ((row_n - 1) %/% max_group_size)+1)
  
  source_vec <- c(...) # Need to combine before `purrr::map()`
  grp_inds <- task_groups %>% 
    split(.$task_grp) %>% 
    purrr::map(~execute_single_group_task_plan(
      final_target_subset = sprintf("3_summarize/out%s/multidriver_annual_metrics_grp_%s.ind", 
                                    tmpdir_suffix, unique(.x$task_grp)),
      task_names_subset = .x$task_id,
      task_steps_list = list(calc_annual_metrics),
      final_steps = "calc_annual_metrics",
      sources = source_vec,
      n_cores
    ))
  
  combine_group_inds_to_csv(final_target, grp_inds)
  
  # Delete temporary files saved in order to decouple task makefile from `remake.yml`.
  file.remove(temp_ranges_file)
  
}

execute_single_group_task_plan <- function(final_target_subset, task_names_subset, 
                                           task_steps_list, final_steps, sources, n_cores) {
  
  # Create the task plan
  task_plan <- create_task_plan(
    task_names = task_names_subset,
    task_steps = task_steps_list,
    final_steps = final_steps,
    add_complete = FALSE)
  
  # Create the task remakefile
  task_makefile <- '3_summarize_multidriver_metric_tasks.yml'
  create_task_makefile(
    task_plan = task_plan,
    makefile = task_makefile,
    sources = sources,
    packages = c('tidyverse', 'purrr', 'readr', 'scipiper', 'arrow', 'data.table'),
    final_targets = final_target_subset,
    finalize_funs = 'combine_to_ind_override',
    as_promises = TRUE,
    tickquote_combinee_objects = TRUE)
  
  # Build all the tasks in the current group makefile
  loop_tasks(task_plan = task_plan,
             task_makefile = task_makefile,
             # Don't specify `task_names` so that the final combine target is built
             num_tries = 1,
             n_cores = n_cores)
  
  # Clean up files created
  
  # Remove the temporary target from remake's DB; it won't necessarily be a unique  
  #   name and we don't need it to persist, especially since killing the task yaml
  scdel(sprintf("%s_promise", basename(final_target_subset)), remake_file=task_makefile)
  # Delete task makefile since it is only needed internally for this function and  
  #   not needed at all once loop_tasks is complete. 
  file.remove(task_makefile)
  
  return(final_target_subset)
  
}

# Combine all inds listing thermal metrics RDS inds
# into a single vector of the data file inds
flatten_inds <- function(nested_ind_list) {
  purrr::map(nested_ind_list, function(ind_list) {
    yaml::yaml.load_file(ind_list) %>% names()
  }) %>% purrr::reduce(c)
}

# I wanted the same indicator with filename: hash, even if there
# is only one file output. `combine_to_ind()` calls `sc_indicate()`
# which only returns the hash if there is one file.This uses some
# of the code from within sc_indicate() to do what we need here.
combine_to_ind_override <- function(ind_file, ...) {
  data_file <- c(...)
  info_list <- list()
  for (file in data_file) {
    info_list[[file]] <- unname(tools::md5sum(file))
  }
  if (!dir.exists(dirname(ind_file))) 
    dir.create(dirname(ind_file), recursive = TRUE)
  readr::write_lines(yaml::as.yaml(info_list), ind_file)
  return(invisible(NULL))
}

# This is called by both `combine_group_inds_to_csv()` and reads in
# the data files, then combines into a single table 
combine_model_thermal_metrics <- function(ind_list) {
  purrr::map(ind_list, function(ind) {
    readRDS(as_data_file(ind))
  }) %>% 
    purrr::reduce(bind_rows)
}

# Used to combine the inds from each group iteration of the task 
# makefiles into one big CSV. Each `ind` inside of `ind_list` contains
# a list of the individual RDS file inds that must be flattened first.
combine_group_inds_to_csv <- function(target_name, ind_list) {
  flatten_inds(ind_list) %>% 
  combine_model_thermal_metrics() %>% 
    readr::write_csv(target_name)
}


do_obs_lake_tasks <- function(target_name, task_df_fn, irr_df_fn, k0_df_fn, ...){
  
  tasks <- tibble(task_filepath = names(yaml::yaml.load_file(task_df_fn))) %>% 
    mutate(filename = basename(task_filepath)) %>% 
    extract(filename, 'site_id', "split_obs_data_(.*).feather", remove = FALSE) %>% 
    select(task_filepath, site_id) %>% 
    filter(nchar(task_filepath) != 0) 
  
  irr_files <- tibble(filepath = names(yaml::yaml.load_file(irr_df_fn))) %>% 
    extract(filepath, into = 'site_id', regex = "pb0_(.*)_irradiance.csv", remove = FALSE) %>% 
    select(site_id, irr_filepath = filepath)
  clarity_files <- tibble(filepath = names(yaml::yaml.load_file(k0_df_fn))) %>% 
    extract(filepath, into = 'site_id', regex = "gam_(.*)_clarity.csv", remove = FALSE) %>% 
    select(site_id, clarity_filepath = filepath)
  
  # Merge clarity and irr file names to use
  tasks <- tasks %>% left_join(irr_files, by = 'site_id') %>% left_join(clarity_files, by = 'site_id')
  
  # ---- create task steps ---- #
  
  #' I'm doing this because each toha task is slow running
  #' and if `morphometry` changes, we don't want to have to re-run all of them,
  #' just the ones where _their_ morphometry changed. So we are subsetting first. 
  split_morphometry <- scipiper::create_task_step(
    step_name = 'split_morphometry',
    target_name = function(task_name, ...){
      sprintf("%s_morphometry", task_name)
    },
    command = function(task_name, ...){
      task_filepath <- dplyr::filter(tasks, site_id == task_name) %>% 
        pull(task_filepath)
      sprintf("morphometry[[I('%s')]]", task_name)
    } 
  )
  
  apply_data_criteria <- scipiper::create_task_step(
    step_name = 'apply_data_criteria',
    target_name = function(task_name, ...){
      sprintf("filtered_obs_data_%s", task_name)
    },
    command = function(task_name, ...){
      task_filepath <- dplyr::filter(tasks, site_id == task_name) %>% pull(task_filepath)
      sprintf("filter_observed_data('%s')", task_filepath)
    } 
  )
  
  reformat_obs_data <- scipiper::create_task_step(
    step_name = 'reformat_obs_data',
    target_name = function(task_name, ...){
      sprintf("formatted_obs_data_%s", task_name)
    },
    command = function(steps, ...){
      sprintf("reformat_observed_data(%s)", steps[["apply_data_criteria"]]$target_name)
    } 
  )
  
  join_temp_kd_i0 <- scipiper::create_task_step(
    step_name = 'join_temp_kd_i0',
    target_name = function(task_name, step_name, ...){
      sprintf("2_process/tmp/munged_obs_data_%s.feather", task_name)
    },
    command = function(task_name, steps, ...){
      irr_filepath <- dplyr::filter(tasks, site_id == task_name) %>% pull(irr_filepath)
      clarity_filepath <- dplyr::filter(tasks, site_id == task_name) %>% pull(clarity_filepath)
      psprintf("join_observed_data(",
               "target_name = target_name,",
               "obs_data = %s," = steps[["reformat_obs_data"]]$target_name, 
               "irr_data_fn = '%s'," = irr_filepath, 
               "k0_data_fn = '%s')" = clarity_filepath)
    } 
  )
  
  calculate_obs_toha <- scipiper::create_task_step(
    step_name = 'calculate_obs_toha',
    target_name = function(task_name, ...){
      sprintf("2_process/tmp/obs_toha_%s.csv", task_name)
    },
    command = function(task_name, steps, ...){
      psprintf("calculate_toha_per_lake(", 
               "target_name = target_name,",
               "site_data_fn = '%s'," = steps[["join_temp_kd_i0"]]$target_name,
               "morphometry = `%s_morphometry`)" = task_name
      )
    } 
  )
  
  # ---- combine into a task plan ---- #
  
  task_plan <- create_task_plan(
    task_names = tasks$site_id,
    task_steps = list(
      split_morphometry, apply_data_criteria, reformat_obs_data,
      join_temp_kd_i0, calculate_obs_toha),
    final_steps = 'calculate_obs_toha',
    add_complete = FALSE)
  
  # ---- combine into a task makefile ---- #
  
  create_task_makefile(
    task_plan = task_plan,
    makefile = '2_obs_lake_tasks.yml',
    include = 'remake.yml',
    sources = c(...),
    packages = c("purrr", "dplyr", "mda.lakes", "feather", "rLakeAnalyzer", "readr"),
    final_targets = c(target_name),
    finalize_funs = c('combine_obs_toha'),
    as_promises = TRUE,
    tickquote_combinee_objects = TRUE)
  
  # ---- build the task makefile ---- #
  
  loop_tasks(
    task_plan = task_plan,
    task_makefile = '2_obs_lake_tasks.yml',
    num_tries = 1)
  
}

combine_obs_toha <- function(target_name, ...) {
  # Read feather files and combine into single CSV
  purrr::map(list(...), read_csv, col_types = cols()) %>% 
    purrr::reduce(bind_rows) %>% 
    write_csv(target_name)
}

do_lakegroup_tasks <- function(final_target, task_ids, irradiance_zips, clarity_zips, predictions_zips, ...) {
  
  ##-- Define task table rows --##
  
  # Prepare a data.frame with one row per task 
  
  group_info <- tibble(group_id = task_ids) %>% mutate(group_n = substr(group_id, 1,2))
  
  parse_into <- c('prefix', 'group_n')
  regx <- '(irradiance|clarity|pb0_predictions|pgdl_predictions)_([0-9]+)'
  irr_files <- tibble(filepath = yaml::yaml.load_file(irradiance_zips) %>% names) %>% 
    extract(filepath, into = parse_into, regex = regx, remove = FALSE) %>% 
    select(-prefix, irr_filepath = filepath)
  clarity_files <- tibble(filepath = yaml::yaml.load_file(clarity_zips) %>% names) %>% 
    extract(filepath, into = parse_into, regex = regx, remove = FALSE) %>% 
    select(-prefix, clarity_filepath = filepath)
  pred_files <- tibble(filepath = yaml::yaml.load_file(predictions_zips) %>% names) %>% 
    extract(filepath, into = parse_into, regex = regx, remove = FALSE) %>% 
    select(pred_filepath = filepath, everything())
  
  model_prefix <- pull(pred_files, prefix) %>% unique() %>% stringr::str_remove('_predictions')
  stopifnot(length(model_prefix) == 1)
  
  tasks <- inner_join(irr_files, clarity_files, by = 'group_n') %>% 
    inner_join(pred_files, by = 'group_n') %>% 
    inner_join(group_info, by = 'group_n') %>% 
    mutate(task_id = sprintf("group_%s", group_n)) %>% select(-prefix)
  
  ##-- Define task table columns --##
  
  get_lake_ids <- scipiper::create_task_step(
    step_name = 'get_lake_ids',
    target_name = function(task_name, step_name, ...){
      sprintf("%s_lake_ids", task_name)
    },
    command = function(task_name, ...){
      group_id <- dplyr::filter(tasks, task_id == task_name) %>% pull(group_id)
      psprintf("get_lakes_from_group(", 
               "sb_groups = sb_group_info,",
               "grp_id = I('%s'))" = group_id
      )
    } 
  )
  combine_model_zipped_files <- scipiper::create_task_step(
    step_name = 'combine_model_zipped_files',
    target_name = function(task_name, step_name, ...){
      sprintf("merged_model_data_%s", task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, task_id == task_name)
      psprintf("unzip_and_merge_files(", 
               "lake_ids = %s_lake_ids," = task_name,
               "irradiance_zipfile = '%s'," = cur_task$irr_filepath,
               "clarity_zipfile = '%s'," = cur_task$clarity_filepath,
               "temp_zipfile = '%s'," = cur_task$pred_filepath,
               "fn_out_template = I('2_process/tmp/merged_%s_data_%s.feather'))" = c(model_prefix, "%s") # this is a template to be used when saving data
      )
    } 
  )
  
  ##-- Create the task plan --##
  
  task_plan <- create_task_plan(
    task_names=tasks$task_id,
    task_steps=list(
      get_lake_ids,
      combine_model_zipped_files),
    add_complete=FALSE,
    final_steps=c('combine_model_zipped_files'))
  
  ##-- Create the task remakefile --##
  
  task_makefile <- sprintf('2_%s_grp_tasks.yml', model_prefix)
  create_task_makefile( 
    task_plan=task_plan,
    makefile=task_makefile,
    include='remake.yml',
    sources=c(...),
    packages=c("purrr", "dplyr", "readr", "feather"),
    final_targets = final_target,
    finalize_funs = "indicate_file_dataframes",
    as_promises = TRUE,
    tickquote_combinee_objects = TRUE
  )
  
  ##-- Build the tasks --##
  
  loop_tasks(task_plan = task_plan,
             task_makefile = task_makefile,
             num_tries = 3)
  
  # Now return the file name of the final target
  return(final_target)
  
}

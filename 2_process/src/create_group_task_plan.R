create_group_task_makefile <- function(makefile, task_plan, remake_file, final_targets) {
  scipiper::create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    include=remake_file,
    packages=c("purrr", "dplyr", "readr", "feather"),
    sources=c("2_process/src/munge_group_files.R"),
    final_targets = final_targets
  )
}

create_group_tasks <- function(task_ids, log_folder, irradiance_zips, clarity_zips, predictions_zips){
  
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
    select(-prefix, pred_filepath = filepath)
  
  tasks <- inner_join(irr_files, clarity_files, by = 'group_n') %>% 
    inner_join(pred_files, by = 'group_n') %>% 
    inner_join(group_info, by = 'group_n') %>% 
    mutate(task_id = sprintf("group_%s", group_n)) %>% 
    filter(group_n %in% c('01','02')) # REMOVE THIS!!

  # # prepare a data.frame with one row per task
  # tasks <- data_frame(task_id=task_ids[1:2]) %>%
  #   mutate(task_name = sprintf("group_%s", substr(task_id, 1,2)))
  
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
  combine_pb0_zipped_files <- scipiper::create_task_step(
    step_name = 'combine_pb0_zipped_files',
    target_name = function(task_name, step_name, ...){
      sprintf("merged_pb0_data_%s", task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(tasks, task_id == task_name)
      psprintf("unzip_and_merge_files(", 
               "lake_ids = %s_lake_ids," = task_name,
               "irradiance_zipfile = '%s'," = cur_task$irr_filepath,
               "clarity_zipfile = '%s'," = cur_task$clarity_filepath,
               "temp_zipfile = '%s'," = cur_task$pred_filepath,
               "fn_out_template = I('2_process/tmp/merged_pb0_data_%s.feather'))" = "%s" # this is a template to be used when saving data
      )
    } 
  )
  
  # ---- combine into a task plan ---- #
  
  scipiper::create_task_plan(
    task_names=tasks$task_id,
    task_steps=list(
      get_lake_ids,
      combine_pb0_zipped_files),
    add_complete=FALSE,
    final_steps=c('combine_pb0_zipped_files'),
    ind_dir='2_process/log')
}

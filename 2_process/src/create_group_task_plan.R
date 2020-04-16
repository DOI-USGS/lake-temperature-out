create_group_task_makefile <- function(makefile, task_plan, remake_file, final_targets) {
  scipiper::create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    include=remake_file,
    packages=c("purrr", "dplyr", "readr", "feather"),
    sources=c("2_process/src/munge_group_files.R"),
    finalize_funs = "indicate_file_vectors",
    final_targets = final_targets
  )
}

create_group_tasks <- function(task_ids, log_folder){
  
  # prepare a data.frame with one row per task
  tasks <- data_frame(task_id=task_ids[1:2]) %>%
    mutate(task_name = sprintf("group_%s", substr(task_id, 1,2)))
  
  get_lake_ids <- scipiper::create_task_step(
    step_name = 'get_lake_ids',
    target_name = function(task_name, step_name, ...){
      sprintf("%s_lake_ids", task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf("get_lakes_from_group(", 
               "sb_groups = sb_group_info,",
               "grp_id = I('%s'))" = cur_task$task_id
      )
    } 
  )
  
  combine_pb0_zipped_files <- scipiper::create_task_step(
    step_name = 'combine_pb0_zipped_files',
    target_name = function(task_name, step_name, ...){
      sprintf("merged_pb0_data_%s", task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf("unzip_and_merge_files(", 
               "lake_ids = %s_lake_ids," = task_name,
               "irradiance_zipfile = '1_fetch/out/irradiance_%s.zip'," = cur_task$task_id,
               "clarity_zipfile = '1_fetch/out/clarity_%s.zip'," = cur_task$task_id,
               "temp_zipfile = '1_fetch/out/pb0_predictions_%s.zip'," = cur_task$task_id,
               "fn_out_template = I('2_process/tmp/merged_pb0_data_%s.feather'))" = "%s" # this is a template to be used when saving data
      )
    } 
  )
  
  # ---- combine into a task plan ---- #
  
  gif_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=list(
      get_lake_ids,
      combine_pb0_zipped_files),
    add_complete=FALSE,
    final_steps=c('combine_pb0_zipped_files'),
    ind_dir='2_process/log')
}

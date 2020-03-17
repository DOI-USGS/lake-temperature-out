create_task_makefile <- function(makefile, task_plan, remake_file) {
  scipiper::create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    include=remake_file,
    packages=c("purrr", "dplyr", "readr"),
    sources=c("2_process/src/combine_sb_files.R"),
    file_extensions=c()
  )
}

create_group_tasks <- function(task_ids, log_folder){
  
  # prepare a data.frame with one row per task
  tasks <- data_frame(task_id=sprintf("%02d", task_ids)) %>%
    mutate(task_name = sprintf("grp_%s", task_id))
  
  combine_irradiance <- scipiper::create_task_step(
    step_name = 'combine_irradiance',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("2_process/out/combined_irradiance_%s.rds", task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf("unzip_and_combine_files(", 
               "target_name = target_name,",
               "keyword = I('irradiance'),",
               "all_zipfiles_file = '1_fetch/out/toha_irradiance_downloaded.rds',",
               "grp = I('%s'))" = cur_task$task_id
      )
    } 
  )
  
  combine_temp_pred <- scipiper::create_task_step(
    step_name = 'combine_temp_pred',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("2_process/out/combined_temp_pred_%s.rds", task_name)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf("unzip_and_combine_files(", 
               "target_name = target_name,",
               "keyword = I('predictions'),",
               "all_zipfiles_file = '1_fetch/out/toha_temp_pred_downloaded.rds',",
               "grp = I('%s'))" = cur_task$task_id
      )
    } 
  )
  
  # ---- combine into a task plan ---- #
  
  gif_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=list(
      combine_irradiance,
      combine_temp_pred),
    add_complete=FALSE,
    final_steps=c('combine_irradiance', 'combine_temp_pred'),
    ind_dir='2_process/log')
}

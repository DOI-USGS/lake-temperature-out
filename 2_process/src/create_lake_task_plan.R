create_lake_task_makefile <- function(makefile, task_plan, remake_file, final_targets) {
  scipiper::create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    include=remake_file,
    packages=c("purrr", "dplyr", "mda.lakes", "feather", "rLakeAnalyzer"),
    sources=c("2_process/src/calculate_toha.R")
  )
}

create_lake_tasks <- function(task_df_fn, log_folder){
  
  # prepare a data.frame with one row per task
  tasks <- readRDS(task_df_fn)[1:3,] %>% 
    rename(task_id = nhdhr, task_filename = filename) %>% 
    mutate(task_name = task_id) %>% 
    # Remove the lakes that didn't merge data
    filter(nchar(task_filename) != 0) 
    
  
  calculate_toha <- scipiper::create_task_step(
    step_name = 'calculate_toha',
    target_name = function(task_name, step_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      sprintf("2_process/out/toha_%s.csv", cur_task$task_id)
    },
    command = function(task_name, ...){
      cur_task <- dplyr::filter(rename(tasks, tn=task_name), tn==task_name)
      psprintf("calculate_toha_per_lake(", 
               "target_name = target_name,",
               "nhdhr = I('%s')," = cur_task$task_id,
               "nhdhr_data_fn = '%s'," = cur_task$task_filename,
               "morphometry = morphometry)"
      )
    } 
  )
  
  # ---- combine into a task plan ---- #
  
  gif_task_plan <- scipiper::create_task_plan(
    task_names=tasks$task_name,
    task_steps=list(
      calculate_toha),
    add_complete=FALSE,
    final_steps=c('calculate_toha'),
    ind_dir='2_process/log')
}

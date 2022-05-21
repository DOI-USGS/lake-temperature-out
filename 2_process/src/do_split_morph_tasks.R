# Split morphometry into separate files per lake to avoid too many processes trying to access the same file
# Doing this so that these are only done once per lake rather than once per task (lake + driver)

do_split_morph_tasks <- function(final_target, dir, n_cores, morphometry, ...) {
  
  # Use site numbers as tasks
  tasks <- names(morphometry)
  
  # Having `remake.yml` listed in "include" for these large task plans
  # is expensive. In order to decouple the task makefile from `remake.yml`
  # to simplify the dependencies and complexity, we need to save some of the 
  # target objects used as files. Each task will read these new files but
  # we don't think that will be an issue, even when doing it in parallel.
  morph_file <- sprintf("tasks_all_morphometry.rds")
  saveRDS(morphometry, morph_file)
  
  # Define task table columns
  
  # I'm doing this because each task is slow running
  # and if `morphometry` changes, we don't want to have to re-run all of them,
  # just the ones where _their_ morphometry changed. So we are subsetting first. 
  split_morphometry <- scipiper::create_task_step(
    step_name = 'split_morphometry',
    target_name = function(task_name, step_name, ...){
      sprintf("%s/%s_morphometry.rds.ind", dir, task_name)
    },
    command = function(task_name, ...){
      sprintf("split_and_save_morphometry(target_name, '%s', I('%s'))", morph_file, task_name)
    } 
  )
  
  # Create the task plan
  task_plan <- create_task_plan(
    task_names = tasks,
    task_steps = list(split_morphometry),
    final_steps = "split_morphometry",
    add_complete = FALSE)
  
  # Create the task remakefile
  task_makefile <- '3_summarize_split_morph_tasks.yml'
  create_task_makefile(
    task_plan = task_plan,
    makefile = task_makefile,
    sources = c(...),
    packages = c('scipiper'),
    final_targets = final_target,
    finalize_funs = 'combine_to_ind_override',
    as_promises = TRUE,
    tickquote_combinee_objects = TRUE)
  
  # Build the tasks
  loop_tasks(task_plan = task_plan,
             task_makefile = task_makefile,
             num_tries = 1,
             n_cores = n_cores)
  
  # Clean up files created

  # Remove the temporary target from remake's DB; it won't necessarily be a unique
  #   name and we don't need it to persist, especially since killing the task yaml
  # scdel(sprintf("%s_promise", basename(final_target)), remake_file=task_makefile)
  # Delete task makefile since it is only needed internally for this function and
  #   not needed at all once loop_tasks is complete. Also delete temporary files
  #   saved in order to decouple task makefile from `remake.yml`.
  file.remove(task_makefile, morph_file)

}

split_and_save_morphometry <- function(out_ind, morphometry_file, site_id) {
  data_file <- as_data_file(out_ind)
  morphometry <- readRDS(morphometry_file)
  saveRDS(morphometry[[site_id]], data_file)
  sc_indicate(ind_file = out_ind, data_file = data_file)
}

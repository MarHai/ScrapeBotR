context('runs')

test_that('is it possible to fetch runs, run logs, and run data', {
  connection <- connect('haim.it')

  expect_error(get_runs(NULL),
               'Connection needs to be a valid connection object')
  expect_error(get_runs(connection, NULL, NULL),
               'Either instance_uid or recipe_uid')

  runs <- get_runs(connection, recipe_uid = 4)
  expect_s3_class(runs, 'tbl_df')
  expect_equal(colnames(runs),
               c('uid', 'recipe_uid', 'instance_uid', 'started', 'runtime', 'status'))
  expect_gt(nrow(runs), 0)

  run_uid <-
    runs %>%
    dplyr::slice_sample(n = 1) %>%
    dplyr::pull(uid)

  run_log <- get_run_log(connection, run_uid)
  expect_s3_class(run_log, 'tbl_df')
  expect_equal(colnames(run_log),
               c('run_uid', 'created', 'type', 'message'))
  expect_gt(nrow(run_log), 0)

  run_data <- get_run_data(connection, run_uid)
  expect_s3_class(run_data, 'tbl_df')
  expect_equal(colnames(run_data),
               c('created', 'run_uid', 'instance_uid', 'recipe_uid', 'step_uid', 'value'))
  expect_gte(nrow(run_data), 0)

  disconnect(connection)
})

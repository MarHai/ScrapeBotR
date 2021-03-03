context('recipes and instances')

test_that('is it possible to fetch instances', {
  connection <- connect_scrapebot('haim.it')
  expect_error(get_instances(NULL),
               'Connection needs to be a valid connection object')
  instances <- get_instances(connection)
  expect_s3_class(instances, 'tbl_df')
  expect_equal(colnames(instances),
               c('uid', 'name', 'created', 'description', 'runs_count', 'runs_latest'))
  expect_gt(nrow(instances), 0)
  disconnect(connection)
})

test_that('can you get recipes and recipe steps', {
  connection <- connect_scrapebot('haim.it')

  expect_error(get_recipes(NULL),
               'Connection needs to be a valid connection object')

  recipes <- get_recipes(connection, include_inactive = TRUE)
  expect_s3_class(recipes, 'tbl_df')
  expect_equal(colnames(recipes),
               c('uid', 'name', 'created', 'description', 'active', 'cookies', 'interval', 'runs_count', 'runs_latest'))
  expect_gt(nrow(recipes), 0)

  recipe_uid <-
    recipes %>%
    dplyr::slice_sample(n = 1) %>%
    dplyr::pull(uid)
  steps <- get_recipe_steps(connection, recipe_uid, include_inactive = TRUE)
  expect_s3_class(steps, 'tbl_df')
  expect_equal(colnames(steps),
               c('recipe_uid', 'uid', 'type', 'value', 'use_random_item', 'use_data_item', 'active', 'random_items'))
  expect_gt(nrow(steps), 0)

  disconnect(connection)
})

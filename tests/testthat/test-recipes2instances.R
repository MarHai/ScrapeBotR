context('recipes and instances')

test_that('user can be created', {
  connection <- connect_scrapebot('haim.it')
  user.email <- 'mario@haim.it'
  user.uid <- get_or_create_user(connection,
                                 user.email)
  expect_type(user.uid, 'integer')
  expect_silent(user.uid2 <- get_or_create_user(connection,
                                                user.email))
  expect_type(user.uid2, 'integer')
  expect_gt(user.uid2, 0)
  expect_equal(user.uid,
               user.uid2)
})

test_that('creation of recipes, instances, and recipe steps', {
  connection <- connect_scrapebot('haim.it')

  instances.before <- get_instances(connection)
  instance.name <- paste(sample(letters, 15),
                         collapse = '')
  instance.uid <- add_instance(connection,
                               instance.name,
                               'mario@haim.it',
                               'test instance')
  expect_type(instance.uid, 'integer')
  expect_gt(instance.uid, 0)
  instances.after <- get_instances(connection)
  expect_equal(nrow(instances.before),
               nrow(instances.after) - 1)

  recipes.before <- get_recipes(connection)
  recipe.name <- paste(sample(letters, 15),
                         collapse = '')
  recipe.uid <- add_recipe(connection,
                           recipe.name,
                           'mario@haim.it',
                           'test recipe',
                           FALSE)
  expect_type(recipe.uid, 'integer')
  expect_gt(recipe.uid, 0)

  recipes.after <- get_recipes(connection)
  expect_equal(nrow(recipes.before),
               nrow(recipes.after))

  expect_true(activate_recipe(connection,
                              recipe.uid))

  recipes.after <- get_recipes(connection)
  expect_equal(nrow(recipes.before),
               nrow(recipes.after) - 1)

  expect_true(deactivate_recipe(connection,
                                recipe.uid))
  recipes.after <- get_recipes(connection)
  expect_equal(nrow(recipes.before),
               nrow(recipes.after))

  expect_gt(add_recipe_step(connection,
                            recipe.uid,
                            'log',
                            'start off testing'),
            0)
  expect_gt(add_recipe_step(connection,
                            recipe.uid,
                            'log',
                            random_items = 1:5),
            0)

  recipe.steps <- get_recipe_steps(connection,
                                   recipe.uid)
  expect_equal(nrow(recipe.steps),
               2)
  expect_equal(recipe.steps[[1, 'use_random_item']],
               FALSE)
  expect_equal(recipe.steps[[2, 'use_random_item']],
               TRUE)
  expect_equal(length(unlist(recipe.steps[[2, 'random_items']])),
               5)

  expect_true(deactivate_recipe_step(connection,
                                     recipe.uid,
                                     recipe.steps[[1, 'uid']]))
  expect_true(activate_recipe_step(connection,
                                   recipe.uid,
                                   recipe.steps[[1, 'uid']]))

  disconnect(connection)
})


test_that('import/export .sbj', {
  connection <- connect_scrapebot('haim.it')
  recipe.uid <- get_recipes(connection,
                            include_inactive = TRUE)[[1, 'uid']]
  expect_silent(sbj <- export_sbj(connection, recipe.uid))
  expect_type(sbj, 'character')

  recipes.before <- get_recipes(connection)
  recipe.name <- paste(sample(letters, 15),
                       collapse = '')
  recipe.uid <- add_recipe(connection,
                           recipe.name,
                           'mario@haim.it',
                           'test recipe',
                           FALSE)
  expect_type(recipe.uid, 'integer')
  expect_gt(recipe.uid, 0)

  recipes.after <- get_recipes(connection)
  expect_equal(nrow(recipes.before),
               nrow(recipes.after))

  expect_true(activate_recipe(connection,
                              recipe.uid))

  recipes.after <- get_recipes(connection)
  expect_equal(nrow(recipes.before),
               nrow(recipes.after) - 1)

  expect_true(deactivate_recipe(connection,
                                recipe.uid))
  recipes.after <- get_recipes(connection)
  expect_equal(nrow(recipes.before),
               nrow(recipes.after))

  expect_gt(add_recipe_step(connection,
                            recipe.uid,
                            'log',
                            'start off testing'),
            0)
  expect_gt(add_recipe_step(connection,
                            recipe.uid,
                            'log',
                            random_items = 1:5),
            0)

  disconnect(connection)
})

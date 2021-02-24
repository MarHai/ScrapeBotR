context('aws')

test_that('is it possible to create and run AWS database, storage, and instances', {
  expect_silent(aws_connection <- connect_aws())
  expect_silent(aws_connection <- aws_launch_database(aws_connection))
  expect_silent(aws_connection <- aws_launch_storage(aws_connection))

  # Instance launches in tryCatch to, in the worst case, at least terminate database/storage
  tryCatch({

    # first one is not silent due to user creation
    aws_connection <- aws_launch_instance(aws_connection,
                                          'mario@haim.it')

    # second one should be silent unless failed installations yield warnings
    aws_connection <- aws_launch_instance(aws_connection,
                                          'mario@haim.it')

  }, error = function(e) {
    expect_silent(aws_connection <- aws_terminate_storage(aws_connection))
    expect_silent(aws_connection <- aws_terminate_database(aws_connection))
    stop('Instance launches failed. Storage/Database have been terminated but double-check instances.')
  })

  # connect to database and check for instance numbers
  expect_silent(scrapebot_connection <- connect_scrapebot(aws_connection$rds_credential_section))
  expect_equal(nrow(get_instances(scrapebot_connection)),
               2)
  expect_equal(nrow(aws_connection$ec2_instances),
               2)
  disconnect(scrapebot_connection)

  # shut down both instances
  expect_silent(aws_connection <-
                  aws_terminate_instance(aws_connection,
                                         aws_connection$ec2_instances[[1, 'instance_aws_id']]))
  expect_silent(aws_connection <-
                  aws_terminate_instance(aws_connection,
                                         aws_connection$ec2_instances[[1, 'instance_aws_id']]))

  # re-check instance numbers
  expect_equal(nrow(aws_connection$ec2_instances),
               0)

  # terminate the rest
  expect_silent(aws_connection <- aws_terminate_storage(aws_connection))
  expect_silent(aws_connection <- aws_terminate_database(aws_connection))
})



test_that('save/load aws_connection', {
  aws_connection <- connect_aws()
  aws_connection$rds_credential_section <- 'haim.it'
  aws_connection$s3_bucket <- 's3b-a1b2c3d4e5f6g7h8r'
  expect_silent(aws_save_connection(aws_connection))

  expect_silent(aws_loaded_connection <- aws_load_connection())

  expect_equal(aws_connection$rds_credential_section,
               aws_loaded_connection$rds_credential_section)
  expect_equal(aws_connection$s3_bucket,
               aws_loaded_connection$s3_bucket)
  expect_equal(aws_connection$aws_access_key,
               aws_loaded_connection$aws_access_key)

  file.remove('aws_connection.rds')
})

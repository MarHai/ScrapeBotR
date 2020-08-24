context('write_credentials, connect, and disconnect')

test_that('is able to create credentials file', {
  expect_equal(write_credentials('my_host', 'my_user', 'my_password', 'my_database', 1234),
               '~/.scrapebot_database.ini')
  config <- configr::read.config('~/.scrapebot_database.ini')
  expect_type(config, 'list')
  expect_true('my_database on my_host' %in% names(config))
  expect_equal(config[['my_database on my_host']][['host']], 'my_host')
  expect_equal(config[['my_database on my_host']][['user']], 'my_user')
  expect_equal(config[['my_database on my_host']][['password']], 'my_password')
  expect_equal(config[['my_database on my_host']][['database']], 'my_database')
  expect_equal(config[['my_database on my_host']][['port']], '1234')
})

test_that('can communicate with a database', {
  expect_error(connect(NULL),
               'Credential section needs to be a character string.')

  expect_error(connect('my_section'),
               'Database connection could not be established')

  expect_error(connect('my_section', 'my_file'),
               'Database connection could not be established')

  connection <- connect('haim.it')
  expect_type(connection, 'list')
  expect_invisible(disconnect(connection))
})

test_that('connection objects look good', {
  connection <- connect('haim.it')
  expect_s4_class(connection$db, 'MariaDBConnection')
  expect_type(connection$db_type, 'character')
  expect_type(connection$db_version, 'character')
  expect_type(connection$db_timeout, 'integer')
  expect_type(connection$credentials_file, 'character')
  expect_type(connection$credentials_section, 'character')
  expect_type(connection$tables, 'character')
  expect_equal(length(connection$tables), 11)
  disconnect(connection)
})

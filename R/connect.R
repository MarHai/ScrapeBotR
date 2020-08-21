#' Connect to database
#'
#' Connect to a ScrapeBot central database as typically provided during the installation process of a ScrapeBot infrastructure.
#' Uses RMariaDB to connect to the MySQL server.
#'
#' @param host The central database host to connect to as character string.
#' @param user The username to connect to the central database as character string.
#' @param password According password as character string.
#' @param database Represents the database name as character string. Defaults to `scrapebot`.
#'
#' @return A connection object (i.e., a specified list), ready to be passed to other [ScrapeBotR] functions.
#'
#' @examples
#' \dontrun{
#'
#' connect('localhost', 'root', 's3cr3t_password')
#' connect('127.0.0.1', 'root', 's3cr3t_password', 'my_db')
#' }
#'
#' @seealso [disconnect()] to close a database connection, <https://github.com/MarHai/ScrapeBot> for more information on the central database.
#'
#' @export

connect <- function(host, user, password, database = 'scrapebot') {

  # Test input
  if(!is.character(host)) {
    stop('Host needs to be a character string.')
  }
  if(!is.character(user)) {
    stop('Username needs to be a character string.')
  }
  if(!is.character(password)) {
    stop('Password needs to be a character string.')
  }
  if(!is.character(database)) {
    stop('Database needs to be a character string.')
  }

  # Try connecting to the database
  tryCatch({
    maria <- DBI::dbConnect(RMariaDB::MariaDB(),
                            host = host,
                            username = user,
                            password = password,
                            dbname = database)
  }, error = function(e) {
    stop(paste0('Database connection could not be established. ', e))
  })

  # Collection ScrapeBot installation information
  tryCatch({
    timeout <- DBI::dbGetQuery(maria, 'SHOW SESSION VARIABLES LIKE "wait_timeout"', n = 1)[[1, 'Value']]
    db_info <- DBI::dbGetInfo(maria)
  }, error = function(e) {
    stop(paste0('Database did not return as expected when requesting wait_timeout. ', e))
  })

  # Set up connection object (a named list)
  connection <- list(
    db = maria,
    db_type = db_info$con.type,
    db_version = db_info$db.version,
    db_timeout = as.integer(timeout),
    tables = DBI::dbListTables(maria)
  )

  return(connection)
}

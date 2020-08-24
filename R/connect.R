#' Connect to database
#'
#' Connect to a ScrapeBot central database as typically provided during the installation process of a ScrapeBot infrastructure.
#' Uses RMariaDB to connect to the MySQL server.
#'
#' In order not to use database credentials here, you need to set up an INI file containing your database credentials. You can do this by hand (see [write_credentials()]) or use the [write_credentials()] function.
#'
#' @param credentials_section The section within your INI file holding the credentials. If [write_credentials()] was used to create the INI file, then the section is called "\code{database} on \code{host}" (e.g., "scrapebot on localhost").
#' @param credentials_file The INI file to use. If [write_credentials()] was used to create the INI file, then it is called \code{~/.scrapebot_database.ini} (the default). Note that the file must (!) be located inside your home directory (i.e., somewhere inside \code{~} as this is a required by <https://mariadb.com/kb/en/configuring-mariadb-with-option-files/>).
#'
#' @return A connection object (i.e., a specified list), ready to be passed to other [ScrapeBotR] functions.
#'
#' @examples
#' \dontrun{
#'
#' connect('my_db on localhost')
#' connect('my_db on localhost', '~/my_own_credentials_file.ini')
#' }
#'
#' @seealso [write_credentials()] to help create the credentials file, [disconnect()] to close a database connection, <https://github.com/MarHai/ScrapeBot> for more information on the central database.
#'
#' @export

connect <- function(credentials_section, credentials_file = '~/.scrapebot_database.ini') {

  # Test input
  if(!is.character(credentials_section)) {
    stop('Credential section needs to be a character string.')
  }
  if(!is.character(credentials_file)) {
    stop('Credential file needs to be a character string.')
  }

  # Try connecting to the database
  tryCatch({
    maria <- DBI::dbConnect(RMariaDB::MariaDB(),
                            default.file = credentials_file,
                            groups = credentials_section)
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
    credentials_file = credentials_file,
    credentials_section = credentials_section,
    tables = DBI::dbListTables(maria)
  )

  return(connection)
}

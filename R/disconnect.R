#' Disconnect current database
#'
#' Properly closes a database connection. Use this if you are employing multiple connections and/or if you are annoyed by the warnings saying to `call dbDisconnect_scrapebot() when finished working with a connection`.
#'
#' @param connection A connection object, as retrieved from [connect_scrapebot()].
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
#' disconnect(connection)
#' }
#'
#' @seealso [connect_scrapebot()]
#' @export

disconnect <- function(connection) {

  # Test input
  if(is.null(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect_scrapebot.')
  }

  DBI::dbDisconnect(connection$db)
}

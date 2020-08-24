#' Disconnect current database
#'
#' Properly closes a database connection. Use this if you are employing multiple connections and/or if you are annoyed by the warnings saying to `call dbDisconnect() when finished working with a connection`.
#'
#' @param connection A connection object, as retrieved from [connect()].
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
#' disconnect(connection)
#' }
#'
#' @seealso [connect()]
#' @export

disconnect <- function(connection) {

  # Test input
  if(is.null(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect.')
  }

  DBI::dbDisconnect(connection$db)
}

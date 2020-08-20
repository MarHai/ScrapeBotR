#' Retrieve log entries for a provided run
#'
#' @param connection a connection object, as retrieved from [connect()]
#' @param run_uid numeric UID of the run to find log entries for
#'
#' @return a [tibble][tibble::tibble-package] listing all log entries including a timestamp, its type (one of: info, warning, error), and the message itself
#'
#' @examples
#' \dontrun{
#' connection <- connect('localhost', 'root', 's3cr3t_password')
#' get_run_log(connection, 13)
#' disconnect(connection)
#' }
#'
#' @seealso [get_runs()], [get_run_data()]
#' @importFrom magrittr %>%
#' @export

get_run_log <- function(connection, run_uid) {

  # Test input
  if(is.null(connection$db) | !DBI::dbIsValid(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect.')
  }
  if(is.null(run_uid)) {
    stop('Missing numeric run_uid to collect its log entries.')
  }

  # Collect log entries
  type_levels <- c('info', 'warning', 'error')
  tryCatch({
    query <- paste0(
      'SELECT a.created, a.type, a.message ',
      'FROM log a ',
      'WHERE a.run_uid = ',
      as.integer(run_uid),
      ' ORDER BY a.created ASC, a.uid ASC'
    )
    DBI::dbGetQuery(connection$db, query) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        type = factor(type, levels = type_levels)
      ) %>%
      return()
  }, error = function(e) {
    warning(paste0('Collecting log entries resulted in an ', e))
    return(tibble::tibble(
      created = as.POSIXct(character()),
      type = factor(levels = type_levels),
      message = character()
    ))
  })
}

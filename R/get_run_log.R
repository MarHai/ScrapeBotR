#' Retrieve log entries for a provided run
#'
#' @param connection A connection object, as retrieved from [connect()].
#' @param run_uid Numeric UID or a vector of numeric UIDs of the run to find log entries for.
#'
#' @return A [tibble][tibble::tibble-package] listing all log entries including a timestamp, its type (one of: `info`, `warning`, `error`), and the message itself.
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('localhost', 'root', 's3cr3t_password')
#' get_run_log(connection, 13)
#' get_run_log(connection, c(13, 99))
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
      'SELECT a.run_uid, a.created, a.type, a.message ',
      'FROM log a ',
      'WHERE a.run_uid IN (',
      paste(c(as.integer(run_uid)), collapse = ', '),
      ') ORDER BY a.run_uid, a.created ASC, a.uid ASC'
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
      run_uid = integer(),
      created = as.POSIXct(character()),
      type = factor(levels = type_levels),
      message = character()
    ))
  })
}

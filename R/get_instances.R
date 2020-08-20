#' Get a tibble of all available instances
#'
#' @param connection a connection object, as retrieved from [connect()]
#'
#' @return a [tibble][tibble::tibble-package] listing all available instances including its UID, name, the date then the instance was first created, its description, as well as the number of runs on that instance and the date of the latest run on that instance
#'
#' @examples
#' \dontrun{
#' connection <- connect('localhost', 'root', 's3cr3t_password')
#' get_instances(connection)
#' disconnect(connection)
#' }
#'
#' @importFrom magrittr %>%
#' @export

get_instances <- function(connection) {

  # Test input
  if(is.null(connection$db) | !DBI::dbIsValid(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect.')
  }

  # Collect instances
  tryCatch({
    DBI::dbGetQuery(connection$db,
                    'SELECT a.uid, a.name, a.created, a.description,
                    COUNT(b.uid) AS runs_count, MAX(b.created) AS runs_latest
                    FROM instance a
                    LEFT JOIN run b ON (b.instance_uid = a.uid)
                    GROUP BY a.uid
                    ORDER BY a.name ASC') %>%
      tibble::as_tibble() %>%
      dplyr::mutate(runs_count = as.integer(runs_count)) %>%
      return()
  }, error = function(e) {
    warning(paste0('Collecting intances resulted in an ', e))
    return(tibble::tibble(
      uid = integer(),
      name = character(),
      created = as.POSIXct(character()),
      description = character(),
      runs_count = integer(),
      runs_latest = as.POSIXct(character())
    ))
  })
}

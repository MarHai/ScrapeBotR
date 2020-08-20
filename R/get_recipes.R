#' Get a tibble of all recipes
#'
#' @param connection a connection object, as retrieved from [connect()]
#' @param include_inactive if true, inactive recipes are included along active recipes; defaults to `FALSE`
#'
#' @return a [tibble][tibble::tibble-package] listing all available recipes including its UID, name, the date then the recipe was first created, its description, whether the recipe is active, as well as the number of its runs and the date of the latest run
#'
#' @examples
#' \dontrun{
#' connection <- connect('localhost', 'root', 's3cr3t_password')
#' get_recipes(connection)
#' get_recipes(connection, include_inactive = TRUE)
#' disconnect(connection)
#' }
#'
#' @seealso [get_recipe_steps()]
#' @importFrom magrittr %>%
#' @export

get_recipes <- function(connection, instance_uid = NULL, include_inactive = FALSE) {

  # Test input
  if(is.null(connection$db) | !DBI::dbIsValid(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect.')
  }

  # Collect recipes
  tryCatch({
    query <- paste0(
      'SELECT a.uid, a.name, a.created, a.description, a.active, COUNT(b.uid) AS runs_count, MAX(b.created) AS runs_latest ',
      'FROM recipe a ',
      'LEFT JOIN run b ON (b.recipe_uid = a.uid) ',
      ifelse(include_inactive, '', 'WHERE a.active '),
      'GROUP BY a.uid ',
      'ORDER BY a.name ASC'
    )
    DBI::dbGetQuery(connection$db, query) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        runs_count = as.integer(runs_count),
        active = as.logical(active)
      ) %>%
      return()
  }, error = function(e) {
    warning(paste0('Collecting recipes resulted in an ', e))
    return(tibble::tibble(
      uid = integer(),
      name = character(),
      created = as.POSIXct(character()),
      description = character(),
      active = logical(),
      runs_count = integer(),
      runs_latest = as.POSIXct(character())
    ))
  })
}

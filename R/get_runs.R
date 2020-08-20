#' Get a tibble of all runs for the provided instance/recipe
#'
#' A run represents one execution of a recipe on a certain instance. For this function to provide meaningful results, you need to ...
#' - specify only `instance_uid` to list all runs on a specific instance
#' - specify only `recipe_uid` to list all runs for a specific recipe
#' - specify both `instance_uid` and `recipe_uid` to list all runs of a specific recipe on a given instance
#'
#' The function will yield an error if neither one of instance/recipe is provided.
#'
#' Note that the resulting tibble might be rather large and the request could take some time.
#'
#' @param connection a connection object, as retrieved from [connect()]
#' @param instance_uid numeric UID of the instance to filter runs for. If NULL, `recipe_uid` has to be provided. Defaults to NULL.
#' @param recipe_uid numeric UID of the recipe to filter runs for. If NULL, `instance_uid` has to be provided. Defaults to NULL.
#'
#' @return a [tibble][tibble::tibble-package] listing all runs including their UID, the UID of the run's recipe, the UID of the run's instance, the date when the run started, its runtime (in seconds), and the status (one of: success, error, config_error, command_not_found, in_progress)
#'
#' @examples
#' \dontrun{
#' connection <- connect('localhost', 'root', 's3cr3t_password')
#' get_runs(connection, instance_uid = 42, recipe_uid = 21)
#' get_runs(connection, instance_uid = 42)
#' get_runs(connection, recipe_uid = 21)
#' disconnect(connection)
#' }
#'
#' @seealso [get_run_data()], [get_run_log()]
#' @importFrom magrittr %>%
#' @export

get_runs <- function(connection, instance_uid = NULL, recipe_uid = NULL) {

  # Test input
  if(is.null(connection$db) | !DBI::dbIsValid(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect.')
  }
  if(is.null(instance_uid) & is.null(recipe_uid)) {
    stop('Either instance_uid or recipe_uid (or both) need(s) to be provided.')
  }

  # Collect runs
  status_levels <- c('success', 'error', 'config_error', 'command_not_found', 'in_progress')
  tryCatch({
    query <- paste0(
      'SELECT a.uid, b.uid AS recipe_uid, c.uid AS instance_uid, a.created AS started, a.runtime, a.status ',
      'FROM run a ',
      'LEFT JOIN recipe b ON (a.recipe_uid = b.uid) ',
      'LEFT JOIN instance c ON (a.instance_uid = c.uid) ',
      'WHERE ',
      ifelse(is.null(instance_uid), '1 = 1 ', paste0('a.instance_uid = ', as.integer(instance_uid), ' ')),
      'AND ',
      ifelse(is.null(recipe_uid), '1 = 1 ', paste0('a.recipe_uid = ', as.integer(recipe_uid), ' ')),
      'GROUP BY a.uid ',
      'ORDER BY a.created ASC'
    )

    # Since this result can be quite large, we go through in portions
    runs <- NULL
    result <- DBI::dbSendQuery(connection$db, query)
    while(!DBI::dbHasCompleted(result)) {
      runs <-
        runs %>%
        dplyr::bind_rows(
          DBI::dbFetch(result, n = 1000) %>%
            tibble::as_tibble()
        )
    }
    DBI::dbClearResult(result)

    runs %>%
      dplyr::mutate(
        status = factor(status, levels = status_levels)
      ) %>%
      return()
  }, error = function(e) {
    warning(paste0('Collecting runs resulted in an ', e))
    return(tibble::tibble(
      uid = integer(),
      recipe_uid = integer(),
      instance_uid = integer(),
      started = as.POSIXct(character()),
      runtime = integer(),
      status = factor(levels = status_levels)
    ))
  })
}

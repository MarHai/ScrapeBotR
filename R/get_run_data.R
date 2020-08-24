#' Collect (large amounts of) data for a given combination of criteria
#'
#' Data is stored heavily within ScrapeBot. Hence, filtering needs to be applied to collect data. Two types of filter are possible:
#' * Filter on the runs for which to collect data: Specify one or many of `run_uid`, `instance_uid`, and `recipe_uid.` At least one is required.
#' * Filter on the step for which to collect data: Optionally specify a `step_uid`.
#' In general, this function only returns data from "success"-ful runs (see [get_runs()]). It then collects runs matching the first set of criteria (i.e., `run_uid`, `instance_uid`, and `recipe_uid`). Ultimately, it collects data, thereby focusing on a specific step if specified.
#'
#' @param connection A connection object, as retrieved from [connect()].
#' @param run_uid Optional numeric UID or a vector of numeric UIDs of a specific run to collect data from. If `NULL`, either `instance_uid` or `recipe_uid` (or both) has to be provided. Defaults to `NULL`.
#' @param instance_uid Optional numeric UID or a vector of numeric UIDs of the instance to filter data for. If `NULL`, either `run_uid` or `recipe_uid` (or both) has to be provided. Defaults to `NULL`.
#' @param recipe_uid Optional numeric UID or a vector of numeric UIDs of the recipe to filter data for. If `NULL`, either `instance_uid` or `run_uid` (or both) has to be provided. Defaults to `NULL`.
#' @param step_uid Optional numeric UID or a vector of numeric UIDs of a step to filter data for. Defaults to `NULL`.
#'
#' @return A [tibble][tibble::tibble-package] listing all data including their creation timestamp, the UIDs of its run, instance, recipe, and step, as well as the data value itself.
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
#' get_run_data(connection, instance_uid = 42)
#' get_run_data(connection, run_uid = 256)
#' get_run_data(connection, recipe_uid = 21)
#' get_run_data(connection, run_uid = c(256, 1024))
#' get_run_data(connection, recipe_uid = 21:23)
#' get_run_data(connection, instance_uid = 42, recipe_uid = 21)
#' get_run_data(connection, instance_uid = 42, recipe_uid = 21, step_uid = 8)
#' disconnect(connection)
#' }
#'
#' @seealso [get_runs()], [get_run_log()]
#' @importFrom magrittr %>%
#' @export
#'
get_run_data <- function(connection,
                         run_uid = NULL, instance_uid = NULL, recipe_uid = NULL,
                         step_uid = NULL) {

  # Test input
  if(is.null(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect.')
  }
  if(is.null(run_uid) & is.null(instance_uid) & is.null(recipe_uid)) {
    stop('Either run_uid or instance_uid or recipe_uid (or a combination thereof) need(s) to be provided.')
  }

  # Find successful runs (and return, if no runs match criteria)
  runs <- tibble::tibble(uid = integer())
  if(is.null(run_uid)) {
    runs <-
      get_runs(connection, instance_uid, recipe_uid) %>%
      dplyr::filter(status == 'success')
  } else {
    runs <-
      DBI::dbGetQuery(connection$db,
                      paste0(
                        'SELECT uid, recipe_uid, instance_uid, created AS started, runtime, status ',
                        'FROM run ',
                        'WHERE uid IN (',
                        paste(c(as.integer(run_uid)), collapse = ', '),
                        ') '
                      )) %>%
      tibble::as_tibble()
  }

  if(nrow(runs) == 0) {
    warning('No runs match your combined criteria of run_uid, instance_uid, and recipe_uid.')
    return(tibble::tibble(
      created = as.POSIXct(character()),
      run_uid = integer(),
      instance_uid = integer(),
      recipe_uid = integer(),
      step_uid = integer(),
      value = character()
    ))
  }

  # Collect data
  tryCatch({
    query <- paste0(
      'SELECT a.created, a.run_uid, a.step_uid, a.value ',
      'FROM data a ',
      'WHERE run_uid IN (',
      runs %>% dplyr::pull(uid) %>% paste(collapse = ', '),
      ') ',
      ifelse(is.null(step_uid), '', paste0('AND a.step_uid IN (', paste(c(as.integer(step_uid)), collapse = ', '), ') ')),
      'ORDER BY a.created ASC, a.run_uid ASC'
    )

    # Since this result can be quite large, we go through in portions
    data <- NULL
    result <- DBI::dbSendQuery(connection$db, query)
    while(!DBI::dbHasCompleted(result)) {
      data <-
        data %>%
        dplyr::bind_rows(
          DBI::dbFetch(result, n = 1000) %>%
            tibble::as_tibble()
        )
    }
    DBI::dbClearResult(result)

    data %>%
      dplyr::left_join(
        runs %>%
          dplyr::select(run_uid = uid, instance_uid, recipe_uid),
        by = 'run_uid'
      ) %>%
      dplyr::select(created, run_uid, instance_uid, recipe_uid, step_uid, value) %>%
      return()
  }, error = function(e) {
    warning(paste0('Collecting data resulted in an ', e))
    return(tibble::tibble(
      created = as.POSIXct(character()),
      run_uid = integer(),
      instance_uid = integer(),
      recipe_uid = integer(),
      step_uid = integer(),
      value = character()
    ))
  })
}

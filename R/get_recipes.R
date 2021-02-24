#' Get a tibble of available recipes
#'
#' Default behavior is to just retrieve all active recipes, no matter on which instance they are running.
#' You may change this behavior in two ways (or combinations thereof):
#' * If `instance_uid` is set, only recipes assigned to at least one of these instances are returned.
#' * If `include_inactive` is set to `TRUE`, also recipes currently not active are returned.
#'
#' @param connection A connection object, as retrieved from [connect_scrapebot()].
#' @param instance_uid Numeric UID or a vector of numeric UIDs of the instance to filter recipes for. Filtering is inclusive (i.e., recipes are included if they are assigned to at least one of the instances; no need to be assigned to all of them to be returned). If NULL, all recipes are provided. Defaults to `NULL`.
#' @param include_inactive If `TRUE`, inactive recipes are included along active recipes; defaults to `FALSE`.
#'
#' @return A [tibble][tibble::tibble-package] listing all available recipes including its UID, name, the date then the recipe was first created, its description, whether the recipe is active, as well as the number of its runs and the date of the latest run.
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
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
  if(is.null(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect_scrapebot.')
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
    recipes <-
      DBI::dbGetQuery(connection$db, query) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        runs_count = as.integer(runs_count),
        active = as.logical(active)
      )

    if(!is.null(instance_uid)) {
      recipe2instance <-
        DBI::dbGetQuery(connection$db,
                        paste0(
                          'SELECT recipe_uid FROM recipe2instance WHERE instance_uid IN (',
                          paste(c(as.integer(instance_uid)), collapse = ', '),
                          ')'
                        )) %>%
        tibble::as_tibble()
      recipes <-
        recipes %>%
        dplyr::filter(uid %in% recipe2instance$recipe_uid)
    }

    return(recipes)
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

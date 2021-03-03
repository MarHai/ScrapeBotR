#' Get a tibble of a given recipe's steps
#'
#' @param connection A connection object, as retrieved from [connect_scrapebot()].
#' @param recipe_uid Numeric UID or a vector of numeric UIDs of the recipe to retrieve steps for.
#' @param include_inactive If `TRUE`, inactive recipes are included along active recipes; defaults to `FALSE`.
#'
#' @return A [tibble][tibble::tibble-package] listing all recipe steps including their `recipe_uid`, UID, type, value (if necessary; depending on type), whether the step builds on a randomly picked item, whether the step builds on previously collected data, whether it is active, as well as a character vector of random items to choose from (if `use_random_item`).
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
#' get_recipe_steps(connection, recipe_uid = 21)
#' get_recipe_steps(connection, recipe_uid = c(21, 32))
#' get_recipe_steps(
#'     connection,
#'     recipe_uid = 21,
#'     include_inactive = TRUE
#' )
#' disconnect(connection)
#' }
#'
#' @seealso [get_recipes()]
#' @importFrom magrittr %>%
#' @export

get_recipe_steps <- function(connection, recipe_uid, include_inactive = FALSE) {

  # Test input
  if(is.null(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect_scrapebot.')
  }
  if(is.null(recipe_uid)) {
    stop('Missing numeric recipe_uid to collect its steps.')
  }

  # Collect steps
  tryCatch({
    query <- paste0(
      'SELECT a.recipe_uid, a.uid, a.type, a.value, a.use_random_item_instead_of_value AS use_random_item, a.use_data_item_instead_of_value AS use_data_item, a.active ',
      'FROM recipestep a ',
      'WHERE a.recipe_uid IN (',
      paste(c(as.integer(recipe_uid)), collapse = ', '),
      ') ',
      ifelse(include_inactive, '', 'AND a.active '),
      'ORDER BY a.recipe_uid ASC, a.sort ASC'
    )
    steps <-
      DBI::dbGetQuery(connection$db, query) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        use_random_item = as.logical(use_random_item),
        use_data_item = as.logical(use_data_item),
        active = as.logical(active),
        random_items = vector(mode = 'list', length = dplyr::n())
      )

    # add random items
    if(nrow(steps) > 0) {
      for(i in 1:nrow(steps)) {
        if(steps[[i, 'use_random_item']]) {
          query <- paste0('SELECT a.value FROM recipestepitem a ',
                          'WHERE a.step_uid = ', steps[[i, 'uid']],
                          ' ORDER BY a.value ASC')
          random_items <- DBI::dbGetQuery(connection$db, query)$value
          if(length(random_items) > 0) {
            steps[[i, 'random_items']] <- list(random_items)
          }
        }
      }
    }

    return(steps)
  }, error = function(e) {
    warning(paste0('Collecting recipe steps resulted in an ', e))
    return(tibble::tibble(
      recipe_uid = integer(),
      uid = integer(),
      type = character(),
      value = character(),
      use_random_item = logical(),
      use_data_item = logical(),
      active = logical(),
      random_items = character()
    ))
  })
}

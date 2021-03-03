#' Set a recipe to be run on a given instance.
#'
#' @param connection A connection object, as retrieved from [connect_scrapebot()].
#' @param recipe_uid The `uid` (integer) of the corresponding recipe.
#' @param instance_uid The `uid` (integer) of the corresponding instance.
#'
#' @return `TRUE` if update was successful, `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
#' combine_recipe_with_instance(connection, 42, 2)
#' disconnect(connection)
#' }
#'
#' @seealso [get_recipes()], [get_instances()]
#' @importFrom magrittr %>%
#' @export

combine_recipe_with_instance <- function(connection, recipe_uid, instance_uid) {

  # Test input
  if(is.null(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect_scrapebot.')
  }

  tryCatch({
    recipe <-
      get_recipes(connection, include_inactive = TRUE) %>%
      dplyr::filter(uid == recipe_uid)
    instance <-
      get_instances(connection) %>%
      dplyr::filter(uid == instance_uid)

    if(nrow(recipe) == 1 & nrow(instance) == 1) {
      recipe2instance <-
        DBI::dbGetQuery(connection$db,
                        paste0('SELECT COUNT(*) AS cnt FROM recipe2instance WHERE recipe_uid = ',
                               as.integer(recipe_uid), ' AND instance_uid = ', as.integer(instance_uid))) %>%
        tibble::as_tibble()

      if(recipe2instance[[1, 'cnt']] > 0) {
        return(TRUE)
      } else {
        DBI::dbAppendTable(connection$db,
                           'recipe2instance',
                           data.frame(created = as.character(as.POSIXct(Sys.time())),
                                      recipe_uid = as.integer(recipe_uid),
                                      instance_uid = as.integer(instance_uid),
                                      cookies_from_last_run = '{}'))
        return(combine_recipe_with_instance(connection, recipe_uid, instance_uid))
      }
    } else {
      return(FALSE)
    }
  }, error = function(e) {
    warning(paste0('Combining a recipe with an instance resulted in an ', e))
    return(FALSE)
  })
}


#' Remove a recipe from a given instance.
#'
#' This function only makes a recipe stop running on a given instance.
#' It does not really delete, neither the recipe, nor the instance, or any previous runs/data.
#'
#' @param connection A connection object, as retrieved from [connect_scrapebot()].
#' @param recipe_uid The `uid` (integer) of the corresponding recipe.
#' @param instance_uid The `uid` (integer) of the corresponding instance.
#'
#' @return `TRUE` if removal was successful, `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
#' remove_recipe_from_instance(connection, 42, 2)
#' disconnect(connection)
#' }
#'
#' @seealso [get_recipes()], [get_instances()]
#' @importFrom magrittr %>%
#' @export

remove_recipe_from_instance <- function(connection, recipe_uid, instance_uid) {

  # Test input
  if(is.null(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect_scrapebot.')
  }

  tryCatch({
    recipe <-
      get_recipes(connection, include_inactive = TRUE) %>%
      dplyr::filter(uid == recipe_uid)
    instance <-
      get_instances(connection) %>%
      dplyr::filter(uid == instance_uid)

    if(nrow(recipe) == 1 & nrow(instance) == 1) {
      recipe2instance <-
        DBI::dbGetQuery(connection$db,
                        paste0('SELECT COUNT(*) AS cnt FROM recipe2instance WHERE recipe_uid = ',
                               as.integer(recipe_uid), ' AND instance_uid = ', as.integer(instance_uid))) %>%
        tibble::as_tibble()

      if(recipe2instance[[1, 'cnt']] == 0) {
        return(TRUE)
      } else {
        result <- DBI::dbSendQuery(connection$db,
                                   paste0('DELETE FROM recipe2instance WHERE recipe_uid = ',
                                          as.integer(recipe_uid), ' AND instance_uid = ', as.integer(instance_uid),
                                          ' LIMIT 1'))
        DBI::dbClearResult(result)
        return(remove_recipe_from_instance(connection, recipe_uid, instance_uid))
      }
    } else {
      return(FALSE)
    }
  }, error = function(e) {
    warning(paste0('Removing a recipe from an instance resulted in an ', e))
    return(FALSE)
  })
}

#' Activate (i.e., set active = TRUE) for a given recipe.
#'
#' @param connection A connection object, as retrieved from [connect_scrapebot()].
#' @param recipe_uid The `uid` (integer) of the corresponding recipe.
#'
#' @return `TRUE` if update was successful, `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
#' activate_recipe(connection, 42)
#' disconnect(connection)
#' }
#'
#' @seealso [get_recipes()], [add_recipe()]
#' @importFrom magrittr %>%
#' @export

activate_recipe <- function(connection, recipe_uid) {

  # Test input
  if(is.null(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect_scrapebot.')
  }

  tryCatch({
    recipe <-
      get_recipes(connection, include_inactive = TRUE) %>%
      dplyr::filter(uid == recipe_uid)

    if(nrow(recipe) == 1) {
      if(recipe[[1, 'active']]) {
        return(TRUE)
      } else {
        result <- DBI::dbSendQuery(connection$db,
                                   paste0('UPDATE recipe SET `active` = 1 WHERE uid = ',
                                          as.integer(recipe_uid)))
        DBI::dbClearResult(result)
        return(activate_recipe(connection, recipe_uid))
      }
    } else {
      return(FALSE)
    }
  }, error = function(e) {
    warning(paste0('Activating a recipe resulted in an ', e))
    return(FALSE)
  })
}



#' Deactivate (i.e., set active = FALSE) for a given recipe.
#'
#' @param connection A connection object, as retrieved from [connect_scrapebot()].
#' @param recipe_uid The `uid` (integer) of the corresponding recipe.
#'
#' @return `TRUE` if update was successful, `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
#' deactivate_recipe(connection, 42)
#' disconnect(connection)
#' }
#'
#' @seealso [get_recipes()], [add_recipe()]
#' @importFrom magrittr %>%
#' @export

deactivate_recipe <- function(connection, recipe_uid) {

  # Test input
  if(is.null(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect_scrapebot.')
  }

  tryCatch({
    recipe <-
      get_recipes(connection, include_inactive = TRUE) %>%
      dplyr::filter(uid == recipe_uid)

    if(nrow(recipe) == 1) {
      if(!recipe[[1, 'active']]) {
        return(TRUE)
      } else {
        result <- DBI::dbSendQuery(connection$db,
                                   paste0('UPDATE recipe SET `active` = 0 WHERE uid = ',
                                          as.integer(recipe_uid)))
        DBI::dbClearResult(result)
        return(deactivate_recipe(connection, recipe_uid))
      }
    } else {
      return(FALSE)
    }
  }, error = function(e) {
    warning(paste0('Deactivating a recipe resulted in an ', e))
    return(FALSE)
  })
}

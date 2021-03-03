#' Activate (i.e., set active = TRUE) for a given recipe step.
#'
#' @param connection A connection object, as retrieved from [connect_scrapebot()].
#' @param recipe_uid The `uid` (integer) of the corresponding recipe.
#' @param step_uid The `uid` (integer) of the corresponding recipe step.
#'
#' @return `TRUE` if update was successful, `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
#' activate_recipe_step(connection, 42, 256)
#' disconnect(connection)
#' }
#'
#' @seealso [get_recipe_steps()], [add_recipe_step()]
#' @importFrom magrittr %>%
#' @export

activate_recipe_step <- function(connection, recipe_uid, step_uid) {

  # Test input
  if(is.null(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect_scrapebot.')
  }

  tryCatch({
    step <-
      get_recipe_steps(connection, recipe_uid, include_inactive = TRUE) %>%
      dplyr::filter(uid == step_uid)

    if(nrow(step) == 1) {
      if(step[[1, 'active']]) {
        return(TRUE)
      } else {
        result <- DBI::dbSendQuery(connection$db,
                                   paste0('UPDATE recipestep SET `active` = 1 WHERE uid = ',
                                          as.integer(step_uid)))
        DBI::dbClearResult(result)
        return(activate_recipe_step(connection, recipe_uid, step_uid))
      }
    } else {
      return(FALSE)
    }
  }, error = function(e) {
    warning(paste0('Activating a recipe step resulted in an ', e))
    return(FALSE)
  })
}



#' Deactivate (i.e., set active = FALSE) for a given recipe step.
#'
#' @param connection A connection object, as retrieved from [connect_scrapebot()].
#' @param recipe_uid The `uid` (integer) of the corresponding recipe.
#' @param step_uid The `uid` (integer) of the corresponding recipe step.
#'
#' @return `TRUE` if update was successful, `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
#' deactivate_recipe_step(connection, 42, 256)
#' disconnect(connection)
#' }
#'
#' @seealso [get_recipe_steps()], [add_recipe_step()]
#' @importFrom magrittr %>%
#' @export

deactivate_recipe_step <- function(connection, recipe_uid, step_uid) {

  # Test input
  if(is.null(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect_scrapebot.')
  }

  tryCatch({
    step <-
      get_recipe_steps(connection, recipe_uid, include_inactive = TRUE) %>%
      dplyr::filter(uid == step_uid)

    if(nrow(step) == 1) {
      if(!step[[1, 'active']]) {
        return(TRUE)
      } else {
        result <- DBI::dbSendQuery(connection$db,
                                   paste0('UPDATE recipestep SET `active` = 0 WHERE uid = ',
                                          as.integer(step_uid)))
        DBI::dbClearResult(result)
        return(deactivate_recipe_step(connection, recipe_uid, step_uid))
      }
    } else {
      return(FALSE)
    }
  }, error = function(e) {
    warning(paste0('Deactivating a recipe step resulted in an ', e))
    return(FALSE)
  })
}

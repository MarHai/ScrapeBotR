#' Add a new recipe
#'
#' Adds a newly created recipe to the database. This new recipe is empty (i.e., does not contain steps).
#' Also, this new recipe f
#'
#' @param connection A connection object, as retrieved from [connect_scrapebot()].
#' @param name Character string identifying the recipe.
#' @param recipe_owner The email address of the ScrapeBot user who will be the owner of the new recipe as character string. If this one does not exist, it will be created (in this case, a text will be raised).
#' @param description Character string to describe the recipe even further; default is an empty string.
#' @param active Boolean to indicate whether the recipe should be active; default is `FALSE`.
#' @param cookies Boolean to indicate whether the recipe should store cookies; default is `FALSE`.
#' @param interval Integer to indicate the number of minutes after which a recipe should be run; default is `15`.
#'
#' @return The `uid` (integer) of the newly created recipe or an integer `NA` in case of error.
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
#' add_recipe(connection, 'visit my website', 'mario@haim.it')
#' add_recipe(connection, 'visit my website', 'mario@haim.it', active = T)
#' disconnect(connection)
#' }
#'
#' @seealso [get_recipes()], [add_recipe_step()]
#' @importFrom magrittr %>%
#' @export

add_recipe <- function(connection, name, recipe_owner,
                       description = '', active = FALSE, cookies = FALSE, interval = 15) {

  # Test input
  if(is.null(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect_scrapebot.')
  }
  if(!is.character(name)) {
    stop('Name needs to be set as character string.')
  }


  tryCatch({
    recipes.before <- get_recipes(connection, include_inactive = T)

    recipe.name <- stringr::str_trim(name)
    recipe.description <- stringr::str_trim(as.character(description))
    recipe.created <- as.character(as.POSIXct(Sys.time()))
    recipe.active <- dplyr::if_else(active, 1, 0, missing = 0)

    DBI::dbAppendTable(connection$db,
                       'recipe',
                       data.frame(name = recipe.name,
                                  created = recipe.created,
                                  description = recipe.description,
                                  active = recipe.active,
                                  cookies = dplyr::if_else(cookies, 1, 0, missing = 0),
                                  interval = as.integer(interval),
                                  owner_uid = get_or_create_user(connection,
                                                                 recipe_owner)))

    recipe <-
      get_recipes(connection, include_inactive = T) %>%
      dplyr::filter(name == recipe.name,
                    active == as.logical(recipe.active),
                    !uid %in% recipes.before$uid) %>%
      dplyr::arrange(dplyr::desc(created))
    if(nrow(recipe) > 0) {
      return(as.integer(recipe[[1, 'uid']]))
    }
  }, error = function(e) {
    warning(paste0('Creating a new recipe resulted in an ', e))
    return(NA_integer_)
  })
}

#' Import a ScrapeBot JSON recipe file (`.sbj`)
#'
#' @param connection A connection object, as retrieved from [connect_scrapebot()].
#' @param sbj_file Character string identifying the recipe.
#' @param recipe_owner The email address of the ScrapeBot user who will be the owner of the new recipe as character string. If this one does not exist, it will be created (in this case, a text will be raised).
#'
#' @return The `uid` (integer) of the newly created recipe or an integer `NA`.
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
#' import_sbj(
#'   connection,
#'   'https://raw.githubusercontent.com/MarHai/ScrapeBot/master/recipes/find_my_ip.sbj',
#'   'mario@haim.it'
#' )
#' disconnect(connection)
#' }
#'
#' @seealso [add_recipe()], [add_recipe_step()]
#' @importFrom magrittr %>%
#' @export

import_sbj <- function(connection, sbj_file, recipe_owner) {

  # Test input
  if(is.null(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect_scrapebot.')
  }
  if(file.access(sbj_file,
                 mode = 4) < 0) {
    stop('SBJ file not readable.')
  }

  tryCatch({
    sbj <- jsonlite::read_json(sbj_file, simplifyVector = T)
    recipe <- add_recipe(connection,
                         sbj$name,
                         get_or_create_user(connection,
                                            recipe_owner),
                         sbj$description,
                         sbj$active,
                         sbj$cookies,
                         sbj$interval)
    for(i in 1:nrow(sbj$steps)) {
      add_recipe_step(connection,
                      recipe,
                      sbj$steps[[i, 'type']],
                      sbj$steps[[i, 'value']],
                      c(sbj$steps[[i, 'random_items']]),
                      sbj$steps[[i, 'use_data_item_instead_of_value']],
                      sbj$steps[[i, 'active']],
                      sbj$steps[[i, 'sort']])
    }
    return(recipe)
  }, error = function(e) {
    warning(paste0('Importing the SBJ file resulted in an ', e))
    return(NA_integer_)
  })
}

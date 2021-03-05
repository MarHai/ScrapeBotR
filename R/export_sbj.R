#' Export a ScrapeBot JSON recipe file (`.sbj`)
#'
#' Given a recipe (`UID`), this function exports the recipe's SBJ representation.
#'
#' @param connection A connection object, as retrieved from [connect_scrapebot()].
#' @param recipe_uid The recipe's `UID`, for which the SBJ should be exported.
#' @param sbj_file If set to a file path, the SBJ file is directly written to this path. Otherwise (or `NULL`), character-string representation is returned.
#'
#' @return If `sbj_file` is set to a valid file path, this path is returned after a successful write. Otherwise, the SBJ raw text is returned.
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
#' export_sbj(connection, 42)
#' export_sbj(connection, 42, 'my_recipe.sbj')
#' disconnect(connection)
#' }
#'
#' @seealso [import_sbj()]
#' @importFrom magrittr %>%
#' @export

export_sbj <- function(connection, recipe_uid, sbj_file = NULL) {

  # Test input
  if(is.null(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect_scrapebot.')
  }
  if(!is.null(sbj_file)) {
    if(file.access(sbj_file, mode = 2) < 0 &
       file.access(sbj_file, mode = 0) == 0) {
      warning('Provided SBJ file path not NULL; file exists but is not writable. Will return SBJ instead.')
      sbj_file <- NULL
    }
  }

  tryCatch({
    recipe <-
      get_recipes(connection, include_inactive = TRUE) %>%
      dplyr::filter(uid == recipe_uid)
    if(nrow(recipe) != 1) {
      stop('Recipe not found.')
    }

    steps <- get_recipe_steps(connection,
                              recipe_uid,
                              include_inactive = TRUE)
    steps <-
      steps %>%
      tibble::rowid_to_column('sort') %>%
      dplyr::select(sort, type, value, active,
                    use_random_item_instead_of_value = use_random_item,
                    random_items,
                    use_data_item_instead_of_value = use_data_item)

    sbj <-
      list('_comment' = jsonlite::unbox(paste0('ScrapeBot recipe, created ',
                                               recipe[[1, 'created']], ', exported ',
                                               as.character(as.POSIXct(Sys.time())))),
           'name' = jsonlite::unbox(recipe[[1, 'name']]),
           'description' = jsonlite::unbox(recipe[[1, 'description']]),
           'interval' = jsonlite::unbox(as.integer(recipe[[1, 'interval']])),
           'cookies' = jsonlite::unbox(as.logical(recipe[[1, 'cookies']])),
           'active' = jsonlite::unbox(as.logical(recipe[[1, 'active']])),
           'steps' = steps)

    if(is.null(sbj_file)) {
      return(jsonlite::toJSON(sbj, pretty = TRUE, null = 'null'))
    } else {
      jsonlite::write_json(sbj, sbj_file, pretty = TRUE, null = 'null')
      return(sbj_file)
    }
  }, error = function(e) {
    stop(paste0('Exporting the SBJ file resulted in an ', e))
  })
}

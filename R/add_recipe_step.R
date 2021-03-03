#' Add a new single recipe step
#'
#' Adds a newly created recipe step to the database.
#' See https://github.com/MarHai/ScrapeBot/blob/master/scrapebot/emulate.py for type descriptions.
#'
#' @param connection A connection object, as retrieved from [connect_scrapebot()].
#' @param recipe_uid The `uid` (integer) of the corresponding recipe.
#' @param type The type of what this step should do. Has to be one of the following as character string:
#' - click
#' - data
#' - element_screenshot
#' - execute_js
#' - find_by_class
#' - find_by_css
#' - find_by_id
#' - find_by_link
#' - find_by_link_partial
#' - find_by_name
#' - find_by_tag
#' - find_by_xpath
#' - get_attribute
#' - get_attributes
#' - get_element_count
#' - get_htmlsource
#' - get_pagetitle
#' - get_text
#' - get_texts
#' - get_value
#' - get_values
#' - go_back
#' - go_forward
#' - log
#' - navigate
#' - pause
#' - post_all_data
#' - post_previous_step_data
#' - random_select
#' - screenshot
#' - scroll_to
#' - sometimes_screenshot
#' - submit
#' - unset_prior_element
#' - write
#' - write_slowly
#' @param fixed_value Possibly required value (depends on type); default is an empty character string.
#' @param random_items Character vector to be used as urn to draw random items for `fixed_value` from.
#' @param use_data_item_instead_of_value Logical indicating whether value should be taken from a previous data item. If `TRUE`, `fixed_value` needs to be set accordingly.
#' @param active Boolean to indicate whether the recipe step should be active; default is `TRUE`.
#' @param sort Numeric index to have steps be run one after another. If `NA`, new step is appended to the end.
#'
#' @return The `uid` (integer) of the newly created recipe step or an integer `NA` in case of error.
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
#' recipe_uid <- add_recipe(connection, 'visit my website')
#' add_recipe_step(connection, recipe_uid, 'navigate', 'https://www.google.com/')
#' add_recipe_step(connection, recipe_uid, 'screenshot')
#' disconnect(connection)
#' }
#'
#' @seealso [add_recipe()], [get_recipes()], [get_recipe_steps()]
#' @importFrom magrittr %>%
#' @export

add_recipe_step <- function(connection,
                            recipe_uid,
                            type = 'log',
                            fixed_value = '',
                            random_items = c(),
                            use_data_item_instead_of_value = FALSE,
                            active = TRUE,
                            sort = NA_integer_) {

  # Test input
  if(is.null(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect_scrapebot.')
  }
  if(!is.character(type) | !type %in% c('navigate',
                                        'find_by_id',
                                        'find_by_name',
                                        'find_by_class',
                                        'find_by_tag',
                                        'find_by_link',
                                        'find_by_link_partial',
                                        'find_by_css',
                                        'find_by_xpath',
                                        'random_select',
                                        'scroll_to',
                                        'pause',
                                        'click',
                                        'write',
                                        'write_slowly',
                                        'submit',
                                        'get_text',
                                        'get_texts',
                                        'get_value',
                                        'get_values',
                                        'get_attribute',
                                        'get_attributes',
                                        'get_pagetitle',
                                        'get_element_count',
                                        'get_htmlsource',
                                        'log',
                                        'data',
                                        'post_all_data',
                                        'post_previous_step_data',
                                        'execute_js',
                                        'go_back',
                                        'go_forward',
                                        'unset_prior_element',
                                        'screenshot',
                                        'sometimes_screenshot',
                                        'element_screenshot')) {

    stop('Type needs to be one of those allowed (see ??ScrapeBotR::add_recipe_step).')
  }
  if(use_data_item_instead_of_value & fixed_value == '') {
    stop('use_data_item_instead_of_value requires value to be set as well.')
  }
  recipes <- get_recipes(connection, include_inactive = T)
  if(is.null(recipe_uid) | !recipe_uid %in% recipes$uid) {
    stop('recipe_uid needs to be set to an existing recipe.')
  }

  tryCatch({
    step.created <- as.character(as.POSIXct(Sys.time()))
    step.type <- type
    step.value <- fixed_value
    step.active <- dplyr::if_else(active, 1, 0, missing = 0)
    step.sort <- as.integer(sort)
    steps.before <- get_recipe_steps(connection, recipe_uid, include_inactive = T)
    if(is.na(sort)) {
      step.sort <- nrow(steps.before) + 1
    }

    DBI::dbAppendTable(connection$db,
                       'recipestep',
                       data.frame(created = step.created,
                                  sort = step.sort,
                                  active = step.active,
                                  type = step.type,
                                  value = step.value,
                                  use_random_item_instead_of_value = dplyr::if_else(length(random_items) > 0,
                                                                                    1, 0, missing = 0),
                                  use_data_item_instead_of_value = use_data_item_instead_of_value,
                                  recipe_uid = recipe_uid))

    step <-
      get_recipe_steps(connection, recipe_uid, include_inactive = T) %>%
      dplyr::filter(active == as.logical(step.active),
                    type == step.type,
                    !uid %in% steps.before$uid)

    if(nrow(step) > 0) {
      step.uid <- as.integer(step[[1, 'uid']])
      if(length(random_items) > 0) {
        for(random_item in random_items) {
          DBI::dbAppendTable(connection$db,
                             'recipestepitem',
                             data.frame(created = step.created,
                                        value = random_item,
                                        step_uid = step.uid))
        }
      }
      return(step.uid)
    }
  }, error = function(e) {
    warning(paste0('Creating a new recipe step resulted in an ', e))
    return(NA_integer_)
  })
}

#' Add a new instance to the database
#'
#' Adds a newly created instance to the database. This does mean launching a server (see [aws_launch_instance()]).
#'
#' @param connection A connection object, as retrieved from [connect_scrapebot()].
#' @param name Character string identifying the instance.
#' @param instance_owner The email address of the ScrapeBot user who will be the owner of the new instance as character string. If this one does not exist, it will be created (in this case, a text will be raised).
#' @param description Character string to describe the instance even further; default is an empty string.
#'
#' @return The `uid` (integer) of the newly created instance or an integer `NA` in case of error.
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
#' add_instance(connection, 'local machine', 'mario@haim.it')
#' disconnect(connection)
#' }
#'
#' @seealso [get_instances()], [aws_launch_instance()]
#' @importFrom magrittr %>%
#' @export

add_instance <- function(connection, name, instance_owner, description = '') {

  # Test input
  if(is.null(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect_scrapebot.')
  }
  if(!is.character(name)) {
    stop('Name needs to be set as character string.')
  }

  tryCatch({
    instances.before <- get_instances(connection)

    instance.created <- as.character(as.POSIXct(Sys.time()))
    instance.name <- stringr::str_trim(name)
    instance.description <- stringr::str_trim(as.character(description))

    DBI::dbAppendTable(connection$db,
                       'instance',
                       data.frame(name = instance.name,
                                  created = instance.created,
                                  description = instance.description,
                                  owner_uid = get_or_create_user(connection,
                                                                 instance_owner)))

    instance <-
      get_instances(connection) %>%
      dplyr::filter(name == instance.name,
                    !uid %in% instances.before$uid) %>%
      dplyr::arrange(dplyr::desc(created))

    if(nrow(instance) > 0) {
      return(as.integer(instance[[1, 'uid']]))
    }
  }, error = function(e) {
    warning(paste0('Creating a new instance resulted in an ', e))
    return(NA_integer_)
  })
}

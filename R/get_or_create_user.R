#' Based on a given ScrapeBot user's email address, get this user's `UID` (or create it first)
#'
#' @param connection A ScrapeBot connection object, as retrieved from [connect_scrapebot()].
#' @param email Character string with the email address to be used for the user.
#'
#' @return The `UID` of the (newly created or found) user (as integer). If a user is created, a text will be shown with the generated password.
#'
#' @examples
#' \dontrun{
#'
#' connection <- connect('my_db on localhost')
#' get_or_create_user(connection)
#' }
#'
#' @importFrom magrittr %>%
#' @export

get_or_create_user <- function(connection, email) {

  # Test input
  if(is.null(connection$db)) {
    stop('Connection needs to be a valid connection object, initiated through ScrapeBotR::connect_scrapebot.')
  }
  if(!is.character(email)) {
    stop('Email address needs to be set as character string.')
  } else {
    email <- stringr::str_trim(stringr::str_to_lower(email))
  }

  # find the user and return their UID
  # if not available, create the user, warn with the password and return their UID
  uid_tibble <-
    DBI::dbGetQuery(connection$db,
                    paste0('SELECT uid FROM `user` WHERE `email` = ',
                           DBI::dbQuoteString(connection$db,
                                              email),
                           ' AND active LIMIT 1')) %>%
    tibble::as_tibble()

  if (nrow(uid_tibble) == 1) {
    return(as.integer(uid_tibble[[1, 'uid']]))
  } else {
    password <- paste0(sample(c(sample(letters, 8, T),
                                sample(LETTERS, 8, T),
                                sample(0:9, 8, T)),
                              16),
                      collapse = '')
    salt <- paste0(sample(c(sample(letters, 8, T),
                            sample(LETTERS, 8, T),
                            sample(0:9, 8, T)),
                          8),
                   collapse = '')
    hash <- paste0('sha512$',
                   salt, '$',
                   openssl::sha512(password,
                                   key = salt))
    DBI::dbAppendTable(connection$db,
                       'user',
                       data.frame(created = as.character(as.POSIXct(Sys.time())),
                                  email = email,
                                  name = email,
                                  password = hash,
                                  active = 1))
    cat(paste0('ScrapeBot user ', email, ' created with password ', password))
    return(get_or_create_user(connection, email))
  }
}

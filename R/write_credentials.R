#' Write ScrapeBot database credentials to INI file
#'
#' You should not use connection functionality by regularly publishing credentials in the console. While credentials will end up in the console log, you also risk pushing scripts including credentials to any sustainable repositories (e.g., Git).
#' Instead, you should prepare an INI file which will then be read/used by the connection functionality.
#'
#' This function is a helper function to create this INI file for you.
#' While creating the INI file like so will still end up pushing your credentials to the console log once,
#' but it will not require you to do that ever again, thus keeping credentials off your repositories.
#' The function pushes the credentials into an INI-file section called "\code{database} on \code{host}" (e.g., "scrapebot on localhost").
#'
#' Alternatively, you can absolutely create this INI file yourself. For this, include a file called \code{.scrapebot.ini} (yes, including the leading dot) in your working directory.
#' This file then needs a section which is indicated in its own line, looking like \code{[my section]}.
#' Below this line (and thus, within this section) you can specify the following parameters to connect to your database:
#' - host (for example, \code{host=localhost})
#' - port (for example, \code{port=3307})
#' - user (for example, \code{user=my_personal_user})
#' - password (for example, \code{password=abcd3.45d:cba!})
#' - database (for example, \code{database=scrapebot})
#'
#' @param host The central database host to connect to as character string.
#' @param user The username to connect to the central database as character string.
#' @param password According password as character string.
#' @param database Represents the database name as character string. Defaults to `scrapebot`.
#' @param port The port number to connect to (only used if greater than 0; not needed for MySQL if 3306). Defaults to `0`.
#'
#' @return The file name of the newly created credentials file, if successful.
#'
#' @examples
#' write_scrapebot_credentials('localhost', 'root', 'my_password')
#' write_scrapebot_credentials(':memory:')
#'
#' @seealso [connect_scrapebot()] to use the credential file in practice
#'
#' @export

write_scrapebot_credentials <- function(host, user = NULL, password = NULL, database = 'scrapebot', port = 0) {

  # Test input
  if(is.null(host)) {
    stop('Host needs to be a character string.')
  }
  if(is.null(database)) {
    stop('Database needs to be a character string.')
  }

  # Set up central indices
  filename <-  path.expand('~/.scrapebot.ini')
  config_index <- paste0(database, ' on ', host)
  config <- list()
  config[[config_index]] <- list()

  # Check if a previous config file exists
  tryCatch({
    previous_config <- configr::read.config(filename)
    if(is.list(previous_config)) {
      config <- previous_config
      if(!config_index %in% names(previous_config)) {
        config[[config_index]] <- list()
      }
      warning('Previous credential file exists; information has been integrated.')
    }
  }, warning = function(w) {})

  # Add credentials and everything
  config[[config_index]][['host']] <- host
  config[[config_index]][['database']] <- database
  if(!is.null(user)) {
    config[[config_index]][['user']] <- user
  }
  if(!is.null(password)) {
    config[[config_index]][['password']] <- password
  }
  if(!is.null(port) & port > 0) {
    config[[config_index]][['port']] <- port
  }

  # Write config file
  if(configr::write.config(config, filename, write.type = 'ini')) {
    return(filename)
  } else {
    stop(paste0('An error might have occured. Check file ', filename, ' or try again.'))
  }
}



#' Write AWS credentials to INI file
#'
#' If you want to orchestrate ScrapeBot on AWS (Amazon Web Services), you need an AWS account.
#' This AWS account then needs an IAM access (via http://console.aws.amazon.com/iam) with an API access key.
#' In addition, launched servers on AWS will be fed your SSH public key to access them.
#' To create a SSH keypair, use the AWS EC2 console (https://console.aws.amazon.com/ec2/) and "Network & Society."
#'
#' That said, you should not use connection functionality by regularly publishing credentials in the console. While credentials will end up in the console log, you also risk pushing scripts including credentials to any sustainable repositories (e.g., Git).
#' Instead, you should prepare an INI file which will then be read/used by the connection functionality.
#'
#' This function is a helper function to create this INI file for you.
#' While creating the INI file like so will still end up pushing your credentials to the console log once,
#' but it will not require you to do that ever again, thus keeping credentials off your repositories.
#' The function pushes the credentials into an INI-file section called "AWS".
#'
#' Alternatively, you can absolutely create this INI file yourself. For this, include a file called \code{.scrapebot.ini} (yes, including the leading dot) in your working directory.
#' This file then needs a section which is indicated in its own line, looking like \code{[my section]}.
#' Below this line (and thus, within this section) you can specify the following parameters to log into your AWS:
#' - access_key_id (for example, \code{access_key_id=ABCD0EF1GH2IJ3KL})
#' - secret_access_key (for example, \code{secret_access_key=fkdusbl+sli725imfn26fks9})
#' - ssh_private_pem_file (for example, \code{my_aws_key_private.pem})
#' - ssh_public_pem_file (for example, \code{my_aws_key_public.pem})
#'
#' @param access_key_id The AWS user's access key ID (via IAM) as character string.
#' @param secret_access_key The corresponding secret as character string.
#' @param ssh_private_pem_file The path/file to your private SSH key in .pem format as character string.
#' @param ssh_public_pem_file The path/file to the corresponding public SSH key in .pem format as character string.
#'
#' @return The file name of the newly created credentials file, if successful.
#'
#' @examples
#' \dontrun{
#'
#' write_aws_credentials(
#'   'ABCD0EF1GH2IJ3KL',
#'   'fkdusbl+sli725imfn26fks9',
#'   'my_aws_key_private.pem',
#'   'my_aws_key_public.pem'
#' )
#' }
#'
#' @seealso [aws_launch_instance()] to use the credential file in practice
#'
#' @export

write_aws_credentials <- function(access_key_id, secret_access_key, ssh_private_pem_file, ssh_public_pem_file) {

  # Test input
  if(is.null(access_key_id)) {
    stop('Access key ID (generate via AWS > IAM) needs to be a character string.')
  }
  if(is.null(secret_access_key)) {
    stop('Secret access key (generate via AWS > IAM) needs to be a character string.')
  }
  if(is.null(ssh_private_pem_file)) {
    stop('SSH private key file needs to be a character string.')
  } else {
    if(!file.exists(ssh_private_pem_file)) {
      stop('SSH private key file cannot be found.')
    }
  }
  if(is.null(ssh_public_pem_file)) {
    stop('SSH public key file needs to be a character string.')
  } else {
    if(!file.exists(ssh_public_pem_file)) {
      stop('SSH public key file cannot be found.')
    }
  }

  # Set up central indices
  filename <-  path.expand('~/.scrapebot.ini')
  config_index <- 'AWS'
  config <- list()
  config[[config_index]] <- list()

  # Check if a previous config file exists
  tryCatch({
    previous_config <- configr::read.config(filename)
    if(is.list(previous_config)) {
      config <- previous_config
      if(!config_index %in% names(previous_config)) {
        config[[config_index]] <- list()
      }
      warning('Previous credential file exists; information has been integrated.')
    }
  }, warning = function(w) {})

  # Add credentials and everything
  config[[config_index]][['access_key_id']] <- access_key_id
  config[[config_index]][['secret_access_key']] <- secret_access_key
  config[[config_index]][['ssh_public']] <- ssh_public_pem_file
  config[[config_index]][['ssh_private']] <- ssh_private_pem_file

  # Write config file
  if(configr::write.config(config, filename, write.type = 'ini')) {
    return(filename)
  } else {
    stop(paste0('An error might have occured. Check file ', filename, ' or try again.'))
  }
}

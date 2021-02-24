#' Connect to ScrapeBot database
#'
#' Connect to a ScrapeBot central database as typically provided during the installation process of a ScrapeBot infrastructure.
#' Uses RMariaDB to connect to the MySQL server.
#'
#' In order not to use database credentials here, you need to set up an INI file containing your database credentials. You can do this by hand (see [write_scrapebot_credentials()]) or use the [write_scrapebot_credentials()] function.
#'
#' @param credentials_section The section within your INI file holding the credentials. If [write_scrapebot_credentials()] was used to create the INI file, then the section is called "\code{database} on \code{host}" (e.g., "scrapebot on localhost").
#' @param credentials_file The INI file to use. If [write_scrapebot_credentials()] was used to create the INI file, then it is called \code{~/.scrapebot.ini} (the default). Note that the file must (!) be located inside your home directory (i.e., somewhere inside \code{~} as this is a required by <https://mariadb.com/kb/en/configuring-mariadb-with-option-files/>).
#'
#' @return A connection object (i.e., a specified list), ready to be passed to other [ScrapeBotR] functions.
#'
#' @examples
#' \dontrun{
#'
#' connect_scrapebot('my_db on localhost')
#' connect_scrapebot('my_db on localhost', '~/my_own_credentials_file.ini')
#' }
#'
#' @seealso [write_scrapebot_credentials()] to help create the credentials file, [disconnect()] to close a database connection, <https://github.com/MarHai/ScrapeBot> for more information on the central database.
#'
#' @export

connect_scrapebot <- function(credentials_section, credentials_file = '~/.scrapebot.ini') {

  # Test input
  if(!is.character(credentials_section)) {
    stop('Credential section needs to be a character string.')
  }
  if(!is.character(credentials_file)) {
    stop('Credential file needs to be a character string.')
  } else {
    credentials_file <- path.expand(credentials_file)
  }

  # Try connecting to the database
  tryCatch({
    maria <- DBI::dbConnect(RMariaDB::MariaDB(),
                            default.file = credentials_file,
                            groups = credentials_section)
  }, error = function(e) {
    stop(paste0('Database connection could not be established. ', e))
  })

  # Collection ScrapeBot installation information
  tryCatch({
    timeout <- DBI::dbGetQuery(maria, 'SHOW SESSION VARIABLES LIKE "wait_timeout"', n = 1)[[1, 'Value']]
    db_info <- DBI::dbGetInfo(maria)
  }, error = function(e) {
    stop(paste0('Database did not return as expected when requesting wait_timeout. ', e))
  })

  # Set up connection object (a named list)
  connection <- list(
    db = maria,
    db_type = db_info$con.type,
    db_version = db_info$db.version,
    db_timeout = as.integer(timeout),
    credentials_file = credentials_file,
    credentials_section = credentials_section,
    tables = DBI::dbListTables(maria)
  )

  return(connection)
}



#' Connect to Amazon Web Services (AWS) for instances (EC2), databases (RDS), and storage (S3)
#'
#' Connect to AWS to set up instances (EC2), databases (RDS), and storage (S3). Only necessary if you want to orchestrate instances through R. Requires IAM account (in credentials_file/credentials_section) to have permissions for "AmazonEC2FullAccess," "AmazonRDSFullAccess, and "AmazonS3FullAccess."
#' In order not to use credentials here, you need to set up an INI file containing your AWS IAM credentials. You can do this by hand (see [write_aws_credentials()]) or use the [write_aws_credentials()] function.
#'
#' AWS works in regions. As such, you need to specify the region here in which all subsequent actions will take place. You AWS user might not have all regions enabled. Find out more about it in your account settings (https://console.aws.amazon.com/billing/home?#/account).
#'
#' @param region The AWS region to connect to (e.g., "eu-central-1" for Frankfurt) as character.
#' @param credentials_section The section within your INI file holding the credentials. If [write_aws_credentials()] was used to create the INI file, then the section is called "AWS".
#' @param credentials_file The INI file to use. If [write_aws_credentials()] was used to create the INI file, then it is called \code{~/.scrapebot.ini} (the default).
#'
#' @return An AWS connection object (i.e., a specified list), ready to be passed to other [ScrapeBotR] functions.
#'
#' @examples
#' \dontrun{
#'
#' connect_aws()
#' connect_aws('us-east-2')
#' connect_aws(NULL, 'my AWS creds', '~/my_own_credentials_file.ini')
#' }
#'
#' @seealso [write_aws_credentials()] to help create the credentials file
#'
#' @export

connect_aws <- function(region = 'eu-central-1',
                        credentials_section = 'AWS',
                        credentials_file = '~/.scrapebot.ini') {

  # Test input
  if(!is.character(credentials_section)) {
    stop('Credential section needs to be a character string.')
  }
  if(!is.character(credentials_file)) {
    stop('Credential file needs to be a character string.')
  } else {
    credentials_file <- path.expand(credentials_file)
  }
  if(!is.character(region)) {
    stop('Region needs to be a character string.')
  }

  # Set up AWS object (a named list)
  aws_connection <- list(
    region = region,
    ec2 = NULL,
    rds = NULL,
    s3 = NULL,
    ec2_instances = tibble::tibble(instance_scrapebot_uid = integer(),
                                   instance_aws_id = character(),
                                   host = character(),
                                   created = as.POSIXct(character()),
                                   instance_type = character(),
                                   instance_username = character(),
                                   instance_region = character(),
                                   browser_useragent = character(),
                                   browser_language = character(),
                                   browser_width = character(),
                                   browser_height = character()),
    s3_bucket = NA_character_,
    rds_credential_section = NA_character_,
    rds_identifier = NA_character_,
    keypair = paste0('ScrapeBotR',
                     credentials_section),
    security_group_instance = NA_character_,
    security_group_database = NA_character_,
    aws_access_key = NA_character_,
    aws_secret = NA_character_,
    ssh_public_file = NA_character_,
    ssh_private_file = NA_character_,
    credentials_file = credentials_file,
    credentials_section = credentials_section
  )

  # Try reading the credentials file
  tryCatch({
    config <- configr::read.config(credentials_file)
    if(is.list(config)) {
      if(is.list(config[[credentials_section]])) {
        aws_connection$aws_access_key <- config[[credentials_section]]$access_key_id
        aws_connection$aws_secret <- config[[credentials_section]]$secret_access_key
        aws_connection$ssh_public_file <- config[[credentials_section]]$ssh_public
        aws_connection$ssh_private_file <- config[[credentials_section]]$ssh_private
      }
    }
  }, error = function(e) {
    stop(paste0('Credentials or SSH file could not be read. ', e))
  })

  # Try connecting to EC2
  tryCatch({
    aws_connection$ec2 <- paws::ec2(config = list(credentials = list(creds = list(access_key_id = aws_connection$aws_access_key,
                                                                                  secret_access_key = aws_connection$aws_secret)),
                                                  region = aws_connection$region))
    aws_deploy_ssh_to_ec2(aws_connection)
  }, error = function(e) {
    warning(paste0('EC2 (instance) connection could not be properly established: ', e))
    aws_connection$ec2 <- NULL
  })

  # Try connecting to RDS
  tryCatch({
    aws_connection$rds <- paws::rds(config = list(credentials = list(creds = list(access_key_id = aws_connection$aws_access_key,
                                                                                  secret_access_key = aws_connection$aws_secret)),
                                                  region = aws_connection$region))
  }, error = function(e) {
    warning(paste0('RDS (database) connection could not be properly established: ', e))
    aws_connection$rds <- NULL
  })

  # Try connecting to S3
  tryCatch({
    aws_connection$s3 <- paws::s3(config = list(credentials = list(creds = list(access_key_id = aws_connection$aws_access_key,
                                                                                secret_access_key = aws_connection$aws_secret)),
                                                region = aws_connection$region))
  }, error = function(e) {
    warning(paste0('S3 (storage) connection could not be properly established: ', e))
    aws_connection$s3 <- NULL
  })

  return(aws_connection)
}


#' Change the current region in your Amazon Web Services (AWS)
#'
#' AWS works in regions. As such, you need to specify the region when connecting for all subsequent actions. You AWS user might not have all regions enabled. Find out more about it in your account settings (https://console.aws.amazon.com/billing/home?#/account).
#'
#' @param aws_connection AWS connection object, as retrieved from [connect_aws()]. Holds the current region.
#' @param new_region The new AWS region to connect to (e.g., "eu-central-1" for Frankfurt) as character.
#'
#' @return The updated AWS connection object.
#'
#' @examples
#' \dontrun{
#'
#' aws_connection <- connect_aws()
#' change_aws_region(aws_connection, 'us-east-2')
#' }
#'
#' @seealso [connect_aws()]
#'
#' @export

change_aws_region <- function(aws_connection, new_region = 'eu-central-1') {

  # Test input
  if(is.null(aws_connection$ec2)) {
    stop('AWS connection needs to be set up with EC2, initiated through ScrapeBotR::connect_aws.')
  }
  if(!is.character(new_region)) {
    stop('Region needs to be a character string.')
  }

  # Set new region
  aws_connection$region <- new_region
  aws_connection$ec2 <- NULL
  aws_connection$rds <- NULL
  aws_connection$s3 <- NULL

  # Try connecting to EC2
  tryCatch({
    aws_connection$ec2 <- paws::ec2(config = list(credentials = list(creds = list(access_key_id = aws_connection$aws_access_key,
                                                                                  secret_access_key = aws_connection$aws_secret)),
                                                  region = aws_connection$region))
    aws_deploy_ssh_to_ec2(aws_connection)
  }, error = function(e) {
    warning(paste0('EC2 (instance) connection could not be properly established: ', e))
    aws_connection$ec2 <- NULL
  })

  # Try connecting to RDS
  tryCatch({
    aws_connection$rds <- paws::rds(config = list(credentials = list(creds = list(access_key_id = aws_connection$aws_access_key,
                                                                                  secret_access_key = aws_connection$aws_secret)),
                                                  region = aws_connection$region))
  }, error = function(e) {
    warning(paste0('RDS (database) connection could not be properly established: ', e))
    aws_connection$rds <- NULL
  })

  # Try connecting to S3
  tryCatch({
    aws_connection$s3 <- paws::s3(config = list(credentials = list(creds = list(access_key_id = aws_connection$aws_access_key,
                                                                                secret_access_key = aws_connection$aws_secret)),
                                                region = aws_connection$region))
  }, error = function(e) {
    warning(paste0('S3 (storage) connection could not be properly established: ', e))
    aws_connection$s3 <- NULL
  })

  return(aws_connection)
}



#' Deploy a public SSH key to an AWS EC2 instance.
#'
#' Checks if a keypair with a name compiled from the credential_section and the prefix "ScrapeBotR" (e.g., "ScrapeBotR AWS") exists.
#' If not, creates a new keypair and imports the public SSH key as provided in [write_aws_credentials()].
#'
#' @param aws_connection AWS connection object, as retrieved from [connect_aws()].
#'
#' @examples
#' \dontrun{
#'
#' aws_deploy_ssh_to_ec2(aws_connection)
#' }
#'
#' @seealso [connect_aws()]

aws_deploy_ssh_to_ec2 <- function(aws_connection) {

  # Test input
  if(is.null(aws_connection$ec2)) {
    stop('No AWS connection established for EC2. Try connect_aws first.')
  }

  tryCatch(aws_connection$ec2$describe_key_pairs(KeyNames = list(aws_connection$keypair)),
           error = function(e) {
             aws_connection$ec2$import_key_pair(KeyName = aws_connection$keypair,
                                                PublicKeyMaterial = aws_connection$ssh_public_file)
            })
}

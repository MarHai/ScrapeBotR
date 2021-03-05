#' Get a default image name for a given region
#'
#' AWS works in regions. And just like instances, also Amazon Machine Images are customized per region (e.g., timezone, language ...).
#' This function serves as helping dictionary in that it provides you with the AMI image name for a given image.
#' The image returned here is the one for an Ubuntu Server 20.04 LTS with SSD volume (64-bit x86, if available).
#' This has proven to work well with both AWS and ScrapeBot.
#' It is also eligible for the free tier.
#'
#' @param region The AWS region to fetch the image name for (e.g., "eu-central-1" for Frankfurt/Europe) as character.
#'
#' @return AWS Amazon Machine Image (AMI) name as character string or (character) \code{NA} if region unknown.
#'
#' @examples
#'
#' aws_default_ec2_image()
#' aws_default_ec2_image('eu-central-1')
#'
#' @seealso [connect_aws()], [aws_launch_instance()]
#' @export

aws_default_ec2_image <- function(region = 'eu-central-1') {
  return(dplyr::case_when(region == 'af-south-1' ~ 'ami-0081edcfb10f9f0d6',
                          region == 'ap-east-1' ~ 'ami-0774445f9e6290ccd',
                          region == 'ap-northeast-1' ~ 'ami-059b6d3840b03d6dd',
                          region == 'ap-northeast-2' ~ 'ami-00f1068284b9eca92',
                          region == 'ap-northeast-3' ~ 'ami-01ecbd21b1e9b987f',
                          region == 'ap-south-1' ~ 'ami-0d758c1134823146a',
                          region == 'ap-southeast-1' ~ 'ami-01581ffba3821cdf3',
                          region == 'ap-southeast-2' ~ 'ami-0a43280cfb87ffdba',
                          region == 'ca-central-1' ~ 'ami-043e33039f1a50a56',
                          region == 'eu-central-1' ~ 'ami-0767046d1677be5a0',
                          region == 'eu-north-1' ~ 'ami-0ed17ff3d78e74700',
                          region == 'eu-south-1' ~ 'ami-08a7e27b95390cc06',
                          region == 'eu-west-1' ~ 'ami-08bac620dc84221eb',
                          region == 'eu-west-2' ~ 'ami-096cb92bb3580c759',
                          region == 'eu-west-3' ~ 'ami-0d6aecf0f0425f42a',
                          region == 'me-south-1' ~ 'ami-07d42d0c2a45aa449',
                          region == 'sa-east-1' ~ 'ami-0b9517e2052e8be7a',
                          region == 'us-east-1' ~ 'ami-042e8287309f5df03',
                          region == 'us-east-2' ~ 'ami-08962a4068733a2b6',
                          region == 'us-west-1' ~ 'ami-031b673f443c2172c',
                          region == 'us-west-2' ~ 'ami-0ca5c3bd5a268e7db',
                          T ~ NA_character_))
}


#' Launch a new ScrapeBot instance as AWS EC2 instance
#'
#' Note that this could cause costs, depending on your free tier and the chosen ec2_type.
#' Also, note that as this function waits for a new instance to launch and reboot, it might take a couple of seconds.
#'
#' This function follows the suggested ScrapeBot behavior in doing six things:
#' 1. Launch (i.e., create and run) one new EC2 instance in the region specified in [connect_aws()]
#'    - this also means to create a new security group that allows SSH traffic from anywhere
#'    - afterwards, an SSH connection is established with the new instance
#' 2. Update the available package-manager repositories and install requirements (python3, firefox, xvfb, git)
#' 3. Get (i.e., clone) the latest version of ScrapeBot
#' 4. Provide Firefox's Geckodriver with execution rights
#' 5. Install Python requirements
#' 6. Setup the newly created ScrapeBot instance
#'    - register the EC2 instance as ScrapeBot instance in the central database
#'    - set up the ScrapeBot instance with the specified database and S3 settings
#'    - set up a cronjob to run the ScrapeBot every 2 minutes
#'
#' As this function is aimed at minimizing efforts in setting up a ScrapeBot instance, not all details as available within AWS can be modified here. Specify, however:
#' - The type of server machine you want to run. \code{t2.micro}, the default, qualifies for AWS' cost-free option. Requirements vary with what you intend to do with your ScrapeBot instance.
#' - The Amazon Machine Image (AMI) or operating system you want to run. The default is to ask the [aws_default_ec2_image()] function.
#' - The user agent string, an identifying header sent with a web request to help the website identify what system it is interacting with
#' - The browser language which nudges various websites to change their layout/language
#' - The browser width and height, which helps to emulate also mobile devices (together with the user agent string)
#' - The email address of the ScrapeBot user (for the web frontend) to be associated with the new instance
#'
#' This function then automatically chooses the following settings:
#' - A security group is either created or re-used, allowing incoming SSH traffic (TCP via port 22) from anywhere.
#' - firefox is chosen as the package/browser of choice, emulated through xvfb (as suggested by ScrapeBot).
#'
#' @param aws_connection AWS connection object, as retrieved from [connect_aws()]. This also specifies the region.
#' @param instance_owner The email address of the ScrapeBot user who will be the owner of the new instance as character string. If this one does not exist, it will be created (in this case, a text will be raised).
#' @param scrapebot_credential_section The section within your INI file holding the credentials to the ScrapeBot central database as character string. Default is to use the one set by [aws_launch_database()] into the aws_connection object.
#' @param ec2_type AWS instance type. The default, \code{t2.micro}, qualifies for the free tier. Variuos \code{t3} types have also proven useful but are connected with costs.
#' @param ec2_image AWS Amazon Machine Image (AMI) to use. Default is \code{NA} which translates to using a region's default image via [aws_default_ec2_image()].
#' @param ec2_image_username The username to log into the respective \code{ec2_image}. for Ubuntu images on AWS, this is \code{ubuntu}.
#' @param browser_useragent The emulated browser's user agent to send to requested websites. Default is a recent Firefox Desktop used under Ubuntu Linux. Will be deployed into ScrapeBot config file.
#' @param browser_language Language to which emulated browser is set. Default is German German. Will be deployed into ScrapeBot config file.
#' @param browser_width Width of the emulated browser in pixels. Default is a recent desktop monitor size. Will be deployed into ScrapeBot config file.
#' @param browser_height Height of the emulated browser in pixels. Default is a recent desktop monitor size. Will be deployed into ScrapeBot config file.
#'
#' @return The updated AWS connection object which get an ec2_instance attached in the respective tibble (keep/store this to later terminate the instance also).
#'
#' @examples
#' \dontrun{
#'
#' aws_connection <- connect_aws()
#' scrapebot_connection <- connect('my_db on localhost')
#' #t3.large:
#' #- not cost-free
#' #- 2 virtual CPUs
#' #- 8GiB RAM
#' #- up to 5GBit of down-/uplink speed
#' aws_launch_instance(aws_connection, scrapebot_connection, 'mario@haim.it', ec_type = 't3.large')
#' }
#'
#' @seealso [connect_aws()], [aws_default_ec2_image()], [get_or_create_user()], [aws_terminate_instance()]
#' @importFrom magrittr %>%
#' @export

aws_launch_instance <- function(aws_connection,
                                instance_owner,
                                scrapebot_credential_section = aws_connection$rds_credential_section,
                                ec2_type = 't2.micro',
                                ec2_image = NA_character_,
                                ec2_image_username = 'ubuntu',
                                browser_useragent = 'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:85.0) Gecko/20100101 Firefox/85.0',
                                browser_language = 'de-de',
                                browser_width = 1920,
                                browser_height = 1080) {
  # Test input
  if(is.null(aws_connection$ec2)) {
    stop('AWS connection needs to be set up with EC2, initiated through ScrapeBotR::connect_aws.')
  }
  if(!is.character(scrapebot_credential_section)) {
    stop('Credential section for connect_scrapebot() needs to be given as character string.')
  }
  if(!is.character(instance_owner)) {
    stop('A new ScrapeBot instance requires an owner, provided as email address character string in instance_owner.')
  }
  if(is.na(ec2_image)) {
    ec2_image <- aws_default_ec2_image(aws_connection$region)
  }
  if(file.access(aws_connection$ssh_private_file,
                 mode = 4) < 0) {
    stop('SSH private key file not readable.')
  }

  # create security group (if necessary)
  instance_security_group_name <- 'ScrapeBotR'
  tryCatch({
    instance_security_group <- aws_connection$ec2$describe_security_groups(GroupNames = list(instance_security_group_name))
    aws_connection$security_group_instance <- instance_security_group$SecurityGroups[[1]]$GroupId
    },
    error = function(e) {
      instance_security_group.creation <- aws_connection$ec2$create_security_group(GroupName = instance_security_group_name,
                                                                                   Description = 'automatically generated from R')
      aws_connection$security_group_instance <- instance_security_group.creation$GroupId
      aws_connection$ec2$authorize_security_group_ingress(CidrIp = '0.0.0.0/0',
                                                          FromPort = 22,
                                                          ToPort = 22,
                                                          GroupId = aws_connection$security_group_instance,
                                                          IpProtocol = 'tcp')
      })

  # launch a new instance
  instance <- aws_connection$ec2$run_instances(ImageId = ec2_image,
                                               InstanceType = ec2_type,
                                               KeyName = aws_connection$keypair,
                                               MinCount = 1,
                                               MaxCount = 1,
                                               SecurityGroupIds = list(aws_connection$security_group_instance))

  # wait for instance to boot
  instance.host <- ''
  while (instance.host == '') {
    Sys.sleep(2)
    instance.status <- aws_connection$ec2$describe_instances(InstanceIds = instance$Instances[[1]]$InstanceId)
    tryCatch({
      instance.host <- instance.status$Reservations[[1]]$Instances[[1]]$PublicDnsName
      instance.state <- instance.status$Reservations[[1]]$Instances[[1]]$State$Name
      if (length(instance.host) == 0) {
        instance.host <- ''
      } else if(stringr::str_length(instance.host) < 20 | is.null(instance.host) | instance.state != 'running') {
        instance.host <- ''
      }
    }, error = function(e) {
      instance.host <- ''
    })
  }

  # creation datetime and instance id
  instance.id <- instance$Instances[[1]]$InstanceId
  instance.created <- instance$Instances[[1]]$LaunchTime

  # SSH connection
  instance.ssh <- NULL
  instance.ssh.tries <- 0
  while(is.null(instance.ssh) & instance.ssh.tries < 5) {
    tryCatch(instance.ssh <- ssh::ssh_connect(host = paste0(ec2_image_username, '@', instance.host),
                                              keyfile = aws_connection$ssh_private_file,
                                              verbose = F),
             error = function(e) {
               instance.ssh <- NULL
             })
    instance.ssh.tries <- instance.ssh.tries + 1
  }
  if(is.null(instance.ssh)) {
    stop(paste0('New EC2 instance launched (', instance.host, ') but SSH connection failed.\n  ',
                'Use AWS web interface (https://console.aws.amazon.com/ec2/) to terminate any unused instances and try again.'))
  }

  # send SSH commands to set up ScrapeBot instance
  for(instance.ssh.cmd in c('sudo apt-get update',
                            'sudo apt-get update',
                            'sudo dpkg --configure -a',
                            'sudo apt-get -yfq install',
                            'sudo apt-get -yfq upgrade',
                            'sudo apt-get -yfq dist-upgrade')) {
    instance.ssh.log <- ssh::ssh_exec_internal(instance.ssh,
                                               command = instance.ssh.cmd,
                                               error = F)
    if(instance.ssh.log$status != 0) {
      warning(paste0('New EC2 instance (', instance.host, ') showed some minor errors during setup:\n  ',
                     'C: ', instance.ssh.cmd, '\n  ',
                     rawToChar(instance.ssh.log$stderr)))
    }
  }

  try(ssh::ssh_disconnect(instance.ssh),
      silent = T)
  aws_connection$ec2$reboot_instances(InstanceIds = list(instance$Instances[[1]]$InstanceId))
  # wait for instance to reboot
  Sys.sleep(10)

  # re-connect via SSH
  instance.ssh2 <- NULL
  instance.ssh2.tries <- 0
  while(is.null(instance.ssh2) & instance.ssh2.tries < 10) {
    tryCatch(instance.ssh2 <- ssh::ssh_connect(host = paste0(ec2_image_username, '@', instance.host),
                                               keyfile = aws_connection$ssh_private_file,
                                               verbose = F),
             error = function(e) {
               instance.ssh2 <- NULL
               Sys.sleep(5)
             })
    instance.ssh2.tries <- instance.ssh2.tries + 1
  }
  if(is.null(instance.ssh2)) {
    stop(paste0('New EC2 instance launched (', instance.host, ') but SSH connection failed.\n  ',
                'Use AWS web interface (https://console.aws.amazon.com/ec2/) to terminate any unused instances and try again.'))
  }

  # continue setup
  for(instance.ssh.cmd in c('sudo apt-get -yfq install python3-pip firefox xvfb',
                            'git clone https://github.com/MarHai/ScrapeBot.git',
                            'chmod u+x ScrapeBot/lib/*',
                            'cd ScrapeBot/ && pip3 install -r requirements.txt')) {
    instance.ssh.log <- ssh::ssh_exec_internal(instance.ssh2,
                                               command = instance.ssh.cmd,
                                               error = F)
    if(instance.ssh.log$status != 0) {
      aws_terminate_instance(aws_connection,
                             instance.id)
      warning(paste0('New EC2 instance launched (', instance.host, ') but setup crashed:\n  ',
                     'C: ', instance.ssh.cmd, '\n  ',
                     rawToChar(instance.ssh.log$stderr), '\n  ',
                     'Instance has thus been terminated and a new instance will launched instead.'))
      return(aws_launch_instance(aws_connection,
                                 instance_owner,
                                 scrapebot_credential_section,
                                 ec2_type,
                                 browser_useragent,
                                 browser_language,
                                 browser_width,
                                 browser_height))
    }
  }

  # connect to ScrapeBot central database
  scrapebot_connection <- connect_scrapebot(scrapebot_credential_section)

  # register ScrapeBot instance with central database
  instances.uid <- add_instance(scrapebot_connection,
                                instance.host,
                                instance_owner,
                                paste0(browser_width, 'x', browser_height, ', ',
                                       browser_language, ', ',
                                       aws_connection$region, ', ',
                                       'launched via ScrapeBotR'))

  # create and upload config file
  scrapebot.config <- configr::read.config(scrapebot_connection$credentials_file)[[scrapebot_connection$credentials_section]]
  instance.file <- 'config.ini'
  instance.fileconn <- file(instance.file)
  writeLines(c('[Database]',
             paste0('host = ', scrapebot.config$host,
                    ifelse(is.null(scrapebot.config$port),
                           '',
                           paste0(':', scrapebot.config$port))),
             paste0('user = ', scrapebot.config$user),
             paste0('password = ', stringr::str_replace_all(scrapebot.config$password,
                                                            stringr::fixed('%'),
                                                            '%%')),
             paste0('database = ', scrapebot.config$database),
             paste0('timeout = ', scrapebot_connection$db_timeout),
             paste0('awsaccess = ', stringr::str_replace_all(aws_connection$aws_access_key,
                                                             stringr::fixed('%'),
                                                             '%%')),
             paste0('awssecret = ', stringr::str_replace_all(aws_connection$aws_secret,
                                                             stringr::fixed('%'),
                                                             '%%')),
             ifelse(is.na(aws_connection$s3_bucket),
                    '',
                    paste0('awsbucket = ', aws_connection$s3_bucket)),
             '',
             '[Instance]',
             paste0('name = ', instance.host),
             'timeout = 1',
             'browser = Firefox',
             paste0('browseruseragent = ', browser_useragent),
             paste0('browserlanguage = ', browser_language),
             paste0('browserwidth = ', browser_width),
             paste0('browserheight = ', browser_height),
             ''),
             instance.fileconn)
  close(instance.fileconn)
  disconnect(scrapebot_connection)

  # SSH-upload config file
  tryCatch({
    ssh::scp_upload(instance.ssh2,
                    instance.file,
                    to = '~/ScrapeBot/',
                    verbose = F)
    file.remove(instance.file)
  }, error = function(e) {
    stop(paste0('Config file could not be uploaded to newly launched EC2 instance (', instance.host, '). ',
                'Please connect manually to EC2 instance and run: python3 ~/ScrapeBot/setup.py'))
  })

  # initiate cronjob
  tryCatch({
    ssh::ssh_exec_internal(instance.ssh2,
                           command = c('crontab -l > cronjobs.temp',
                                       'echo "*/2 * * * * cd ~/ScrapeBot/ && python3 scrapebot.py >> scrapebot_cron.log" >> cronjobs.temp',
                                       'crontab cronjobs.temp',
                                       'rm -f cronjobs.temp'))
  }, error = function(e) {
    stop(paste0('Cronjob could not be started on newly launched EC2 instance (', instance.host, '). ',
                'Please connect manually to EC2 instance and python3 ~/ScrapeBot/setup.py'))
  })

  # disconnect from SSH
  try(ssh::ssh_disconnect(instance.ssh2),
      silent = T)

  # add instances to aws_connection
  aws_connection$ec2_instances <-
    aws_connection$ec2_instances %>%
    dplyr::bind_rows(tibble::tibble(instance_scrapebot_uid = instances.uid,
                                    instance_aws_id = instance.id,
                                    host = instance.host,
                                    created = instance.created,
                                    instance_type = ec2_type,
                                    instance_username = ec2_image_username,
                                    instance_region = aws_connection$region,
                                    browser_useragent = browser_useragent,
                                    browser_language = browser_language,
                                    browser_width = as.character(browser_width),
                                    browser_height = as.character(browser_height)))

  return(aws_connection)
}


#' Terminate an AWS EC2 instance
#'
#' Terminating an EC2 instance translates to shutting down and deleting it. That means, all data stored on that machine is gone.
#' This does not affect a central database, given that the central database is located on a different machine.
#' Note that AWS lists an instance after shutting it down for some 10-20 minutes longer. You won't be charged for such instances, though.
#'
#' @param aws_connection AWS connection object, as retrieved from [connect_aws()]. This also specifies the region.
#' @param instance_aws_uid Character string with the instance to be terminated. This has been put into the aws_connection$ec2_instances tibble.
#'
#' @return The updated AWS connection object.
#'
#' @examples
#' \dontrun{
#'
#' aws_connection <- connect_aws()
#' aws_terminate_instance(aws_connection, 'i-0a1b2c3d4e5f6g7h8i9j')
#' }
#'
#' @seealso [connect_aws()], [aws_launch_instance()]
#' @importFrom magrittr %>%
#' @export

aws_terminate_instance <- function(aws_connection, instance_aws_uid) {

  # Test input
  if(is.null(aws_connection$ec2)) {
    stop('AWS connection needs to be set up with EC2, initiated through ScrapeBotR::connect_aws.')
  }
  if(!is.character(instance_aws_uid)) {
    stop('AWS instance ID needs to be included and is provided in the ec2_instances tibble within the aws_connection if launched via ScrapeBotR.')
  }

  # Terminate
  aws_connection$ec2$terminate_instances(InstanceIds = instance_aws_uid)

  # Remove instance from aws_connection / ec2_instances
  aws_connection$ec2_instances <-
    aws_connection$ec2_instances %>%
    dplyr::filter(instance_aws_id != instance_aws_uid)

  return(aws_connection)
}




#' Launch a new storage as AWS S3 bucket
#'
#' The bucket gets created in the current region without particular security settings.
#' Its name is an automatically generated random character string.
#'
#' @param aws_connection AWS connection object, as retrieved from [connect_aws()]. This also specifies the region.
#'
#' @return The updated AWS connection object with s3_bucket set (keep/store this to later terminate the bucket also).
#'
#' @examples
#' \dontrun{
#'
#' aws_connection <- connect_aws()
#' aws_launch_storage(aws_connection)
#' }
#'
#' @seealso [connect_aws()], [aws_terminate_storage()]
#' @export

aws_launch_storage <- function(aws_connection) {

  # Test input
  if(is.null(aws_connection$s3)) {
    stop('AWS connection needs to be set up with S3, initiated through ScrapeBotR::connect_aws.')
  }

  # S3 name
  s3_bucket = paste0(c('s3b-',
                       sample(c(sample(letters, 8, T),
                                sample(0:9, 8, T)),
                              16),
                       'r'),
                     collapse = '')

  # launch new bucket
  bucket <- aws_connection$s3$create_bucket(Bucket = s3_bucket,
                                            CreateBucketConfiguration = list(LocationConstraint = aws_connection$region))
  aws_connection$s3_bucket <- s3_bucket

  return(aws_connection)
}


#' Terminate an AWS S3 bucket
#'
#' Terminating an S3 bucket means deleting it and all its contents.
#' The S3 bucket to be deleted is taken from the aws_connection object (`$s3_bucket`) which itself is set in [aws_launch_storage()].
#' This is major, do that with great care!
#'
#' @param aws_connection AWS connection object, as retrieved from [connect_aws()]. This also specifies the region.
#'
#' @return The updated AWS connection object.
#'
#' @examples
#' \dontrun{
#'
#' aws_terminate_storage(aws_connection)
#' }
#'
#' @seealso [connect_aws()], [aws_launch_storage()]
#' @export

aws_terminate_storage <- function(aws_connection) {

  # Test input
  if(is.null(aws_connection$s3)) {
    stop('AWS connection needs to be set up, initiated through ScrapeBotR::connect_aws.')
  }
  if(!is.character(aws_connection$s3_bucket)) {
    stop('A bucket ID to be terminated needs to be included. It is usually provided in the s3_buckets tibble within the aws_connection if launched via ScrapeBotR.')
  }

  # Terminate
  aws_connection$s3$delete_bucket(Bucket = aws_connection$s3_bucket)
  aws_connection$s3_bucket <- NA_character_

  return(aws_connection)
}




#' Launch a new database as AWS RDS database instance
#'
#' Launches an RDS instance and installs a fresh ScrapeBot central database.
#'
#' Note that this may cause costs as RDS instances cost real money.
#' Also, note that as this function waits for a new instance to launch, it takes quiet some time (usually some 3min).
#'
#' The RDS instance gets created in the current region with TCP traffic via port 3306 (default MySQL port) open to the world.
#' Currently uses MySQL 8.0.21 on a single-AZ publicly accessible RDS instance without automated backups and 50 GByte of storage.
#' The function waits for the instance to boot (this takes some time, up to 3-5min) before calling the [write_scrapebot_credentials()] function to store the respective credentials.
#' Ultimately, the function installs an empty ScrapeBot database, updates the aws_connection object and returns the latter.
#'
#' @param aws_connection AWS connection object, as retrieved from [connect_aws()]. This also specifies the region.
#' @param rds_type Just like EC2 instances, also RDS instances require a type. Note that these are not free. The default here, db.m6g.xlarge (along with the here pre-set 50GByte of storage), amounts to roughly 270 USD per month.
#'
#' @return The updated aws_connection object with rds_credential_section (to be used with connect_scrapebot) and rds_identifier (to be used with aws_terminate_database) set.
#'
#' @examples
#' \dontrun{
#'
#' aws_connection <- connect_aws()
#' aws_connection <- aws_launch_database(aws_connection)
#' aws_connection <- aws_launch_database(aws_connection, 'db.m6g.x2large')
#' }
#'
#' @seealso [connect_aws()], [aws_terminate_database()], [get_or_create_user()]
#' @export

aws_launch_database <- function(aws_connection, rds_type = 'db.m6g.xlarge') {

  # Test input
  if(is.null(aws_connection$rds)) {
    stop('AWS connection needs to be set up with RDS, initiated through ScrapeBotR::connect_aws.')
  }

  # rds settings
  dbname <- 'scrapebot'
  instance.name <- paste0('rds-instance-',
                          dbname)
  instance.engine <- 'mysql'
  instance.version <- '8.0.21'
  instance.storage <- 50
  instance.masteruser <- 'admin'
  instance.masteruserpassword <- paste0(sample(c(sample(letters, 8, T),
                                                 sample(LETTERS, 8, T),
                                                 sample(0:9, 8, T),
                                                 c('-', '_', '.', ':', ',', ';', '&', '%')),
                                               24),
                                        collapse = '')
  instance.port <- 3306
  scrapebot.sqlfile <- 'scrapebot.sql'
  scrapebot.user <- ''
  scrapebot.userpassword <- paste0(sample(c(sample(letters, 8, T),
                                            sample(LETTERS, 8, T),
                                            sample(0:9, 6, T)),
                                          12),
                                   collapse = '')

  # create security group
  instance_security_group_name <- 'ScrapeBotR RDS database'
  tryCatch({
    instance_security_group <- aws_connection$ec2$describe_security_groups(GroupNames = list(instance_security_group_name))
    aws_connection$security_group_database <- instance_security_group$SecurityGroups[[1]]$GroupId
    },
    error = function(e) {
      instance_security_group.creation <- aws_connection$ec2$create_security_group(GroupName = instance_security_group_name,
                                                                                   Description = 'automatically generated from R')
      aws_connection$security_group_database <- instance_security_group.creation$GroupId
      aws_connection$ec2$authorize_security_group_ingress(CidrIp = '0.0.0.0/0',
                                                          FromPort = instance.port,
                                                          ToPort = instance.port,
                                                          GroupId = aws_connection$security_group_database,
                                                          IpProtocol = 'tcp')
      })

  # launch new RDS instance
  database <- aws_connection$rds$create_db_instance(DBName = dbname,
                                                    DBInstanceIdentifier = instance.name,
                                                    DBInstanceClass = rds_type,
                                                    Engine = instance.engine,
                                                    MasterUsername = instance.masteruser,
                                                    MasterUserPassword = instance.masteruserpassword,
                                                    AvailabilityZone = paste0(aws_connection$region, 'a'),
                                                    MultiAZ = FALSE,
                                                    BackupRetentionPeriod = 0,
                                                    EngineVersion = instance.version,
                                                    PubliclyAccessible = TRUE,
                                                    VpcSecurityGroupIds = list(aws_connection$security_group_database),
                                                    AllocatedStorage = instance.storage)

  # wait for instance to boot
  database.host <- ''
  while (database.host == '') {
    Sys.sleep(2)
    instance.status <- aws_connection$rds$describe_db_instances(DBInstanceIdentifier = instance.name)
    tryCatch({
      database.host <- instance.status$DBInstances[[1]]$Endpoint$Address
      instance.state <- instance.status$DBInstances[[1]]$DBInstanceStatus
      if (length(database.host) == 0) {
        database.host <- ''
      } else if(stringr::str_length(database.host) < 20 | instance.state != 'available') {
        database.host <- ''
      }
    }, error = function(e) {
      database.host <- ''
    })
  }

  # store credentials
  write_scrapebot_credentials(database.host,
                              instance.masteruser,
                              instance.masteruserpassword,
                              dbname,
                              instance.port)

  aws_connection$rds_credential_section <- paste0(dbname, ' on ', database.host)
  aws_connection$rds_identifier <- instance.name

  # create ScrapeBot database
  scrapebot_connection <- connect_scrapebot(aws_connection$rds_credential_section)
  #scrapebot.sqlfileconn <- file(scrapebot.sqlfile)
  #scrapebot.sql <- strsplit(paste(readLines(scrapebot.sqlfileconn,
  #                                          encoding = 'utf-8'),
  #                                collapse = ' '),
  #                          ';',
  #                          fixed = T)[[1]]
  #close(scrapebot.sqlfileconn)
  #usethis::use_data(scrapebot.sql, internal = T)
  for(sql in scrapebot.sql) {
    res <- DBI::dbSendQuery(scrapebot_connection$db, sql)
    DBI::dbClearResult(res)
  }
  disconnect(scrapebot_connection)

  return(aws_connection)
}


#' Terminate an AWS RDS database instance
#'
#' Terminating an RDS database instance means deleting it and all its contents, backups, and images.
#' The RDS database instance to be deleted is taken from the aws_connection object (`$rds_identifier`) which itself is set in [aws_launch_database()].
#' This is major, do that with great care!
#'
#' @param aws_connection AWS connection object, as retrieved from [connect_aws()]. This also specifies the region.
#'
#' @return The updated AWS connection object.
#'
#' @examples
#' \dontrun{
#'
#' aws_terminate_database(aws_connection)
#' }
#'
#' @seealso [connect_aws()], [aws_launch_storage()]
#' @export

aws_terminate_database <- function(aws_connection) {

  # Test input
  if(is.null(aws_connection$rds)) {
    stop('AWS connection needs to be set up with RDS, initiated through ScrapeBotR::connect_aws.')
  }
  if(is.na(aws_connection$rds_identifier)) {
    stop('No AWS RDS instance set. Maybe this was not initiated through aws_launch_database?')
  }

  # Terminate
  aws_connection$rds$delete_db_instance(DBInstanceIdentifier = aws_connection$rds_identifier,
                                        SkipFinalSnapshot = TRUE)
  aws_connection$rds_identifier <- NA_character_

  return(aws_connection)
}


#' Save an AWS connection object for later use.
#'
#' AWS connection objects hold information about running instances, storages, and set up databases.
#' To later terminate them, you need to store/save your AWS connection object.
#'
#' @param aws_connection AWS connection object, as retrieved from [connect_aws()].
#' @param file Path to the file where to save the aws_connection object (in RDS format).
#'
#' @return The unchanged AWS connection object for chaining.
#'
#' @examples
#' \dontrun{
#'
#' aws_save_connection(aws_connection)
#' }
#'
#' @seealso [connect_aws()], [aws_load_connection()]
#' @export

aws_save_connection <- function(aws_connection, file = 'aws_connection.rds') {

  aws_connection_saved <- aws_connection
  aws_connection_saved$ec2 <- NULL
  aws_connection_saved$rds <- NULL
  aws_connection_saved$s3 <- NULL

  readr::write_rds(aws_connection_saved,
                   file)

  return(aws_connection)
}


#' Load a previously saved AWS connection object.
#'
#' Loads a previously saved AWS connection object.
#' Then, the function re-initiates the connection to AWS.
#'
#' @param file Path to the file where to save the aws_connection object (in RDS format).
#'
#' @return The loaded and re-initiated AWS connection object.
#'
#' @examples
#' \dontrun{
#'
#' aws_load_connection('aws_connection.rds')
#' }
#'
#' @seealso [connect_aws()], [aws_load_connection()]
#' @export

aws_load_connection <- function(file = 'aws_connection.rds') {

  aws_connection <- readr::read_rds(file)

  # Try connecting to EC2
  tryCatch({
    aws_connection$ec2 <- paws::ec2(config = list(credentials = list(creds = list(access_key_id = aws_connection$aws_access_key,
                                                                                  secret_access_key = aws_connection$aws_secret)),
                                                  region = aws_connection$region))
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

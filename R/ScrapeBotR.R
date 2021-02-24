#' ScrapeBotR: Orchestrate Instances and Retrieve Data from a ScrapeBot Database
#'
#' ScrapeBotR allows to easily retrieve (large amounts of) data from a [ScrapeBot](https://github.com/MarHai/ScrapeBot) installation.
#' The package provides easy-to-use functions to read and export instances, recipes, runs, log information, and data.
#' The package also allows to orchestrate instances on your AWS account (caution: may cause real costs).
#' Thereby, the package plugs neatly into the tidyverse as it makes heavy use of tibbles.
#'
#' @section Getting Started:
#' - Create a credentials file to connect to your database. You may do so using [write_scrapebot_credentials()].
#' - Create a connection object by using [connect_scrapebot()].
#' - List the available recipes through [get_recipes()] or the available instances through [get_instances()].
#' - Get information about specific runs through [get_runs()].
#' - Collect data via [get_run_data()].
#' - Bulk-download (and resize) screenshots from S3 through [collect_screenshots_from_s3()].
#' - [disconnect()] from your database.
#'
#' @section Orchestrate AWS:
#' You can use this package to orchestrate your Amazon Web Services (AWS) account.
#' Once set up with IAM user credentials, this package allows you to ...
#' - set up an RDS database and install a new ScrapeBot central database therein
#' - set up S3 storage to upload your screenshots to
#' - set up multiple EC2 instances as, well, ScrapeBot instances, immediately ready to run
#' - vary regions, browser sizes, language, and the like
#' - terminate all of these servers for good
#'
#' @section ScrapeBot:
#' The ScrapeBot (without "R") is a non-R tool for so-called "agent-based testing" to automatically visit, modify, and scrape a defined set of webpages regularly.
#' It was built to automate various web-based tasks and keep track of them in a controllable way for academic research, primarily in the realm of computational social science.
#'
#' @seealso <https://github.com/MarHai/ScrapeBot>
#'
#' @references Haim, M. (2020). Agent-based testing: An automated approach toward artificial reactions to human behavior.
#'     Journalism Studies, 21(7), 895-911. <https://dx.doi.org/10.1080/1461670x.2019.1702892>
#'
#' @docType package
#' @name ScrapeBotR

NULL

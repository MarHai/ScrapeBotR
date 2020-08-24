#' ScrapeBotR: Retrieve Data from a ScrapeBot Database
#'
#' ScrapeBotR allows to easily retrieve (large amounts of) data from a [ScrapeBot](https://github.com/MarHai/ScrapeBot) installation.
#' The package provides easy-to-use functions to read and export instances, recipes, runs, log information, and data.
#' Thereby, the package plugs neatly into the tidyverse as it makes heavy use of tibbles.
#'
#' @section Getting Started:
#' - Create a credentials file to connect to your database. You may do so using [write_credentials()].
#' - Create a connection object by using [connect()].
#' - List the available recipes through [get_recipes()] or the available instances through [get_instances()].
#' - Get information about specific runs through [get_runs()].
#' - Collect data via [get_run_data()].
#' - [disconnect()] from your database.
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

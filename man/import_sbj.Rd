% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_sbj.R
\name{import_sbj}
\alias{import_sbj}
\title{Import a ScrapeBot JSON recipe file (\code{.sbj})}
\usage{
import_sbj(connection, sbj_file, recipe_owner)
}
\arguments{
\item{connection}{A connection object, as retrieved from \code{\link[=connect_scrapebot]{connect_scrapebot()}}.}

\item{sbj_file}{Character string identifying the recipe.}

\item{recipe_owner}{The email address of the ScrapeBot user who will be the owner of the new recipe as character string. If this one does not exist, it will be created (in this case, a text will be raised).}
}
\value{
The \code{uid} (integer) of the newly created recipe or an integer \code{NA}.
}
\description{
Import a ScrapeBot JSON recipe file (\code{.sbj})
}
\examples{
\dontrun{

connection <- connect('my_db on localhost')
import_sbj(
  connection,
  'https://raw.githubusercontent.com/MarHai/ScrapeBot/master/recipes/find_my_ip.sbj',
  'mario@haim.it'
)
disconnect(connection)
}

}
\seealso{
\code{\link[=add_recipe]{add_recipe()}}, \code{\link[=add_recipe_step]{add_recipe_step()}}
}

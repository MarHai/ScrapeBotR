% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recipe2instance.R
\name{combine_recipe_with_instance}
\alias{combine_recipe_with_instance}
\title{Set a recipe to be run on a given instance.}
\usage{
combine_recipe_with_instance(connection, recipe_uid, instance_uid)
}
\arguments{
\item{connection}{A connection object, as retrieved from \code{\link[=connect_scrapebot]{connect_scrapebot()}}.}

\item{recipe_uid}{The \code{uid} (integer) of the corresponding recipe.}

\item{instance_uid}{The \code{uid} (integer) of the corresponding instance.}
}
\value{
\code{TRUE} if update was successful, \code{FALSE} otherwise.
}
\description{
Set a recipe to be run on a given instance.
}
\examples{
\dontrun{

connection <- connect('my_db on localhost')
combine_recipe_with_instance(connection, 42, 2)
disconnect(connection)
}

}
\seealso{
\code{\link[=get_recipes]{get_recipes()}}, \code{\link[=get_instances]{get_instances()}}
}

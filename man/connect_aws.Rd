% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connect.R
\name{connect_aws}
\alias{connect_aws}
\title{Connect to Amazon Web Services (AWS) for instances (EC2), databases (RDS), and storage (S3)}
\usage{
connect_aws(
  region = "eu-central-1",
  credentials_section = "AWS",
  credentials_file = "~/.scrapebot.ini"
)
}
\arguments{
\item{region}{The AWS region to connect to (e.g., "eu-central-1" for Frankfurt) as character.}

\item{credentials_section}{The section within your INI file holding the credentials. If \code{\link[=write_aws_credentials]{write_aws_credentials()}} was used to create the INI file, then the section is called "AWS".}

\item{credentials_file}{The INI file to use. If \code{\link[=write_aws_credentials]{write_aws_credentials()}} was used to create the INI file, then it is called \code{~/.scrapebot.ini} (the default).}
}
\value{
An AWS connection object (i.e., a specified list), ready to be passed to other \link{ScrapeBotR} functions.
}
\description{
Connect to AWS to set up instances (EC2), databases (RDS), and storage (S3). Only necessary if you want to orchestrate instances through R. Requires IAM account (in credentials_file/credentials_section) to have permissions for "AmazonEC2FullAccess," "AmazonRDSFullAccess, and "AmazonS3FullAccess."
In order not to use credentials here, you need to set up an INI file containing your AWS IAM credentials. You can do this by hand (see \code{\link[=write_aws_credentials]{write_aws_credentials()}}) or use the \code{\link[=write_aws_credentials]{write_aws_credentials()}} function.
}
\details{
AWS works in regions. As such, you need to specify the region here in which all subsequent actions will take place. You AWS user might not have all regions enabled. Find out more about it in your account settings (https://console.aws.amazon.com/billing/home?#/account).
}
\examples{
\dontrun{

connect_aws()
connect_aws('us-east-2')
connect_aws(NULL, 'my AWS creds', '~/my_own_credentials_file.ini')
}

}
\seealso{
\code{\link[=write_aws_credentials]{write_aws_credentials()}} to help create the credentials file
}

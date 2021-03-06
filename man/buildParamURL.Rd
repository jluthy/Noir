% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Noir.R
\name{buildParamURL}
\alias{buildParamURL}
\title{Set new url to find Parameters associated with Samples loaded into a workspace}
\usage{
buildParamURL(character)
}
\arguments{
\item{character}{The start of the url needed to query the sample names loaded into workspace.}
}
\value{
This function will return the url needed to find samples based on loaded workspace.
}
\description{
Set new url to find Parameters associated with Samples loaded into a workspace
}
\examples{
\dontrun{
url <- "http://localhost:4567/api/v1/parameters/all?workspaceid="
urlParams <- buildParamURL(url)
}
}

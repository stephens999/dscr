% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dsc.R
\name{reset_dsc}
\alias{reset_dsc}
\title{Removes all data, output and results for the dsc}
\usage{
reset_dsc(dsc, force = FALSE)
}
\arguments{
\item{dsc}{A dsc object.}

\item{force}{Boolean, indicates whether to proceed without
prompting user.}
}
\value{
Nothing; simply deletes files.
}
\description{
Removes all files in scores, meta, input and output
  subdirectories. Mostly useful for testing purposes.
}

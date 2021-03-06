% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shortcuts.R
\name{assignLoad}
\alias{assignLoad}
\title{Load .rdata files and assign them to an object of your choosing}
\usage{
assignLoad(rdata)
}
\arguments{
\item{rdata}{A filepath to a file to be read with `load()`}
}
\value{
An R object or objects read in with `load()`, without the original object names attached
}
\description{
This allows you to read files with `load()` without needing to keep the original object name
}

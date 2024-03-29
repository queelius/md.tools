% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/md_tools.R
\name{print.tbl_md}
\alias{print.tbl_md}
\title{print.tbl_md}
\usage{
\method{print}{tbl_md}(x, drop_latent = FALSE, max.print = NULL, ...)
}
\arguments{
\item{x}{A masked data object (tbl_md) to print.}

\item{drop_latent}{A logical value. If TRUE, latent random variables are dropped. Default is FALSE.}

\item{max.print}{An integer specifying the maximum number of rows and/or columns to print. Default is NULL (no limit).}

\item{...}{Additional arguments to pass to the print method.}
}
\description{
Print method for masked data (tbl_md).
}

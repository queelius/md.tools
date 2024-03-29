% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_write.R
\name{md_read_csv_with_meta}
\alias{md_read_csv_with_meta}
\title{md_read_csv_with_meta}
\usage{
md_read_csv_with_meta(
  file,
  read_meta = T,
  comment = "#",
  max_meta_lns = 1000L,
  ...
)
}
\arguments{
\item{file}{a path to a file, a connection, or literal data}

\item{read_meta}{whether to read in metadata to populate attributes}

\item{comment}{comment indicator, defaults to `#`}

\item{max_meta_lns}{limit metadata search to the indicated number of lines}

\item{...}{additional arguments to pass, like `skip`}
}
\description{
Read a (masked) data frame table from a connection (e.g., url or filename).
}

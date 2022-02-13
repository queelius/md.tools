#' Write data frame object to a CSV (comma separated file), optionally
#' with associated attribute data (stored as JSON in comments)
#'
#' @param df a data frame with attributes to write (like a masked data frame)
#' @param file filename for csv
#' @param comment denotes a comment block
#' @param ... additional arguments to pass
#'
#' @export
md_write_csv_with_meta <- function(df, file, comment="#",...)
{
  tmp <- tempfile()
  md_write_as_json(tmp,attributes(df))
  readr::write_lines(paste0(comment," ",readLines(tmp)),file)
  readr::write_csv(df,file,append=T,col_names=T,...)
}

#' Read a (masked) data frame table from a connection (e.g., url or filename).
#'
#' @param file a path to a file, a connection, or literal data
#' @param read_meta whether to read in metadata to populate attributes
#' @param comment comment indicator, defaults to \code{#}
#' @param max_meta_lns limit metadata search to the indicated number of lines
#' @param ... additional arguments to pass, like \code{skip}
#'
#' @export
md_read_csv_with_meta <- function(file,read_meta=T,comment="#",
                                  max_meta_lns=1000,...)
{
  metadata <- NULL
  if (read_meta)
    metadata <- md_read_json(file,comment,max_meta_lns)

  df <- NULL
  if (is.null(metadata) && is.null(metadata$col_types))
    df <- readr::read_csv(file,comment=comment,...)
  else
    df <- readr::read_csv(file,comment=comment,
                          col_types=metadata$col_types,
                          ...)

  attributes(df) <- c(attributes(df),metadata)
  md(df)
}

#' Write data frame object to a CSV (comma separated file), optionally
#' with associated attribute data (stored as JSON in comments)
#'
#' @param df a data frame with attributes to write
#' @param file filename for csv
#' @param comment denotes a comment block
#'
#' @export
md_write_csv_with_meta <- function(df, file, comment="#")
{
  tmp <- tempfile()
  md_write_as_json(tmp,attributes(df),comment)
  readr::write_lines(paste0("# ",readLines(tmp)),file)
  readr::write_csv(df,file,append=T,col_names = T)
}

#' helper function
#'
#' @param file connection
#' @param ls object (typically list) to write as json
#'
#' TODO: recursively search x and only write those values that can be
#'       serialized and deserialized by jsonlite.
md_write_as_json <- function(file,ls)
{
  x[["names"]] <- NULL
  x[["row.names"]] <- NULL
  x[["problems"]] <- NULL
  x[["spec"]] <- NULL

  jsonlite::write_json(x,file,simplifyVector=T,pretty=T)
}

# helper function
md_read_json <- function(file,prefix="",max_lines=-1L)
{
  pat <- paste0("^\\s*",prefix)
  lns <- grep(pat,readLines(file,n=max_lines),value=T)

  parse <- NULL
  pat <- paste0(pat,"(.*)")
  for (ln in lns)
    parse <- paste0(parse,gsub(pat,"\\1",ln))

  jsonlite::parse_json(parse,simplifyVector=T)
}


#' Read a data frame table from a connection (e.g., url or filename).
#'
#' @param file a path to a file, a connection, or literal data
#' @param read_meta whether to read in metadata to populate attributes
#' @param comment comment indicator, defaults to "#'
#' @param max_meta_lns limit metadata search to the indicated number of lines
#' @param skip a number indicating how many of the first lines to skip
#'
#' @export
md_read_csv_with_meta <- function(file,read_meta=T,comment="#",
                                  max_meta_lns=1000,skip=0)
{
  metadata <- NULL
  if (read_meta)
    metadata <- md_read_json(file,comment,max_meta_lns)

  df <- NULL
  if (is.null(metadata) && is.null(metadata$col_types))
    df <- readr::read_csv(file,comment=comment,skip=skip)
  else
    df <- readr::read_csv(file,comment=comment,skip=skip,
                          col_types=metadata$col_types)

  attributes(df) <- c(attributes(df),metadata)
  df
}

#' Obtain a list of latent variables from masked data.
#'
#' @param md masked data to retrieve latent variables from.
#' @export
md_latent <- function(md)
{
  attr(md,"latent")
}

#' Mark a variable in a masked data frame as latent.
#'
#' @param md masked data to modify
#' @param vars variables to mark as latent.
#' @export
md_mark_latent <- function(md, vars)
{
  attr(md,"latent") <- union(vars,md_latent(md))
  md
}

#' Mark a variable in a masked data frame as latent.
#'
#' @param md masked data to modify
#' @param vars variables to mark as latent.
#' @export
md_unmark_latent <- function(md, vars)
{
  attr(md,"latent") <- setdiff(md_latent(md),vars)
  md
}

#' Obtains a matrix from specified columns in a data frame.
#'
#' An \code{nrow(df)}-by-\code{p} boolean matrix \code{var} is encoded in
#' data fame \code{df} as the columns \code{var.1},...,\code{var.p} or
#' \code{var1},...,\code{varp}.
#'
#' A matrix will be returned with the appropriate ordering denoted by the
#' index, e.g., \code{a.2} will come before \code{a.4}. There should be no
#' gaps in the matrix indexes, e.g., if there is \code{a.4} then there must
#' be \code{a.1,a.2,a.3}.
#'
#' @param df data frame that contains the matrix
#' @param var the symbolic name of the matrix
#' @return a matrix
#' @export
matrix_from <- function(df,var)
{
  int_pat <- "[[:digit:]]+"
  pat <- paste0(var,"\\.?(",int_pat,")")
  cols <- colnames(df)[grepl(pat,colnames(df),ignore.case=F)]
  if (purrr::is_empty(cols) == 0)
    NA

  rank <- as.integer(stringr::str_replace(cols,pat,"\\1"))
  as.matrix(df[cols][,order(rank)])
}

#' Map boolean matrix defined by \code{var}, as described in
#' \code{matrix_from}, to a list of vector of integers.
#'
#' @param df masked data
#' @param var symbolic variable used to represent the matrix
#' @param name column name of matrix strings
#' @export
boolean_matrix_to_integer_list <- function(df,var,name=NULL)
{
  if (is.null(name))
    name <- var

  stopifnot(!(name %in% colnames(df)))
  A <- matrix_from(df,var)
  stopifnot(!is.na(A))

  m <- ncol(A)
  ints <- list()
  for (i in 1:nrow(df))
  {
    ints[[i]] <- (1:m)[A[i,]]
  }
  df[name] <- list(ints)
  df
}

#' Print method for masked data (\code{tbl_md}).
#'
#' @param x masked data to print
#' @param drop_latent Boolean, drop the latent random variables
#' @param ... additional arguments to pass
#' @importFrom dplyr %>%
#' @export
print.tbl_md <- function(x,drop_latent=F,...)
{
  if (!purrr::is_empty(md_latent(x)))
    cat("Latent variables: ", md_latent(x), "\n")

  cat(attributes(x))

  if (drop_latent)
    x <- x %>% dplyr::select(-intersect(md_latent(x),colnames(x)))

  NextMethod(...)
}

#' Test whether an object is a masked data (\code{tbl_md}).
#'
#' @param x object to determine if masked data
#' @export
is_md <- function(x)
{
  inherits(x,"tbl_md")
}

#' Constructor for masked data.
#'
#' Takes an object \code{x} and converts it to masked data, a tibble (data
#' frame) with some extra attributes, e.g., \code{latent} attribute to specify
#' which variables are latent in the model.
#'
#' @param x object to convert to masked data.
#' @export
md <- function(x)
{
  x <- tibble::as_tibble(x)
  class(x) <- unique(c("tbl_md",class(x)))
  x
}

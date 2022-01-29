#' Write masked data data frame (tibble) object to a CSV (comma separated file),
#' optionally writing associated meta-data to a JSON file. In particular, meta-data
#' in this case is defined as the attributes of the data frame object.
#'
#' @param md a masked data frame
#' @param filename filename for csv
#' @param write.metadata write a separate
#'
#' @export
md_write_csv <- function(md, filename, write.metadata=T)
{
  readr::write_csv(md, filename)
  if (write.metadata)
  {
    metadata <- attributes(md)
    metadata[["dataset"]] <- c(basename(filename))
    metadata[["row.names"]] <- NULL
    metadata[["names"]] <- NULL
    metadata[["class"]] <- NULL

    metadata.out <- paste(tools::file_path_sans_ext(filename),"json",sep=".")
    jsonlite::write_json(metadata, metadata.out, pretty=T)
  }
}

#' Read masked data from a JSON file.
#' If the JSON file has a 'dataset' field,
#' then each member of this field is assumed
#' to refer to a CSV file to read a masked
#' data sample from.
#'
#' Any metadata in the JSON
#' file is inserted into the attributes
#' of the masked data samples.
#'
#' @param filename filename for csv
#' @return list of masked data objects
#'
#' @export
md_read_json <- function(filename)
{
  metadata <- jsonlite::read_json(filename)
  dataset <- metadata[["dataset"]]

  ds <- list()
  for (data in dataset)
  {
    data.path <- file.path(dirname(filename),data)
    d <- readr::read_csv(data.path)
    tmp <- metadata
    tmp[["dataset"]] <- c(data)
    attributes(d) <- c(attributes(d),tmp)
    mds[[data]] <- md(d)
  }
  ds
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
  if (drop_latent)
    x <- x %>% dplyr::select(-intersect(md_latent(x),colnames(x)))

  NextMethod(...)

  if (purrr::is_empty(md_latent(x)))
    cat("latent variables: NONE\n")
  else
    cat("latent variables: ", md_latent(x), "\n")
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
  attr(x,"latent") <- c()
  x
}

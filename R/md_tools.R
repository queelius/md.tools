#' md_latent
#'
#' Obtain a list of latent variables from masked data.
#'
#' @param md masked data to retrieve latent variables from.
#' @return character vector containing latent variables
#' @export
md_latent <- function(md)
{
  attr(md, "latent")
}

#' md_mark_latent
#'
#' Mark variables in a masked data frame as latent.
#'
#' @param md masked data to modify
#' @param vars character vector containing variables to mark as latent.
#' @return modified masked data with updated latent variables
#' @export
md_mark_latent <- function(md, vars)
{
  stopifnot(is.character(vars))
  attr(md, "latent") <- union(vars, md_latent(md))
  md
}

#' md_unmark_latent
#'
#' Unmark variables in a masked data frame as latent.
#'
#' @param md masked data to modify
#' @param vars character vector containing variables to unmark as latent.
#' @return modified masked data with updated latent variables
#' @export
md_unmark_latent <- function(md, vars)
{
  stopifnot(is.character(vars))
  attr(md, "latent") <- setdiff(md_latent(md), vars)
  md
}

#' Decodes a matrix from specified columns in a data frame.
#'
#' An `nrow(df)`-by-`p` matrix `var` is encoded in
#' data frame `df` with the columns `var.1`,...,`var.p` or
#' `var1`,...,`varp`.
#'
#' A matrix will be returned with the appropriate ordering denoted by the
#' index, e.g., `a.2` will come before `a.4`. There should be no
#' gaps in the matrix indexes, e.g., if there is `a.4` then there must
#' also be `a.1,a.2,a.3`.
#'
#' @param df data frame that contains the matrix
#' @param var the symbolic name of the matrix
#' @return a matrix
#' @importFrom purrr is_empty
#' @importFrom stringr str_replace
#' @export
md_decode_matrix <- function(df,var)
{
  stopifnot(is.data.frame(df), is.character(var))

  int_pat <- "[[:digit:]]+"
  pat <- paste0(var,"\\.?(",int_pat,")")
  cols <- colnames(df)[grepl(pat,colnames(df),ignore.case=FALSE)]

  if (purrr::is_empty(cols))
    return(NULL)

  rank <- as.integer(stringr::str_replace(cols,pat,"\\1"))
  as.matrix(df[cols][,order(rank)])
}

#' Map Boolean matrix `mat` to a list of integer vectors.
#'
#' @param mat Boolean matrix
#' @return list of integer vectors
#' @export
md_boolean_matrix_to_list <- function(mat)
{
  stopifnot(is.matrix(mat), is.logical(mat))

  xs <- list()
  for (i in seq_len(nrow(mat)))
    xs[[i]] <- ((1:ncol(mat))[mat[i,]])
  xs
}

#' Map list of integer vectors to Boolean matrix.
#'
#' @param xs List of integer vectors.
#' @export
md_list_to_boolean_matrix <- function(xs)
{
  stopifnot(is.list(xs), all(vapply(xs, is.integer, logical(1))))
  n = length(xs)
  m <- max(unlist(
    lapply(xs,function(xs) { ifelse(length(xs)==0,0,max(xs)) })))
  mat <- matrix(nrow=n,ncol=m)
  for (i in seq_len(n))
    for (j in seq_len(m))
      mat[i,j] <- !is.na(match(j,xs[[i]]))
  mat
}

#' Encodes a matrix as a data frame with specified columns.
#'
#' @param mat matrix
#' @param var the symbolic name of the matrix (prefix of column names)
#' @return a tibble (data frame) encoding of a matrix
#' @importFrom tibble as_tibble
#' @export
md_encode_matrix <- function(mat,var)
{
  t <- tibble::as_tibble(mat,.name_repair="minimal",rownames=NULL)
  names(t) <- paste0(var,1:ncol(mat))
  md(t)
}

#' print.tbl_md
#'
#' Print method for masked data (tbl_md).
#'
#' @param x A masked data object (tbl_md) to print.
#' @param drop_latent A logical value. If TRUE, latent random variables are dropped. Default is FALSE.
#' @param max.print An integer specifying the maximum number of rows and/or columns to print. Default is NULL (no limit).
#' @param ... Additional arguments to pass to the print method.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr intersect
#' @importFrom purrr is_empty
#' @export
print.tbl_md <- function(x, drop_latent = FALSE, max.print = NULL, ...)
{
  if (!purrr::is_empty(md_latent(x))) {
    cat("Latent variables: ", md_latent(x), "\n")
  }

  if (drop_latent) {
    x <- x %>% dplyr::select(-dplyr::intersect(md_latent(x), colnames(x)))
  }

  NextMethod(max.print = max.print, ...)
}

#' is_md
#'
#' Test whether an object is a masked data (tbl_md).
#'
#' @param x object to determine if masked data
#' @export
is_md <- function(x)
{
  inherits(x,"tbl_md")
}

#' md
#'
#' Constructor for masked data.
#'
#' Converts an object `x` to masked data, which is a tibble (data frame) with
#' some extra attributes, such as the `latent` attribute to specify which
#' variables are latent in the model.
#'
#' @param x An object to convert to masked data.
#'
#' @return A masked data tibble with additional attributes.
#'
#' @importFrom tibble as_tibble
#' @export
md <- function(x)
{
  x <- tibble::as_tibble(x)
  class(x) <- unique(c("tbl_md", class(x)))
  x
}

#' md_set_size
#'
#' Decorates a masked data frame by adding a new column indicating the size of
#' sets (represented as boolean vectors with a specific prefix column encoding).
#'
#' @param md A masked data frame.
#' @param setvar A character string representing the column prefix for matrix encoding. Default is "x".
#' @param cname An optional character string for the name of the new column denoting the size of the sets. Defaults to concatenating "size_", and `setvar`.
#'
#' @return A decorated masked data frame with a new column `cname` indicating the size of the sets.
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @export
md_set_size <- function(md, setvar = "x", cname = NULL) {
  if (!is.data.frame(md)) {
    stop("md must be a data frame")
  }

  x <- md_decode_matrix(md, setvar)

  if (is.na(x)) {
    stop("x must not be NA")
  }

  w <- tibble::as_tibble(apply(x, 1, sum))

  if (is.null(cname)) {
    cname <- paste0("size_", setvar)
  }

  colnames(w) <- cname
  md %>% dplyr::bind_cols(w)
}

#' md_set_contains
#'
#' Decorates a data frame by adding a new column indicating whether a set
#' (represented as a boolean vector in the data frame with a specific
#' prefix column encoding) contains an element (stored under a specific
#' column name). For example, it can be used to check if a candidate set
#' contains the failed component.
#'
#' @param md A masked data frame.
#' @param setvar A character string representing the column prefix for matrix encoding. Default is "x".
#' @param elvar A character string representing the column name for elements to test membership for in `setvar`. Default is "k".
#' @param cname An optional character string for the name of the new column denoting outcomes of membership tests. Defaults to concatenating `elvar`, "_in_", and `setvar`.
#'
#' @return A decorated masked data frame with a new column `cname` indicating the membership test outcomes.
md_set_contains <- function(md, setvar = "x", elvar = "k", cname = NULL)
{
  if (!elvar %in% colnames(md))
    stop("elvar must be in the column names of md")
  x <- md_decode_matrix(md, setvar)
  if (is.na(x))
    stop("x must not be NA")

  els <- md[[elvar]]
  n <- nrow(x)

  if (is.null(cname))
    cname <- paste0(elvar, "_in_", setvar)

  md[[cname]] <- apply(x, 1, function(row, elements) row[elements], els)
  md
}

# md_set_contains <- function(md,setvar="x",elvar="k",cname=NULL)
# {
#   stopifnot(elvar %in% colnames(md))
#   x <- md_decode_matrix(md,setvar)
#   stopifnot(!is.na(x))
#   els <- md[elvar]
#   n <- nrow(x)
#   m <- ncol(x)
#
#   if (is.null(cname))
#     cname <- paste0(elvar, "_in_", setvar)
#
#   md[cname] <- rep(F,n)
#   for (i in 1:n)
#   {
#     md[cname][i,] <- x[i,els[i]]
#   }
#   md
# }

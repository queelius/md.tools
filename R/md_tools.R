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

#' Decodes a matrix from specified columns in a data frame.
#'
#' An \code{nrow(df)}-by-\code{p} matrix \code{var} is encoded in
#' data frame \code{df} with the columns \code{var.1},...,\code{var.p} or
#' \code{var1},...,\code{varp}.
#'
#' A matrix will be returned with the appropriate ordering denoted by the
#' index, e.g., \code{a.2} will come before \code{a.4}. There should be no
#' gaps in the matrix indexes, e.g., if there is \code{a.4} then there must
#' also be \code{a.1,a.2,a.3}.
#'
#' @param df data frame that contains the matrix
#' @param var the symbolic name of the matrix
#' @return a matrix
#' @importFrom purrr is_empty
#' @importFrom stringr str_replace
#' @export
md_decode_matrix <- function(df,var)
{
  int_pat <- "[[:digit:]]+"
  pat <- paste0(var,"\\.?(",int_pat,")")
  cols <- colnames(df)[grepl(pat,colnames(df),ignore.case=F)]
  if (purrr::is_empty(cols))
    return(NULL)

  rank <- as.integer(stringr::str_replace(cols,pat,"\\1"))
  as.matrix(df[cols][,order(rank)])
}

#' Map Boolean matrix \code{mat} to a list of integer vectors.
#'
#' @param mat Boolean matrix
#' @export
md_boolean_matrix_to_list <- function(mat)
{
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
  n = length(xs)
  m <- max(unlist(
    lapply(xs,function(xs) { ifelse(length(xs)==0,0,max(xs)) })))
  mat <- matrix(nrow=n,ncol=m)
  for (i in seq_len(n))
  {
    for (j in seq_len(m))
      mat[i,j] <- !is.na(match(j,xs[[i]]))
  }
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

#' Print method for masked data (\code{tbl_md}).
#'
#' @param x masked data to print
#' @param drop_latent If TRUE, drop the latent random variables
#' @param ... additional arguments to pass
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @export
print.tbl_md <- function(x,drop_latent=F,...)
{
  if (!purrr::is_empty(md_latent(x)))
    cat("Latent variables: ", md_latent(x), "\n")

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
#' @importFrom tibble as_tibble
#' @export
md <- function(x)
{
  x <- tibble::as_tibble(x)
  class(x) <- unique(c("tbl_md",class(x)))
  x
}

#' Decorates masked data with candidate sizes
#'
#' Takes masked data frame \code{md} with candidate set encoded as \code{x}
#' and returns a decorated masked data frame with a column \code{w} that
#' denotes the size of the candidate sets. No new information is added, it
#' just counts the number of times that a row element of \code{x} is \code{TRUE}.
#'
#' @param md masked data frame
#' @param var column prefix for matrix encoding
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols
#' @export
md_cand_sizes <- function(md,var="x")
{
  x <- md_decode_matrix(md,var)
  w <- tibble::as_tibble(apply(x,1,sum))
  colnames(w) <- c("w")
  md %>% dplyr::bind_cols(w)
}

#' Decorates masked data with whether candidate set contains failed component.
#'
#' Takes masked data frame \code{md} with candidate set encoded as \code{x}
#' and a column \code{k} denoting component cause of failure and returns a
#' decorated masked data frame with a column \code{contains} that
#' denotes whether the candidate set contains the component cause of failure.
#'
#' @param md masked data frame
#' @param var column prefix for matrix encoding
#' @export
md_cand_contains <- function(md,var="x")
{
  stopifnot(!is.null(md$k))
  x <- md_decode_matrix(md,var)
  stopifnot(!is.na(x))
  n <- nrow(x)
  m <- ncol(x)

  md$contains <- rep(F,n)
  for (i in 1:n)
  {
    k <- md$k[i]
    md$contains[i] <- x[i,k]
  }
  md
}

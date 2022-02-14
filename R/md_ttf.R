#' Generates time-to-failure (ttf) and component cause of failure for a parallel
#' system with the given data frame of component times-to-failure encoded by
#' the matrix columns prefixed with \code{var} in masked data frame \code{md}.
#'
#' @param md a data frame of component failure times
#' @param tau suspension time
#' @param var component symbolic variable, defaults to \code{t}
#' @importFrom dplyr %>%
#' @export
md_par_ttf <- function(md, tau = NULL, var="t")
{
  t <- md_decode_matrix(md,var)
  n <- nrow(t)
  m <- ncol(t)
  if (is.null(tau))
    tau <- rep(Inf,n)

  md$ttf <- apply(t,1,max)
  md$k <- matrix(rep(T,nrow(t)*ncol(t)),nrow=nrow(t))
  md$tau <- tau
  md$s <- ifelse(md$tau < md$ttf, md$tau, md$ttf)
  md$right_censored <- ifelse(md$tau < md$ttf, TRUE, FALSE)
  attr(md,"structure") <- "series"
  md.tools::md(md %>% md_mark_latent(c("ttf","k",paste0(var,1:m))))
}

#' Generates time-to-failure (ttf) and component cause of failure for a series
#' system with the given data frame of component times-to-failure encoded by
#' the matrix columns prefixed with \code{var} in masked data frame \code{md}.
#'
#' @param md a data frame with the indicated component times-to-failure
#' @param tau suspension time
#' @param var component symbolic variable, defaults to \code{t}
#' @importFrom dplyr %>%
#' @export
md_series_ttf <- function(md, tau=NULL, var="t")
{
  t <- md_decode_matrix(md,var)
  m <- ncol(t)
  if (is.null(tau))
    tau <- rep(Inf,nrow(t))
  md$k <- apply(t,1,which.min)
  md$ttf <- apply(t,1,min)
  md$tau <- tau
  md$s <- ifelse(md$tau < md$ttf, md$tau, md$ttf)
  md$right_censored <- ifelse(md$tau < md$ttf, TRUE, FALSE)
  attr(md,"structure") <- "parallel"
  md.tools::md(md %>% md_mark_latent(c("ttf","k",paste0(var,1:m))))
}

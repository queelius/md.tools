#' Masked data for a series system with exponentially distributed nodes.
#'
#' Masked data containing the system lifetime and other attributes of
#' \code{n=1000} series system with parameter value \code{theta=c(3,4,5)} and
#' candidate model in which conditions 1, 2, and 3 are met.
#'
#' @format A data frame with 1000 rows and 9 variables:
#' \describe{
#'   \item{s}{Real observable variable, series system lifetime}
#'   \item{k}{Integer latent variable, the assigned index of the failed component}
#'   \item{t1}{Real latent variable, lifetime of component 1}
#'   \item{t2}{Real latent variable, lifetime of component 2}
#'   \item{t3}{Real latent variable, lifetime of component 3}
#'   \item{x1}{Boolean observable variable, TRUE indicates component assigned index 1 is in candidate set}
#'   \item{x2}{Boolean observable variable, TRUE indicates component assigned index 2 is in candidate set}
#'   \item{x3}{Boolean observable variable, TRUE indicates component assigned index 3 is in candidate set}
#' }
#' @source \url{https://github.com/queelius/masked.data/blob/master/data-raw/exp_series_data_1_gen.R}
"exp_series_data_1"

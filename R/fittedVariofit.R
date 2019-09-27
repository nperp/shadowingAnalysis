#' @export
##For Conversion
#' @family fit
#' @name fittedVariofit
#' @title Fit the variogram depending on the max.dist based on the variofit of geoR
#' @returns  a data frame with the distances and the respective fitted variogram values
#' \code{fittedVariofit} uses the following inputs:
#'
#' @param x a variogram object/list
#' @param  max.dist the maximum distance between points taken into consideration
#' @description  Produces a fit depending on the x object and the max. distance
#' @details The available covariance models are: "matern", "powered.exponential","cauchy",generalized cauchy ("gencauchy")
#'
fittedVariofit <- function (x, max.dist, scaled = FALSE, ...) {

  my.l <- list()
  if (missing(max.dist)) {
    my.l$max.dist <- x$max.dist
    if (is.null(my.l$max.dist))
      stop("argument max.dist needed for this object")
  }
  else my.l$max.dist <- max.dist
  if (any(x$cov.model == c("matern", "powered.exponential",
                           "cauchy", "gencauchy", "gneiting.matern")))
    my.l$kappa <- x$kappa
  else kappa <- NULL
  if (is.vector(x$cov.pars))
    my.l$sill.total <- x$nugget + x$cov.pars[1]
  else my.l$sill.total <- x$nugget + sum(x$cov.pars[, 1])
  my.l$nugget <- x$nugget
  my.l$cov.pars <- x$cov.pars
  my.l$cov.model <- x$cov.model
  if (scaled) {
    if (is.vector(x$cov.model))
      my.l$cov.pars[1] <- my.l$cov.pars[1]/my.l$sill.total
    else my.l$cov.pars[, 1] <- my.l$cov.cov.pars[, 1]/my.l$sill.total
    my.l$sill.total <- 1
  }
  gamma.f <- function(x, my.l) {
    if (any(my.l$cov.model == c("linear", "power")))
      return(my.l$nugget + my.l$cov.pars[1] * (x^my.l$cov.pars[2]))
    else return(my.l$sill.total - geoR::cov.spatial(x, cov.model = my.l$cov.model,
                                              kappa = my.l$kappa, cov.pars = my.l$cov.pars))
  }
  x_vals <- seq(0, my.l$max.dist, length.out = 100)
  return(data.frame(x = x_vals, fit = gamma.f(x_vals, my.l = my.l)))

}

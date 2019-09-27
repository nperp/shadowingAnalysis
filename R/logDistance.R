#' @export
##For Conversion
#' @family path loss models
#' @name logDistance
#' @title Log distance path loss model
#' @return  A list with the estimated Intercept of the model and the residuals of the propagation, i.e. Shadowing
#' \code{logDistance} uses the following inputs:
#'
#' @param Data A dataframe with columns the distance from the transmiter and the measured power at that point.
#' @description  Produces the estiamted power in each measured point based on the log Distance path loss model
#'
#'

logDistance <- function(Data){

  #log the distance
  CDIST <- log10(Data$dist)
  # model the power with the distance
  logDistModel <- lm(Data$power ~ CDIST)

  return(logDistModel)
}

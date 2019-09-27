#' @export
##For Conversion
#' @family path loss models
#' @name erceg
#' @title Erceg HL/FM path loss model
#' @return  A list with the estimated Intercept of the model and the residuals of the propagation, i.e. Shadowing
#' \code{erceg} uses the following inputs:
#'
#' @param Data A dataframe with columns the distance from the transmiter and the measured power at that point.
#' @param freq the frequency in MHz of the transmition
#'
#' @description  Produces the estiamted power in each measured point based on the Erceg HL/FM path loss model
#'
#'
erceg <- function(Data,freq){

  #initialize the results list
  results <- list()

  # estimate the path loss from Erceg model
  path_loss <- 20*log10(freq)+
              10*(4-0.0065*25+17.1/25)*log10(Data$dist)
  # calculate the intercept
  results$intercept <- mean( Data$power + path_loss)

  # and the residuals
  results$residuals <- (results$intercept-path_loss)-
    Data$power;
  return(results)

}

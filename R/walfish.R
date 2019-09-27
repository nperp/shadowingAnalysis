#' @export
##For Conversion
#' @family path loss models
#' @name walfish
#' @title Walfish-Ikegami path loss model
#' @return  A list with the estimated Intercept of the model and the residuals of the propagation, i.e. Shadowing
#' \code{walfish} uses the following inputs:
#'
#' @param Data A dataframe with columns the distance from the transmiter and the measured power at that point.
#' @param freq the frequency in MHz of the transmition
#'
#' @description  Produces the estiamted power in each measured point based on the Walfish-Ikegami path loss model
#'
#'

walfish <- function(Data,freq){
  #initialize the results list
  results <- list()

  # estimate the path loss from Walfish-Ikegami model
  path_loss=32.45+20*log10(Data$dist/1000)+20*log10(freq)+(-16.9-10*log10(6)+10*log10(freq)+20*log10(12-1.76)+(4-0.11*(90-55)))+(-18*(1+25-15)+54+18*log10(Data$dist/1000)+(-4+0.7*(freq/925-1))*log10(freq)-9*log10(30))

   # estimate the intercept
  results$intercept<-mean( Data$power + path_loss)

  # and the residuals
  results$residuals<-(results$intercept-path_loss)-
    Data$power
return(results)
}

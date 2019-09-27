#' @export
##For Conversion
#' @family path loss models
#' @name cost231
#' @title COST 231 path loss model
#' @return  A list with the estimated Intercept of the model and the residuals of the propagation, i.e. Shadowing
#' \code{cost231} uses the following inputs:
#'
#' @param Data A dataframe with columns the distance from the transmiter and the measured power at that point.
#' @param freq the frequency in MHz of the transmition
#'
#' @description  Produces the estiamted power in each measured point based on the COST 231 path loss model
#'
#'
cost231 <- function(Data,freq){
  #initialize the results list
  results <- list()

  # estimate the path loss from Walfish-Ikegami model
  path_loss=-46.3-33.9*log10(freq)+13.82*log10(25)+((1.1*log10(freq)-0.7)*1.76+(1.56*log10(freq)-0.8))-(44.9-6.55*log10(25))*log10(Data$dist/1000)

  # estimate the intercept
  results$intercept<-mean( Data$power + path_loss)

  # and the residuals
  results$residuals<-(results$intercept-path_loss)-Data$power
  return(results)


}

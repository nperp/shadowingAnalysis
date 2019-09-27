#' @export
##For Conversion
#' @family path loss models
#' @name okumura
#' @title Okumura - Hata path loss model
#' @return  A list with the estimated Intercept of the model and the residuals of the propagation, i.e. Shadowing
#' \code{okumura} uses the following inputs:
#'
#' @param Data A dataframe with columns the distance from the transmiter and the measured power at that point.
#' @param freq the frequency in MHz of the transmition
#' @param typeOfArea the type of area the measurement took place, i.e.: Urban or Suburban
#'
#' @description  Produces the estiamted power in each measured point based on the Okumura-Hata path loss model
#'
#'

okumura <- function(Data,freq,typeOfArea){
  #initialize the results list
  results <- list()

  # estimate the path loss from Walfish-Ikegami model
  switch(typeOfArea,"Urban"={
    path_loss=69.55+26.16*log10(freq)-13.82*log10(25)-((1.11*log10(freq)-0.7)*1.76-(1.56*log10(freq)-0.8))+(44.9-6.55*log10(25))*log10(Data$dist/1000)

    },"Suburban"={
      path_loss=69.55+26.16*log10(freq)-13.82*log10(25)-((1.11*log10(freq)-0.7)*1.76-(1.56*log10(freq)-0.8))-(5.4+2*(log10(freq/28))^2)+(44.9-6.55*log10(25))*log10(Data$dist/1000)
  },{
    path_loss=69.55+26.16*log10(freq)-13.82*log10(25)-((1.11*log10(freq)-0.7)*1.76-(1.56*log10(freq)-0.8))+(44.9-6.55*log10(25))*log10(Data$dist/1000)
  })
  # estimate the intercept
  results$intercept<-mean( Data$power + path_loss)

  # and the residuals
  results$residuals<-(results$intercept-path_loss)-
    Data$power
  return(results)
}

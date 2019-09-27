#' @export
##For Conversion
#' @family fit
#' @name ExtractShadowingInfo
#' @title Extract variogram and variogram fit information
#' @return  A list with the estimated Intercept of the model and the residuals of the propagation, i.e. Shadowing
#' \code{ExtractShadowingInfo} uses the following inputs:
#'
#' @param Data a character of the propagation model
#' @param fittedModel the variogram model to be fitted to data
#' @param propModel the propagation model to be used
#' @description  Produces the estiamted power in each measured point based on the selected path loss model
#' @details The available propagation models are: "Log Distance", "Erceg HL/FM","Walfish-Ikegami","Okumura-Hata","COST 231"
#'
ExtractShadowingInfo <- function(Data,fittedModel,propModel){

  #coordinates of the TX
  xTx <- Data$tx[1]
  yTx <- Data$tx[2]

  #coordinates of the measurement points
  x <- Data$x
  y <- Data$y

  #Finding the distance of each measurement point to transmitter ####

  # create a point patern of transmiter with the measurement points
  ppTx <- spatstat::ppp(xTx, yTx, window=spatstat::bounding.box.xy(append(x,xTx),append(y,yTx)))

  # create a point pathern of measurement points
  ppRcv <- spatstat::ppp(x,y, window=spatstat::bounding.box.xy(x, y))
  # create scaling so as to move from coords to meter distance (based on earth dimensions)
  my <- median(y)
  yscale <- (2*pi/360)*6378100
  xscale <- yscale*cos(2*pi*abs(my)/360)

  # scale x and y coords
  ppTx <- spatstat::affine(ppTx, diag(c(xscale,yscale)))
  ppRcv <- spatstat::affine(ppRcv, diag(c(xscale,yscale)))
  dist <- as.vector(spatstat::crossdist(ppTx, ppRcv));

  Data$dist<-dist
  estimatedShadowing <- FitLinearPropModels(propModel,Data,Data$metadata$freq,Data$metadata$area)



  # create a geodata object to analyze
  shadowing=geoR::as.geodata(cbind(ppRcv$x, ppRcv$y, estimatedShadowing$residuals))
  # jitter the duplicate coords by max 1 meter so as not to have duplicate values from the GPS error
  shadowing$coords=geoR::jitterDupCoords(shadowing$coords, max=1);

  estimatedVariog<-geoR::variog(shadowing,max.dist=Data$max.dist)

  startingValues<-expand.grid(seq(10,30,by=2),seq(50,200,by=1));

  fittedVariogram<-geoR::variofit(estimatedVariog,startingValues,
                            cov.model=fittedModel,fix.nugget=TRUE,fix.kappa=FALSE);


  Data$Shadowing<-shadowing
  Data$varioG<-estimatedVariog
  Data$FIT<-fittedVariogram
  Data$propmodel<-estimatedShadowing
return(Data)

}

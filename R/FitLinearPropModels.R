#' @export
##For Conversion
#' @family fit
#' @name FitLinearPropModels
#' @title Fit various path loss models
#' @return  A list with the estimated Intercept of the model and the residuals of the propagation, i.e. Shadowing
#' \code{FitLinearPropModels} uses the following inputs:
#'
#' @param PropModel a character of the propagation model
#' @param Data A dataframe with columns the distance from the transmiter and the measured power at that point.
#' @param freq the frequency in MHz of the transmition
#' @param typeOfArea the type of area the measurement took place, i.e.: Urban or Suburban
#' @description  Produces the estiamted power in each measured point based on the selected path loss model
#' @details The available propagation models are: "Log Distance", "Erceg HL/FM","Walfish-Ikegami","Okumura-Hata","COST 231"
#'
FitLinearPropModels <- function(PropModel,Data,
                             freq,typeOfArea){
  #checks
  #TODO: change the checks if LOAD to be used
  if (!PropModel %in% c("Log Distance", "Erceg HL/FM","Walfish-Ikegami",
                        "Okumura-Hata","COST 231")){
    warning('Unknown propagation model. Log distance is used.')
    PropModel <- 'Log Distance'
  }else if (PropModel== "Okumura-Hata" & (is.null(freq) || is.null(typeOfArea))){
    warning('Information is missing for the Okumura-Hata model. Log distance is used.')
    PropModel <- 'Log Distance'
  }else if(PropModel %in%  c("Erceg HL/FM","Walfish-Ikegami","COST 231") & is.null(freq)){
    warning(paste0("Information is missing for the ", PropModel,". Log distance is used."))
            PropModel <- 'Log Distance'
  }

  results <- switch(PropModel,"Log Distance"={
    logDistance(Data)
  },"Erceg HL/FM"={erceg(Data,freq)
  },"Walfish-Ikegami"={walfish(Data,freq)
  },"Okumura-Hata"={okumura(Data,freq,typeOfArea)
  },"COST 231"={cost231(Data,freq)

  },"Load"={
    ###TODO: change this to loading mechanism
    CDIST <- log10(Data$dist)
    logDistance(Data)
  })
}


PlotShadowingAnalysis<-function (Data){
  listOfPlots<-list()
  dat<-data.frame(data=Data$power)
  # density plot of the measured received power
  loaded_received_density<-ggplot2::ggplot(dat, ggplot2::aes(x=data))+
    ggplot2::geom_histogram(ggplot2::aes(y=..density..),      # Histogram with density instead of count on y-axis
                   #        binwidth=.5,
                   colour="black", fill="white") +
    ggplot2::geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
    ggplot2::scale_x_continuous(name="Measured Power [dBm]")+
    ggplot2::scale_y_continuous(name="Density")

  dat<-data.frame(data=Data$Shadowing$data)

  # plot of the measured received power on respective points on map
  full_measurement_points<-ggmap::ggmap(Data$metadata$map,darken=0)+
    ggplot2::geom_point(ggplot2::aes(x,y,colour=Power),data=data.frame(x=Data$x,y=Data$y,Power=Data$power),size = 4)+
    ggplot2::scale_x_continuous(name="Longitude")+
    ggplot2::scale_y_continuous(name="Latitude")+
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Power [dBm]",keywidth = 1, keyheight = 1))

  # density plot of the extracted shadowing field
  full_Shadowing_density<-ggplot2::ggplot(dat, ggplot2::aes(x=data))+
    ggplot2::geom_histogram(ggplot2::aes(y=..density..),      # Histogram with density instead of count on y-axis
                   #        binwidth=.5,
                   colour="black", fill="white")+
    ggplot2::geom_density(alpha=.2, fill="#FF6666")+  # Overlay with transparent density plot
    ggplot2::scale_x_continuous(name="Shadowing [dB]")+
    ggplot2::scale_y_continuous(name="Density")

  # variogram of the shadowing and the respective fit
  full_varioG<-ggplot2::ggplot(data.frame(semivariance=Data$varioG$v,distance=Data$varioG$u),ggplot2::aes(x=distance,y=semivariance))+
    ggplot2::geom_point()+
    ggplot2::geom_line(data=fittedVariofit(Data$FIT),ggplot2::aes(x=x,y=fit))+
    ggplot2::scale_y_continuous(name="Semivariance [dB]")+
    ggplot2::scale_x_continuous(name="Distance [m]")

  #results
  listOfPlots$full_measurement_points<-full_measurement_points
  listOfPlots$loaded_received_density<-loaded_received_density
  listOfPlots$full_Shadowing_density<-full_Shadowing_density
  listOfPlots$full_varioG<-full_varioG

  return(listOfPlots)
  #grid.arrange(full_measurement_points,loaded_received_density,full_Shadowing_density,full_varioG,nrow=2,ncol=2) #,
}

plotSitesTimes<-function(input,data,filter="green",doPercent=FALSE,direction=2) {
  
  azimuth<-45
  elevation<-40
  distance<-1000000
  
  alpha<-1

  if (is.null(input)) 
    input<-list(whichDay="week days",whichTime="",whichSite="",whichLimit="auto")

  volumes<-matrix(0,9,24)
  for (site in 1:9) {
    for (time in 0:23) {
      fullresult<-getSpeeds(list(whichDay=input$whichDay,whichTime=time,whichSite=site,whichLimit=input$whichLimit),data)
      switch(filter,
             "black"=use<-(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"black")),
             "purple"=use<-(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"purple")),
             "red"=use<-(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"red")),
             "orange"=use<-(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"orange")),
             "green"=use<-(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"green"))
      )
      if (length(direction)==1) {
        if (doPercent) 
              volumes[site,time+1]<-sum(fullresult$counts[direction,use])/sum(fullresult$counts[direction,])*100
        else  volumes[site,time+1]<-sum(fullresult$counts[direction,use])
      }
      else {
        if (doPercent) 
              volumes[site,time+1]<-(sum(fullresult$counts[direction,use])+sum(fullresult$counts[2,use]))/sum(fullresult$counts)*100
        else  volumes[site,time+1]<-sum(fullresult$counts[1,use])+sum(fullresult$counts[2,use])
      }
    }
  }
  
  if (filter=="purple") zlim<-c(0,1)*max(25,max(volumes,na.rm=TRUE)*1.1*2)
  else zlim<-c(0,1)*max(100,max(volumes,na.rm=TRUE)*1.1*2)
  if (doPercent) zlim<-c(0,110)
  zticks<-axisTicks(zlim,FALSE,NULL,5)
  
  if (doPercent) zlabel<-"%vehicles"
  else zlabel<-"vehicles per hour"
  
  ylabel<-"site"
  ylim<-c(0,10)
  yticks<-seq(1,9,1)
  xlabel<-"time"
  xlim<-c(25,-1)
  
  xticks<-seq(0,24,4)
  g<-start3dPlot(xlim,ylim,zlim,
                 xticks,yticks,zticks,
                 xlabel,ylabel,zlabel,
                 azimuth=azimuth,elevation=elevation,distance=distance)
  mapping<-mapping3D(azimuth=azimuth,elevation=elevation,distance=distance,xlim=xlim,ylim=ylim,zlim=zlim)
  
  for (site in 9:1) {
    t<-c()
    v<-c(0)
    for (time in 0:23) {
      fullresult<-getSpeeds(list(whichDay=input$whichDay,whichTime=time,whichSite=site,whichLimit=input$whichLimit),data)
      switch(filter,
             "black"=use<-(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"black")),
             "purple"=use<-(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"purple")),
             "red"=use<-(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"red")),
             "orange"=use<-(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"orange")),
             "green"=use<-(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"green"))
      )
      
      for (band in c("black","purple","red","orange","green")) {
        if (band=="black") {
          t<-c(t,time,time)
        }
        switch(band,
             "black"=use1<-(fullresult$speeds<speedUpperBand(fullresult$speedLimit,"black")),
             "purple"=use1<-(fullresult$speeds<speedUpperBand(fullresult$speedLimit,"purple")),
             "red"=use1<-(fullresult$speeds<speedUpperBand(fullresult$speedLimit,"red")),
             "orange"=use1<-(fullresult$speeds<speedUpperBand(fullresult$speedLimit,"orange")),
             "green"=use1<-(fullresult$speeds<speedUpperBand(fullresult$speedLimit,"green"))
      )
        if (length(direction)==1)
              volume<-sum(fullresult$counts[direction,use&use1])
        else  volume<-sum(fullresult$counts[1,use&use1])+sum(fullresult$counts[2,use&use1])
        if (doPercent) volume<-volume/sum(fullresult$counts[direction,])*100
      if (band=="black") {
        v<-c(v,volume,volume)
      }
      
      y<-c(0,0,0,0)+site
      x<-(c(0,0,1,1)+time)
      z<-c(0,1,1,0)*volume
      pts<-rotate3D(data.frame(x=x,y=y,z=z),mapping)
      g<-addG(g,dataPolygon(pts,colour=NA,fill=speedFill(band),alpha=alpha))
      }
    }
    y<-rep(site,length(t))
    x<-t
    z<-v[1:(length(v)-1)]
    pts<-rotate3D(data.frame(x=x,y=y,z=z),mapping)
    g<-addG(g,dataLine(pts))
  }
  return(g)
}


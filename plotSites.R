plotSites<-function(input,data) {
  
  BrawOpts(graphicsType = "HTML")
  
  xlim<-c(0,10)
  
  ylim<-c(-1,1)*75
  
  g<-startPlot(xlim=xlim,
               ylim=ylim,
               xlabel="Site",xticks=1:9,
               ylabel="Speed",yticks=list(breaks=NULL,labels=NULL,logScale=FALSE)
               )
  for (site in 1:9) {
    d<-data[[paste0("s",site)]]
    fullresult<-getSpeeds(input,d$values)
    for (direction in 1:2) {
      for (i in 2:length(fullresult$speeds)) {
        result<-data.frame(y=c(0,0,1,1)*fullresult$speeds[i-1]+c(1,1,0,0)*fullresult$speeds[i],
                           x=site+c(-0.5,0.5,0.5,-0.5)
        )
        if (direction==1) result$y<- - result$y
        if (fullresult$speeds[i-1]<d$speedLimit) col<-"green"
        else {
          if (fullresult$speeds[i-1]<d$speedLimit*1.1+2) col<-"orange"
          else col<-"red"
        }
        g<-addG(g,dataPolygon(result,colour=NA,fill=col,alpha=fullresult$counts[direction,i-1]/100))
      }
    }
  }

  g<-addG(g,dataLine(data.frame(x=xlim,y=c(0,0)),colour="grey"))
  
  g<-addG(g,dataText(data.frame(x=max(xlim),y=max(ylim)*0.8),label="← Westwards ←",hjust = 1,vjust=1))
  g<-addG(g,dataText(data.frame(x=min(xlim),y=min(ylim)*0.8),label="→ Eastwards →",hjust = 0,vjust=0))
  return(g)
}
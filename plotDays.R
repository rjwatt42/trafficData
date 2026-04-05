plotDays<-function(input,data,volume=FALSE,filter="none") {
  
  BrawOpts(graphicsType = "HTML")
  
  site<-as.numeric(input$whichSite)
  
  xlim<-c(0,8)
  
  if (volume) {
    volumes<-matrix(0,2,7)
    d<-data[[paste0("s",site)]]
    for (day in 1:7) {
      whichDay<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")[day]
      fullresult<-getSpeeds(list(whichDay=whichDay,whichTime=input$whichTime),d$values)
      switch(filter,
             "none"={use<-1:length(fullresult$speeds)},
             "red"=use<-which(fullresult$speeds>=(d$speedLimit*1.1+2)),
             "orange"=use<-which(fullresult$speeds>=(d$speedLimit))
      )
      for (direction in 1:2) {
        volumes[direction,day]<-sum(fullresult$counts[direction,use])
      }
    }
    
    ylim<-c(-1,1)*max(500,max(volumes))
    g<-startPlot(xlim=xlim,
                 ylim=ylim,
                 xlabel="Day",xticks=list(breaks=1:7,labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),logScale=FALSE),
                 ylabel="Volume",yticks=list(breaks=NULL,labels=NULL,logScale=FALSE)
    )
    
    for (day in 1:7) {
      v<-data.frame(y=-c(0,1,1,0)*volumes[1,day],x=c(0,0,1,1)+(day-0.5))
      g<-addG(g,dataPolygon(v,fill="red",colour=NA))
      v<-data.frame(y=c(0,1,1,0)*volumes[2,day],x=c(0,0,1,1)+(day-0.5))
      g<-addG(g,dataPolygon(v,fill="red",colour=NA))
      if (filter!="red") {
        for (direction in 1:2) {
          use<-fullresult$speeds<(d$speedLimit*1.1+2)
          if (filter=="orange") use<-use & fullresult$speeds>=(d$speedLimit)
          volumes[direction,day]<-sum(fullresult$counts[direction,use])
        }
        v<-data.frame(y=-c(0,1,1,0)*volumes[1,day],x=c(0,0,1,1)+(day-0.5))
        g<-addG(g,dataPolygon(v,fill="orange",colour=NA))
        v<-data.frame(y=c(0,1,1,0)*volumes[2,day],x=c(0,0,1,1)+(day-0.5))
        g<-addG(g,dataPolygon(v,fill="orange",colour=NA))
        if (filter!="orange") {
          for (direction in 1:2) {
            use<-fullresult$speeds<d$speedLimit
            volumes[direction,day]<-sum(fullresult$counts[direction,use])
          }
          v<-data.frame(y=-c(0,1,1,0)*volumes[1,day],x=c(0,0,1,1)+(day-0.5))
          g<-addG(g,dataPolygon(v,fill="green",colour=NA))
          v<-data.frame(y=c(0,1,1,0)*volumes[2,day],x=c(0,0,1,1)+(day-0.5))
          g<-addG(g,dataPolygon(v,fill="green",colour=NA))
        }
      }
    }
    g<-addG(g,dataLine(data.frame(x=xlim,y=c(0,0)),colour="grey"))
    
    g<-addG(g,dataText(data.frame(x=max(xlim),y=max(ylim)*0.8),label="← Westwards ←",hjust = 1,vjust=1))
    g<-addG(g,dataText(data.frame(x=min(xlim),y=min(ylim)*0.8),label="→ Eastwards →",hjust = 0,vjust=0))
    return(g)
  } 
  
  ylim<-c(-1,1)*75
  
  g<-startPlot(xlim=xlim,
               ylim=ylim,
               xlabel="Day",xticks=list(breaks=1:7,labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),logScale=FALSE),
               ylabel="Speed",yticks=list(breaks=NULL,labels=NULL,logScale=FALSE)
               )
  d<-data[[paste0("s",site)]]
  for (day in 1:7) {
    whichDay<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")[day]
    fullresult<-getSpeeds(list(whichDay=whichDay,whichTime=input$whichTime),d$values)
    for (direction in 1:2) {
      for (i in 2:length(fullresult$speeds)) {
        result<-data.frame(y=c(0,0,1,1)*fullresult$speeds[i-1]+c(1,1,0,0)*fullresult$speeds[i],
                           x=day+c(-0.5,0.5,0.5,-0.5)
        )
        if (direction==1) result$y<- - result$y
        if (fullresult$speeds[i-1]<d$speedLimit) col<-"green"
        else {
          if (fullresult$speeds[i-1]<d$speedLimit*1.1+2) col<-"orange"
          else col<-"red"
        }
        g<-addG(g,dataPolygon(result,colour=NA,fill=col,alpha=fullresult$counts[direction,i-1]/100))
      }
      use<-fullresult$speeds>=(d$speedLimit*1.1+2)
      hmm<-sum(fullresult$counts[direction,use])
      if (direction==2) 
        g<-addG(g,dataText(data.frame(x=day,y=ylim[2]),hmm,hjust=0.5,vjust=1,colour="#c00",size=0.75,fontface="bold"))
      else
        g<-addG(g,dataText(data.frame(x=day,y=ylim[1]),hmm,hjust=0.5,colour="#c00",size=0.75,fontface="bold"))
      use<-fullresult$speeds<(d$speedLimit*1.1+2) & fullresult$speeds>=d$speedLimit
      hmm<-sum(fullresult$counts[direction,use])
      if (direction==2) 
        g<-addG(g,dataText(data.frame(x=day,y=ylim[2]-diff(ylim)/20),hmm,hjust=0.5,vjust=1,colour="#ca0",size=0.75,fontface="bold"))
      else
        g<-addG(g,dataText(data.frame(x=day,y=ylim[1]+diff(ylim)/20),hmm,hjust=0.5,colour="#ca0",size=0.75,fontface="bold"))
    }
  }

  g<-addG(g,dataLine(data.frame(x=xlim,y=c(0,0)),colour="grey"))
  
  g<-addG(g,dataText(data.frame(x=max(xlim),y=max(ylim)*0.8),label="← Westwards ←",hjust = 1,vjust=1))
  g<-addG(g,dataText(data.frame(x=min(xlim),y=min(ylim)*0.8),label="→ Eastwards →",hjust = 0,vjust=0))
  return(g)
}
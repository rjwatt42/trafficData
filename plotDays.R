plotDays<-function(input,data,volume=FALSE,filter="green",doPercent=FALSE,showNumbers=FALSE) {
  
  site<-as.numeric(input$whichSite)
  
  xlim<-c(0,8)
  
  if (volume) {
    volumes<-matrix(0,2,7)
    d<-data[[paste0("s",site)]]
    for (day in 1:7) {
      whichDay<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")[day]
      fullresult<-getSpeeds(list(whichDay=whichDay,whichTime=input$whichTime,whichSite=input$whichSite),data)
      switch(filter,
             "green"={use<-rep(TRUE,length(fullresult$speeds))},
             "purple"=use<-(fullresult$speeds>=(fullresult$speedLimit+10)),
             "red"=use<-(fullresult$speeds>=(fullresult$speedLimit*1.1+2)),
             "orange"=use<-(fullresult$speeds>=(fullresult$speedLimit))
      )
      for (direction in 1:2) {
        volumes[direction,day]<-sum(fullresult$counts[direction,use])
      }
    }
    if (showNumbers) {
      ylim<-c(-5,5)
      g<-startPlot(xlim=xlim,
                   ylim=ylim,
                   xlabel="Day",xticks=list(breaks=1:7,labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),logScale=FALSE),
                   ylabel="Volume",yticks=list(breaks=0,labels=" ",logScale=FALSE),
                   box="x",top=1
      )
    } else  {
      if (doPercent) {ylim<-c(-1,1)*100;ylabel<-"Percent"}
      else           {ylim<-c(-1,1)*max(500,max(volumes,na.rm=TRUE));ylabel<-"Volume"}
      g<-startPlot(xlim=xlim,
                   ylim=ylim,
                   xlabel="Day",xticks=list(breaks=1:7,labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),logScale=FALSE),
                   ylabel=ylabel,yticks=list(breaks=NULL,labels=NULL,logScale=FALSE),
                   top=1
      )
    }         
    if (input$whichTime=="Average") 
      g<-addG(g,plotTitle(paste0("site:",input$whichSite," at ","average time")))
    else
      g<-addG(g,plotTitle(paste0("site:",input$whichSite," at ",input$whichTime)))
    
    for (day in 1:7) {
      whichDay<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")[day]
      fullresult<-getSpeeds(list(whichDay=whichDay,whichTime=input$whichTime,whichSite=input$whichSite),data)
      if (showNumbers) {
        g<-plotNumbers(d,fullresult,day,g)
      } else {
        g<-plotBars(fullresult,volumes,day,filter,doPercent=doPercent,g)
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
               ylabel="Speed",yticks=list(breaks=NULL,labels=NULL,logScale=FALSE),
               top=1
  )
  if (input$whichTime=="Average") 
    g<-addG(g,plotTitle(paste0("site:",input$whichSite," at ","average time")))
  else
    g<-addG(g,plotTitle(paste0("site:",input$whichSite," at ",input$whichTime)))
  d<-data[[paste0("s",site)]]
  for (day in 1:7) {
    whichDay<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")[day]
    fullresult<-getSpeeds(list(whichDay=whichDay,whichTime=input$whichTime,whichSite=input$whichSite),data)
    for (direction in 1:2) {
      for (i in 2:length(fullresult$speeds)) {
        result<-data.frame(y=c(0,0,1,1)*fullresult$speeds[i-1]+c(1,1,0,0)*fullresult$speeds[i],
                           x=day+c(-0.5,0.5,0.5,-0.5)
        )
        if (direction==1) result$y<- - result$y
        if (fullresult$speeds[i-1]<fullresult$speedLimit[i-1]) col<-"green"
        else {
          if (fullresult$speeds[i-1]<fullresult$speedLimit[i-1]*1.1+2) col<-"orange"
          else {
            if (fullresult$speeds[i-1]<fullresult$speedLimit[i-1]+10) col<-"red"
            else col<-"purple"
          }
        }
        g<-addG(g,dataPolygon(result,colour=NA,fill=col,alpha=fullresult$counts[direction,i-1]/100))
      }
      if (showNumbers) {
        use<-fullresult$speeds>=(fullresult$speedLimit*1.1+2)
        hmm<-sum(fullresult$counts[direction,use])
        if (direction==2) 
          g<-addG(g,dataText(data.frame(x=day,y=ylim[2]),hmm,hjust=0.5,vjust=1,colour="#c00",size=0.75,fontface="bold"))
        else
          g<-addG(g,dataText(data.frame(x=day,y=ylim[1]),hmm,hjust=0.5,colour="#c00",size=0.75,fontface="bold"))
        use<-fullresult$speeds<(fullresult$speedLimit*1.1+2) & fullresult$speeds>=fullresult$speedLimit
        hmm<-sum(fullresult$counts[direction,use])
        if (direction==2) 
          g<-addG(g,dataText(data.frame(x=day,y=ylim[2]-diff(ylim)/20),hmm,hjust=0.5,vjust=1,colour="#ca0",size=0.75,fontface="bold"))
        else
          g<-addG(g,dataText(data.frame(x=day,y=ylim[1]+diff(ylim)/20),hmm,hjust=0.5,colour="#ca0",size=0.75,fontface="bold"))
      }
    }
  }
  
  g<-addG(g,dataLine(data.frame(x=xlim,y=c(0,0)),colour="grey",linewidth=1.5))
  
  g<-addG(g,dataText(data.frame(x=max(xlim),y=max(ylim)*0.8),label="← Westwards ←",hjust = 1,vjust=1))
  g<-addG(g,dataText(data.frame(x=min(xlim),y=min(ylim)*0.8),label="→ Eastwards →",hjust = 0,vjust=0))
  return(g)
}

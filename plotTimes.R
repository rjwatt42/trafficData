plotTimes<-function(input,data,volume=FALSE,filter="green",doPercent=FALSE,showNumbers=FALSE) {
  
  xlim<-c(-1,24)
  sites<-NULL
  if (input$whichSite=="all") sites<-1:9
  if (input$whichSite=="all20") sites<-3:6
  if (input$whichSite=="all30") sites<-c(1:2,7:9)
  if (is.null(sites)) sites<-as.numeric(input$whichSite)
  if (length(sites)>1)   volume<-TRUE

  
  if (volume) {
    volumes<-matrix(0,2,24)
    for (site in sites){
    for (time in 0:23) {
      fullresult<-getSpeeds(list(whichDay=input$whichDay,whichTime=time,whichSite=site),data)
      switch(filter,
             "green"={use<-rep(TRUE,length(fullresult$speeds))},
             "black"=use<-(fullresult$speeds>=(fullresult$speedLimit+20)),
             "purple"=use<-(fullresult$speeds>=(fullresult$speedLimit+10)),
             "red"=use<-(fullresult$speeds>=(fullresult$speedLimit*1.1+2)),
             "orange"=use<-(fullresult$speeds>=(fullresult$speedLimit))
      )
      for (direction in 1:2) {
        volumes[direction,time+1]<-volumes[direction,time+1]+sum(fullresult$counts[direction,use])
      }
    }
    }
    
    if (showNumbers) {
      ylim<-c(-5,5)
      g<-startPlot(xlim=xlim,
                   ylim=ylim,
                   xlabel="Time of Day",xticks=list(breaks=seq(0,25,4),labels=paste0(seq(0,25,4),":00"),logScale=FALSE),
                   ylabel="Volume",yticks=list(breaks=0,labels=" ",logScale=FALSE),
                   box="x",top=1
      )
    } else  {
      if (doPercent) {ylim<-c(-1,1)*100;ylabel<-"Percent"}
      else           {ylim<-c(-1,1)*max(100,max(volumes,na.rm=TRUE)*1.05);ylabel<-"Volume"}
      g<-startPlot(xlim=xlim,
                   ylim=ylim,
                   xlabel="Time of Day",xticks=list(breaks=seq(0,25,4),labels=paste0(seq(0,25,4),":00"),logScale=FALSE),
                   ylabel=ylabel,yticks=list(breaks=NULL,labels=NULL,logScale=FALSE),
                   top=1
      )
    }
      g<-addG(g,plotTitle(paste0("site:",input$whichSite," on ",input$whichDay)))
    
    for (time in 0:23) {
      fullresult<-getSpeeds(list(whichDay=input$whichDay,whichTime=time,whichSite=input$whichSite),data)
      if (showNumbers) {
        g<-plotNumbers(d,fullresult,time,g)
      } else {
        g<-plotBars(fullresult,volumes,time+1,filter,doPercent=doPercent,g)
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
               xlabel="Time of Day",xticks=list(breaks=seq(0,25,4),labels=paste0(seq(0,25,4),":00"),logScale=FALSE),
               ylabel="Speed",yticks=list(breaks=NULL,labels=NULL,logScale=FALSE),
               top=1
  )
    g<-addG(g,plotTitle(paste0("site:",input$whichSite," on ",input$whichDay)))
  means<-matrix(0,2,24)
  for (time in 0:23) {
    fullresult<-getSpeeds(list(whichDay=input$whichDay,whichTime=time,whichSite=input$whichSite),data)
    for (direction in 1:2) {
      for (i in 2:length(fullresult$speeds)) {
        result<-data.frame(y=c(0,0,1,1)*fullresult$speeds[i-1]+c(1,1,0,0)*fullresult$speeds[i],
                           x=time+c(-0.5,0.5,0.5,-0.5)
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
      meanSpeed<-sum(fullresult$speeds*fullresult$counts[direction,])/sum(fullresult$counts[direction,])
      means[direction,time+1]<-meanSpeed
      if (showNumbers) {
        use<-fullresult$speeds>=(fullresult$speedLimit*1.1+2)
        hmm<-sum(fullresult$counts[direction,use])
        if (direction==2) 
          g<-addG(g,dataText(data.frame(x=time,y=ylim[2]),hmm,hjust=0.5,vjust=1,colour="#c00",size=0.75,fontface="bold"))
        else
          g<-addG(g,dataText(data.frame(x=time,y=ylim[1]),hmm,hjust=0.5,colour="#c00",size=0.75,fontface="bold"))
        use<-fullresult$speeds<(fullresult$speedLimit*1.1+2) & fullresult$speeds>=fullresult$speedLimit
        hmm<-sum(fullresult$counts[direction,use])
        if (direction==2) 
          g<-addG(g,dataText(data.frame(x=time,y=ylim[2]-diff(ylim)/20),hmm,hjust=0.5,vjust=1,colour="#ca0",size=0.75,fontface="bold"))
        else
          g<-addG(g,dataText(data.frame(x=time,y=ylim[1]+diff(ylim)/20),hmm,hjust=0.5,colour="#ca0",size=0.75,fontface="bold"))
      }
    }
  }
  
  g<-addG(g,dataLine(data.frame(x=xlim,y=c(0,0)),colour="grey",linewidth=1.5))
  # g<-addG(g,dataPath(data.frame(x=0:23,y=means[1,]),colour="white"))
  # g<-addG(g,dataPath(data.frame(x=0:23,y=-means[2,]),colour="white"))
  g<-addG(g,dataText(data.frame(x=max(xlim),y=max(ylim)*0.8),label="← Westwards ←",hjust = 1,vjust=1))
  g<-addG(g,dataText(data.frame(x=min(xlim),y=min(ylim)*0.8),label="→ Eastwards →",hjust = 0,vjust=0))
  return(g)
}

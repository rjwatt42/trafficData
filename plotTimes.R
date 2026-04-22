plotTimes<-function(input,data,volume=FALSE,filter="green",showNumbers=FALSE) {
  
  xlim<-c(-1,24)
  site<-as.numeric(input$whichSite)
  
  if (volume) {
    volumes<-matrix(0,2,24)
    d<-data[[paste0("s",site)]]
    for (time in 0:23) {
      fullresult<-getSpeeds(list(whichDay=input$whichDay,whichTime=time),d$values)
      switch(filter,
             "green"={use<-1:length(fullresult$speeds)},
             "red"=use<-which(fullresult$speeds>=(d$speedLimit*1.1+2)),
             "orange"=use<-which(fullresult$speeds>=(d$speedLimit))
      )
      for (direction in 1:2) {
        volumes[direction,time+1]<-sum(fullresult$counts[direction,use])
      }
    }
    
    if (showNumbers) {
      ylim<-c(-5,5)
      g<-startPlot(xlim=xlim,
                   ylim=ylim,
                   xlabel="Time",xticks=seq(0,24,4),
                   ylabel="Volume",yticks=list(breaks=0,labels=" ",logScale=FALSE),
                   box="x",top=1
      )
    } else  {
      ylim<-c(-1,1)*max(500,max(volumes,na.rm=TRUE))
      g<-startPlot(xlim=xlim,
                   ylim=ylim,
                   xlabel="Time",xticks=seq(0,24,4),
                   ylabel="Volume",yticks=list(breaks=NULL,labels=NULL,logScale=FALSE),
                   top=1
      )
    }
    if (input$whichDay=="Average") 
      g<-addG(g,plotTitle(paste0("site:",input$whichSite," on ","average day")))
    else
      g<-addG(g,plotTitle(paste0("site:",input$whichSite," on ",input$whichDay)))
    
    for (time in 0:23) {
      fullresult<-getSpeeds(list(whichDay=input$whichDay,whichTime=time),d$values)
      if (showNumbers) {
        mins<-c(0,d$speedLimit,d$speedLimit*1.1+2)
        maxs<-c(d$speedLimit,d$speedLimit*1.1+2,100)
        cols<-c("green","orange","red")
        for (direction in 1:2) {
          sn<-(direction-1.5)
          v<-sum(fullresult$counts[direction,])
          g<-addG(g,dataText(data.frame(x=time,y=6*sn),label=format(v,digits=1),colour="white",hjust=0.5,vjust=0.5,size=0.5))
          for (i in 1:3) {
            use<-fullresult$speeds>=mins[i] & fullresult$speeds<maxs[i]
            v<-sum(fullresult$counts[direction,use])
            g<-addG(g,dataText(data.frame(x=time,y=i*sn),label=format(v,digits=1),colour=cols[i],hjust=0.5,vjust=0.5,size=0.5))
          }
        }
      } else {
        v<-data.frame(y=-c(0,1,1,0)*volumes[1,time+1],x=c(0,0,1,1)+(time-0.5))
        g<-addG(g,dataPolygon(v,fill="purple",colour=NA))
        v<-data.frame(y=c(0,1,1,0)*volumes[2,time+1],x=c(0,0,1,1)+(time-0.5))
        g<-addG(g,dataPolygon(v,fill="purple",colour=NA))
        if (filter!="purple") {
          for (direction in 1:2) {
            fullresult<-getSpeeds(list(whichDay=input$whichDay,whichTime=time),d$values)
            use<-fullresult$speeds<(d$speedLimit+10)
            if (filter=="red") use<-use & fullresult$speeds>=(d$speedLimit)
            volumes[direction,time+1]<-sum(fullresult$counts[direction,use])
          }
          v<-data.frame(y=-c(0,1,1,0)*volumes[1,time+1],x=c(0,0,1,1)+(time-0.5))
          g<-addG(g,dataPolygon(v,fill="red",colour=NA))
          v<-data.frame(y=c(0,1,1,0)*volumes[2,time+1],x=c(0,0,1,1)+(time-0.5))
          g<-addG(g,dataPolygon(v,fill="red",colour=NA))
          if (filter!="red") {
            for (direction in 1:2) {
              fullresult<-getSpeeds(list(whichDay=input$whichDay,whichTime=time),d$values)
              use<-fullresult$speeds<(d$speedLimit*1.1+2)
              if (filter=="orange") use<-use & fullresult$speeds>=(d$speedLimit)
              volumes[direction,time+1]<-sum(fullresult$counts[direction,use])
            }
            v<-data.frame(y=-c(0,1,1,0)*volumes[1,time+1],x=c(0,0,1,1)+(time-0.5))
          g<-addG(g,dataPolygon(v,fill="orange",colour=NA))
          v<-data.frame(y=c(0,1,1,0)*volumes[2,time+1],x=c(0,0,1,1)+(time-0.5))
          g<-addG(g,dataPolygon(v,fill="orange",colour=NA))
          if (filter!="orange") {
            for (direction in 1:2) {
              fullresult<-getSpeeds(list(whichDay=input$whichDay,whichTime=time),d$values)
              use<-fullresult$speeds<d$speedLimit
              volumes[direction,time+1]<-sum(fullresult$counts[direction,use])
            }
            v<-data.frame(y=-c(0,1,1,0)*volumes[1,time+1],x=c(0,0,1,1)+(time-0.5))
            g<-addG(g,dataPolygon(v,fill="green",colour=NA))
            v<-data.frame(y=c(0,1,1,0)*volumes[2,time+1],x=c(0,0,1,1)+(time-0.5))
            g<-addG(g,dataPolygon(v,fill="green",colour=NA))
          }
          }
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
               xlabel="Time",xticks=seq(0,24,4),
               ylabel="Speed",yticks=list(breaks=NULL,labels=NULL,logScale=FALSE),
               top=1
  )
  if (input$whichDay=="Average") 
    g<-addG(g,plotTitle(paste0("site:",input$whichSite," on ","average day")))
  else
    g<-addG(g,plotTitle(paste0("site:",input$whichSite," on ",input$whichDay)))
  d<-data[[paste0("s",site)]]
  means<-matrix(0,2,24)
  for (time in 0:23) {
    fullresult<-getSpeeds(list(whichDay=input$whichDay,whichTime=time),d$values)
    for (direction in 1:2) {
      for (i in 2:length(fullresult$speeds)) {
        result<-data.frame(y=c(0,0,1,1)*fullresult$speeds[i-1]+c(1,1,0,0)*fullresult$speeds[i],
                           x=time+c(-0.5,0.5,0.5,-0.5)
        )
        if (direction==1) result$y<- - result$y
        if (fullresult$speeds[i-1]<d$speedLimit) col<-"green"
        else {
          if (fullresult$speeds[i-1]<d$speedLimit*1.1+2) col<-"orange"
          else {
            if (fullresult$speeds[i-1]<d$speedLimit+10) col<-"red"
            else col<-"purple"
          }
        }
        g<-addG(g,dataPolygon(result,colour=NA,fill=col,alpha=fullresult$counts[direction,i-1]/100))
      }
      meanSpeed<-sum(fullresult$speeds*fullresult$counts[direction,])/sum(fullresult$counts[direction,])
      means[direction,time+1]<-meanSpeed
      if (showNumbers) {
        use<-fullresult$speeds>=(d$speedLimit*1.1+2)
        hmm<-sum(fullresult$counts[direction,use])
        if (direction==2) 
          g<-addG(g,dataText(data.frame(x=time,y=ylim[2]),hmm,hjust=0.5,vjust=1,colour="#c00",size=0.75,fontface="bold"))
        else
          g<-addG(g,dataText(data.frame(x=time,y=ylim[1]),hmm,hjust=0.5,colour="#c00",size=0.75,fontface="bold"))
        use<-fullresult$speeds<(d$speedLimit*1.1+2) & fullresult$speeds>=d$speedLimit
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
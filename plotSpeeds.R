plotSpeeds<-function(input,data,fixedLimits=NA,filter="green",volume=FALSE,showNumbers=FALSE) {
  
  fullresult<-getSpeeds(input,data)

  if (volume) {
    volumes<-matrix(0,2,1)
    site<-1
      fullresult<-getSpeeds(input,data)
      switch(filter,
             "green"={use<-1:length(fullresult$speeds)},
             "black"=use<-which(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"black")),
             "purple"=use<-which(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"purple")),
             "red"=use<-which(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"red")),
             "orange"=use<-which(fullresult$speeds>=(fullresult$speedLimit))
      )
      for (direction in 1:2) {
        volumes[direction,1]<-sum(fullresult$counts[direction,use])
      }

      xlim<-c(-1,1)+site
      if (showNumbers) {
      ylim<-c(-5,5)
      g<-startPlot(xlim=xlim,
                   ylim=ylim,
                   xlabel="Site",xticks=list(breaks=1,labels=input$whichSite,logScale=FALSE),
                   ylabel="Volume",yticks=list(breaks=0,labels=" ",logScale=FALSE),
                   box="x",top=1
      )
    } else {
      ylim<-c(-1,1)*max(500,max(volumes,na.rm=TRUE))
      g<-startPlot(xlim=xlim,
                   ylim=ylim,
                   xlabel="Site",xticks=list(breaks=1,labels=input$whichSite,logScale=FALSE),
                   ylabel="Volume",yticks=list(breaks=NULL,labels=NULL,logScale=FALSE),
                   top=1
      )
    }
    g<-addG(g,plotTitle(paste0("site:",input$whichSite," on ",input$whichDay," at ",input$whichTime)))
    
      fullresult<-getSpeeds(input,data)
      if (showNumbers) {
        mins<-c(0,fullresult$speedLimit,fullresult$speedLimit*1.1+2)
        maxs<-c(fullresult$speedLimit,fullresult$speedLimit*1.1+2,100)
        cols<-c("green","orange","red","purple")
        for (direction in 1:2) {
          sn<-(direction-1.5)
          v<-sum(fullresult$counts[direction,])
          g<-addG(g,dataText(data.frame(x=site,y=6*sn),label=format(v,digits=1),colour="white",hjust=0.5,vjust=0.5))
          for (i in 1:4) {
            use<-fullresult$speeds>=mins[i] & fullresult$speeds<maxs[i]
            v<-sum(fullresult$counts[direction,use])
            g<-addG(g,dataText(data.frame(x=site,y=i*sn),label=format(v,digits=1),colour=cols[i],hjust=0.5,vjust=0.5))
          }
        }
      } else {
        v<-data.frame(y=-c(0,1,1,0)*volumes[1,1],x=c(0,0,1,1)+(site-0.5))
        g<-addG(g,dataPolygon(v,fill="purple",colour=NA))
        v<-data.frame(y=c(0,1,1,0)*volumes[2,1],x=c(0,0,1,1)+(site-0.5))
        g<-addG(g,dataPolygon(v,fill="purple",colour=NA))
        if (filter!="purple") {
          use<-fullresult$speeds<speedLowerBand(fullresult$speedLimit,"purple")
          if (filter=="red") use<-use & fullresult$speeds>=(fullresult$speedLimit)
          for (direction in 1:2) {
            volumes[direction,1]<-sum(fullresult$counts[direction,use])
          }
          v<-data.frame(y=-c(0,1,1,0)*volumes[1,1],x=c(0,0,1,1)+(site-0.5))
          g<-addG(g,dataPolygon(v,fill="red",colour=NA))
          v<-data.frame(y=c(0,1,1,0)*volumes[2,1],x=c(0,0,1,1)+(site-0.5))
          g<-addG(g,dataPolygon(v,fill="red",colour=NA))
          if (filter!="red") {
            use<-fullresult$speeds<speedLowerBand(fullresult$speedLimit,"red")
            if (filter=="orange") use<-use & fullresult$speeds>=(fullresult$speedLimit)
            for (direction in 1:2) {
              volumes[direction,1]<-sum(fullresult$counts[direction,use])
            }
            v<-data.frame(y=-c(0,1,1,0)*volumes[1,1],x=c(0,0,1,1)+(site-0.5))
            g<-addG(g,dataPolygon(v,fill="orange",colour=NA))
            v<-data.frame(y=c(0,1,1,0)*volumes[2,1],x=c(0,0,1,1)+(site-0.5))
            g<-addG(g,dataPolygon(v,fill="orange",colour=NA))
            if (filter!="orange") {
              use<-fullresult$speeds<fullresult$speedLimit
              for (direction in 1:2) {
              volumes[direction,1]<-sum(fullresult$counts[direction,use])
            }
            v<-data.frame(y=-c(0,1,1,0)*volumes[1,1],x=c(0,0,1,1)+(site-0.5))
            g<-addG(g,dataPolygon(v,fill="green",colour=NA))
            v<-data.frame(y=c(0,1,1,0)*volumes[2,1],x=c(0,0,1,1)+(site-0.5))
            g<-addG(g,dataPolygon(v,fill="green",colour=NA))
            }
          }
        }
      }
    g<-addG(g,dataLine(data.frame(x=xlim,y=c(0,0)),colour="grey"))
    
    g<-addG(g,dataText(data.frame(x=max(xlim),y=max(ylim)*0.8),label="← Westwards ←",hjust = 1,vjust=1))
    g<-addG(g,dataText(data.frame(x=min(xlim),y=min(ylim)*0.8),label="→ Eastwards →",hjust = 0,vjust=0))
    return(g)
  } 

  xlim<-c(0,max(fullresult$speeds)*1.05)
  
  ylim<-0
  speeds<-unique(fullresult$speeds)
  for (i in 2:length(speeds)) {
    for (direction in 1:2) {
      result<-data.frame(x=fullresult$speeds,y=fullresult$counts[direction,])
      use<-result$x>speeds[i-1] & result$x<=speeds[i]
      ylim<-max(ylim,sum(result$y[use]))
    }
  }

  if (is.na(fixedLimits) || fixedLimits==0)
    if (ylim==0) ylim<-c(-1,1) else ylim<-c(-1,1)*ylim*1.05
  else ylim<-c(-1,1)*fixedLimits
  
  g<-startPlot(xlim=xlim,
               ylim=ylim,
               xlabel="Speed",xticks=list(breaks=NULL,labels=NULL,logScale=FALSE),
               ylabel="Count",yticks=list(breaks=NULL,labels=NULL,logScale=FALSE),
               top=1
               )
  g<-addG(g,plotTitle(paste0("site:",input$whichSite," on ",input$whichDay," at ",input$whichTime)))
  for (direction in 1:2) {
    result<-data.frame(x=fullresult$speeds,y=fullresult$counts[direction,])
    if (direction==1) result$y<- -result$y
    
    speeds<-unique(fullresult$speeds)
    sL1<-20
    sL2<-30
    for (i in 2:length(speeds)) {
      use<-result$x>speeds[i-1] & result$x<=speeds[i]
      x<-c(result$x[i],result$x[i],result$x[i+1],result$x[i+1])
      y<-c(0,sum(result$y[use]),sum(result$y[use]),0)
      fill<-"black"
      if (speeds[i]<speedUpperBand(sL1,"purple")) fill<-"purple"
      if (speeds[i]<speedUpperBand(sL1,"red")) fill<-"red"
      if (speeds[i]<speedUpperBand(sL1,"orange")) fill<-"orange"
      if (speeds[i]<speedUpperBand(sL1,"green")) fill<-"green"
      g<-addG(g,dataPolygon(data.frame(x=x,y=y),colour="none",fill=fill))
      
      use<-result$x>speeds[i-1] & result$x<=speeds[i] & fullresult$speedLimit==sL2
      x<-c(result$x[i],result$x[i],result$x[i+1],result$x[i+1])
      y<-c(0,sum(result$y[use]),sum(result$y[use]),0)
      fill<-"black"
      if (speeds[i]<speedUpperBand(sL2,"purple")) fill<-"purple"
      if (speeds[i]<speedUpperBand(sL2,"red")) fill<-"red"
      if (speeds[i]<speedUpperBand(sL2,"orange")) fill<-"orange"
      if (speeds[i]<speedUpperBand(sL2,"green")) fill<-"green"
      g<-addG(g,dataPolygon(data.frame(x=x,y=y),colour="none",fill=fill))
    }
  }
  
  g<-addG(g,dataLine(data.frame(x=xlim,y=c(0,0)),colour="grey",linewidth=1.5))
  
  g<-addG(g,dataText(data.frame(x=max(xlim),y=max(ylim)*0.6),label="← Westwards ←",hjust = 1,vjust=1))
  g<-addG(g,dataText(data.frame(x=max(xlim),y=min(ylim)*0.6),label="→ Eastwards →",hjust = 1,vjust=0))
  return(g)
}

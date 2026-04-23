plotSpeeds<-function(input,data,fixedLimits=NA,filter="green",volume=FALSE,showNumbers=FALSE) {
  
  d<-data[[paste0("s",input$whichSite)]]
  fullresult<-getSpeeds(input,d$values)
  
  if (volume) {
    volumes<-matrix(0,2,1)
    site<-as.numeric(input$whichSite)
      d<-data[[paste0("s",site)]]
      fullresult<-getSpeeds(input,d$values)
      switch(filter,
             "green"={use<-1:length(fullresult$speeds)},
             "purple"=use<-which(fullresult$speeds>=(d$speedLimit+10)),
             "red"=use<-which(fullresult$speeds>=(d$speedLimit*1.1+2)),
             "orange"=use<-which(fullresult$speeds>=(d$speedLimit))
      )
      for (direction in 1:2) {
        volumes[direction,1]<-sum(fullresult$counts[direction,use])
      }

      xlim<-c(-1,1)+site
      if (showNumbers) {
      ylim<-c(-5,5)
      g<-startPlot(xlim=xlim,
                   ylim=ylim,
                   xlabel="Site",xticks=site,
                   ylabel="Volume",yticks=list(breaks=0,labels=" ",logScale=FALSE),
                   box="x",top=1
      )
    } else  {
      ylim<-c(-1,1)*max(500,max(volumes,na.rm=TRUE))
      g<-startPlot(xlim=xlim,
                   ylim=ylim,
                   xlabel="Site",xticks=site,
                   ylabel="Volume",yticks=list(breaks=NULL,labels=NULL,logScale=FALSE),
                   top=1
      )
    }         
    g<-addG(g,plotTitle(paste0("site:",input$whichSite," on ",input$whichDay," at ",input$whichTime)))
    
      d<-data[[paste0("s",site)]]
      fullresult<-getSpeeds(input,d$values)
      if (showNumbers) {
        mins<-c(0,d$speedLimit,d$speedLimit*1.1+2)
        maxs<-c(d$speedLimit,d$speedLimit*1.1+2,100)
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
          for (direction in 1:2) {
            use<-fullresult$speeds<(d$speedLimit+10)
            if (filter=="red") use<-use & fullresult$speeds>=(d$speedLimit)
            volumes[direction,1]<-sum(fullresult$counts[direction,use])
          }
          v<-data.frame(y=-c(0,1,1,0)*volumes[1,1],x=c(0,0,1,1)+(site-0.5))
          g<-addG(g,dataPolygon(v,fill="red",colour=NA))
          v<-data.frame(y=c(0,1,1,0)*volumes[2,1],x=c(0,0,1,1)+(site-0.5))
          g<-addG(g,dataPolygon(v,fill="red",colour=NA))
          if (filter!="red") {
            for (direction in 1:2) {
              use<-fullresult$speeds<(d$speedLimit*1.1+2)
              if (filter=="orange") use<-use & fullresult$speeds>=(d$speedLimit)
              volumes[direction,1]<-sum(fullresult$counts[direction,use])
            }
            v<-data.frame(y=-c(0,1,1,0)*volumes[1,1],x=c(0,0,1,1)+(site-0.5))
            g<-addG(g,dataPolygon(v,fill="orange",colour=NA))
            v<-data.frame(y=c(0,1,1,0)*volumes[2,1],x=c(0,0,1,1)+(site-0.5))
            g<-addG(g,dataPolygon(v,fill="orange",colour=NA))
            if (filter!="orange") {
            for (direction in 1:2) {
              use<-fullresult$speeds<d$speedLimit
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
  
  if (is.na(fixedLimits) || fixedLimits==0)
    if (max(fullresult$counts)==0) ylim<-c(-1,1) else ylim<-c(-1,1)*max(fullresult$counts)*1.05
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
  if (direction==1) result$y<- - result$y
  
  use1<-max(which(result$x<=d$speedLimit))-1
  for (i in 1:use1) {
    x<-c(result$x[i],result$x[i],result$x[i+1],result$x[i+1])
    y<-c(0,result$y[i],result$y[i],0)
    g<-addG(g,dataPolygon(data.frame(x=x,y=y),colour="none",fill="green"))
  }
  use2<-max(which(result$x<=(d$speedLimit*1.1+3)))-1
  for (i in (use1+1):use2) {
    x<-c(result$x[i],result$x[i],result$x[i+1],result$x[i+1])
    y<-c(0,result$y[i],result$y[i],0)
    g<-addG(g,dataPolygon(data.frame(x=x,y=y),colour="none",fill="orange"))
  }
  use3<-max(which(result$x<=(d$speedLimit+10)))-1
  for (i in (use2+1):use3) {
    x<-c(result$x[i],result$x[i],result$x[i+1],result$x[i+1])
    y<-c(0,result$y[i],result$y[i],0)
    g<-addG(g,dataPolygon(data.frame(x=x,y=y),colour="none",fill="red"))
  }
  use4<-max(which(result$x<=(1000)))-1
  for (i in (use3+1):use4) {
    x<-c(result$x[i],result$x[i],result$x[i+1],result$x[i+1])
    y<-c(0,result$y[i],result$y[i],0)
    g<-addG(g,dataPolygon(data.frame(x=x,y=y),colour="none",fill="purple"))
  }
  }
  
  g<-addG(g,dataLine(data.frame(x=xlim,y=c(0,0)),colour="grey",linewidth=1.5))
  
  g<-addG(g,dataText(data.frame(x=max(xlim),y=max(ylim)*0.6),label="← Westwards ←",hjust = 1,vjust=1))
  g<-addG(g,dataText(data.frame(x=max(xlim),y=min(ylim)*0.6),label="→ Eastwards →",hjust = 1,vjust=0))
  return(g)
}
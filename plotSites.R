plotSites<-function(input,data,volume=FALSE,filter="green",showNumbers=FALSE) {
  
  xlim<-c(0,10)
  
  if (input$whichDay=="Average") title<-"Average day at "
  else title<-paste0(input$whichDay," at ")
  if (input$whichTime=="Average") title<-paste0(title,"average time")
  else title<-paste0(title,input$whichTime)
  
  
  if (volume) {
    volumes<-matrix(0,2,9)
    for (site in 1:9) {
      d<-data[[paste0("s",site)]]
      fullresult<-getSpeeds(input,d$values)
      switch(filter,
             "green"={use<-rep(TRUE,length(fullresult$speeds))},
             "purple"=use<-(fullresult$speeds>=(d$speedLimit+10)),
             "red"=use<-(fullresult$speeds>=(d$speedLimit*1.1+2)),
             "orange"=use<-(fullresult$speeds>=(d$speedLimit))
      )
      for (direction in 1:2) {
        volumes[direction,site]<-sum(fullresult$counts[direction,use])
      }
    }
    
    if (showNumbers) {
      ylim<-c(-5,5)
      g<-startPlot(xlim=xlim,
                   ylim=ylim,
                   xlabel="Site",xticks=1:9,
                   ylabel="Volume",yticks=list(breaks=0,labels=" ",logScale=FALSE),
                   box="x",top=1
      )
    } else  {
      ylim<-c(-1,1)*max(500,max(volumes,na.rm=TRUE))
      g<-startPlot(xlim=xlim,
                   ylim=ylim,
                   xlabel="Site",xticks=1:9,
                   ylabel="Volume",yticks=list(breaks=NULL,labels=NULL,logScale=FALSE),
                   top=1
      )
    }         
    g<-addG(g,plotTitle(title))
    
    for (site in 1:9) {
      d<-data[[paste0("s",site)]]
      fullresult<-getSpeeds(input,d$values)
      if (showNumbers) {
        g<-plotNumbers(d,fullresult,site,g)
      } else {
        g<-plotBars(d,fullresult,volumes,site,filter,g)
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
               xlabel="Site",xticks=1:9,
               ylabel="Speed",yticks=list(breaks=NULL,labels=NULL,logScale=FALSE),
               top=1
  )
  g<-addG(g,plotTitle(title))
  
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
          else {
            if (fullresult$speeds[i-1]<d$speedLimit+10) col<-"red"
            else col<-"purple"
          }
        }
        g<-addG(g,dataPolygon(result,colour=NA,fill=col,alpha=fullresult$counts[direction,i-1]/100))
      }
      if (showNumbers) {
        use<-fullresult$speeds>=(d$speedLimit*1.1+2)
        hmm<-sum(fullresult$counts[direction,use])
        if (direction==2) 
          g<-addG(g,dataText(data.frame(x=site,y=ylim[2]),hmm,hjust=0.5,vjust=1,colour="#c00",fill="black",background=TRUE,size=0.75,fontface="bold"))
        else
          g<-addG(g,dataText(data.frame(x=site,y=ylim[1]),hmm,hjust=0.5,colour="#c00",fill="black",background=TRUE,size=0.75,fontface="bold"))
        use<-fullresult$speeds<(d$speedLimit*1.1+2) & fullresult$speeds>=d$speedLimit
        hmm<-sum(fullresult$counts[direction,use])
        if (direction==2) 
          g<-addG(g,dataText(data.frame(x=site,y=ylim[2]-diff(ylim)/20),hmm,hjust=0.5,vjust=1,colour="#ca0",fill="black",background=TRUE,size=0.75,fontface="bold"))
        else
          g<-addG(g,dataText(data.frame(x=site,y=ylim[1]+diff(ylim)/20),hmm,hjust=0.5,colour="#ca0",fill="black",background=TRUE,size=0.75,fontface="bold"))
      }
    }
  }
  
  g<-addG(g,dataLine(data.frame(x=xlim,y=c(0,0)),colour="grey",linewidth=1.5))
  
  g<-addG(g,dataText(data.frame(x=max(xlim),y=max(ylim)*0.8),label="← Westwards ←",hjust = 1,vjust=1))
  g<-addG(g,dataText(data.frame(x=min(xlim),y=min(ylim)*0.8),label="→ Eastwards →",hjust = 0,vjust=0))
  return(g)
}
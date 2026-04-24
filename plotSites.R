plotSites<-function(input,data,volume=FALSE,filter="green",doPercent=FALSE,showNumbers=FALSE) {
  
  xlim<-c(0,10)
  
  title<-paste0(input$whichDay," at ",input$whichTime)
  
  
  if (volume) {
    volumes<-matrix(0,2,9)
    for (site in 1:9) {
      fullresult<-getSpeeds(list(whichDay=input$whichDay,whichTime=input$whichTime,whichSite=site,whichLimit=input$whichLimit),data)
      switch(filter,
             "black"=use<-(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"black")),
             "purple"=use<-(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"purple")),
             "red"=use<-(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"red")),
             "orange"=use<-(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"orange")),
             "green"=use<-(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"green"))
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
                   ylabel="vehicles per hour",yticks=list(breaks=0,labels=" ",logScale=FALSE),
                   box="x",top=1
      )
    } else  {
      if (doPercent) {ylim<-c(-1,1)*100;ylabel<-"Percent"}
      else           {
        if (filter=="purple") ylim<-c(-1,1)*max(25,max(volumes,na.rm=TRUE)*1.1)
        else ylim<-c(-1,1)*max(100,max(volumes,na.rm=TRUE)*1.1)
        ylabel<-"vehicles per hour"
      }
      g<-startPlot(xlim=xlim,
                   ylim=ylim,
                   xlabel="Site",xticks=1:9,
                   ylabel=ylabel,yticks=list(breaks=NULL,labels=NULL,logScale=FALSE),
                   top=1
      )
    }         
    g<-addG(g,plotTitle(title))
    
    for (site in 1:9) {
      fullresult<-getSpeeds(list(whichDay=input$whichDay,whichTime=input$whichTime,whichSite=site,whichLimit=input$whichLimit),data)
      if (showNumbers) {
        g<-plotNumbers(d,fullresult,site,doPercent=doPercent,g=g)
      } else {
        g<-plotBars(fullresult,volumes,site,filter,doPercent=doPercent,g)
      }
    }
    g<-addG(g,dataLine(data.frame(x=xlim,y=c(0,0)),colour="black",linewidth=1))
    
    g<-addG(g,dataText(data.frame(x=max(xlim),y=max(ylim)-diff(ylim)*0.01),label="← Westwards ←",hjust = 1,vjust=1))
    g<-addG(g,dataText(data.frame(x=min(xlim),y=min(ylim)+diff(ylim)*0.01),label="→ Eastwards →",hjust = 0,vjust=0))
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
    fullresult<-getSpeeds(list(whichDay=input$whichDay,whichTime=input$whichTime,whichSite=site,whichLimit=input$whichLimit),data)
    for (direction in 1:2) {
      for (i in 2:length(fullresult$speeds)) {
        result<-data.frame(y=c(0,0,1,1)*fullresult$speeds[i-1]+c(1,1,0,0)*fullresult$speeds[i],
                           x=site+c(-0.5,0.5,0.5,-0.5)
        )
        if (direction==1) result$y<- - result$y
        fill<-speedFill("black")
        if (fullresult$speeds[i-1]<speedUpperBand(fullresult$speedLimit[i-1],"purple")) fill<-speedFill("purple")
        if (fullresult$speeds[i-1]<speedUpperBand(fullresult$speedLimit[i-1],"red")) fill<-speedFill("red")
        if (fullresult$speeds[i-1]<speedUpperBand(fullresult$speedLimit[i-1],"orange")) fill<-speedFill("orange")
        if (fullresult$speeds[i-1]<speedUpperBand(fullresult$speedLimit[i-1],"green")) fill<-speedFill("green")
        g<-addG(g,dataPolygon(result,colour=NA,fill=fill,alpha=fullresult$counts[direction,i-1]/100))
      }
      if (showNumbers) {
        use<-fullresult$speeds>=speedLowerBand(fullresult$speedLimit,"red")
        hmm<-sum(fullresult$counts[direction,use])
        if (direction==2) 
          g<-addG(g,dataText(data.frame(x=site,y=ylim[2]),hmm,hjust=0.5,vjust=1,colour="#c00",fill="black",background=TRUE,size=0.75,fontface="bold"))
        else
          g<-addG(g,dataText(data.frame(x=site,y=ylim[1]),hmm,hjust=0.5,colour="#c00",fill="black",background=TRUE,size=0.75,fontface="bold"))
        use<-fullresult$speeds<speedLowerBand(fullresult$speedLimit,"red") & fullresult$speeds>=fullresult$speedLimit
        hmm<-sum(fullresult$counts[direction,use])
        if (direction==2) 
          g<-addG(g,dataText(data.frame(x=site,y=ylim[2]-diff(ylim)/20),hmm,hjust=0.5,vjust=1,colour="#ca0",fill="black",background=TRUE,size=0.75,fontface="bold"))
        else
          g<-addG(g,dataText(data.frame(x=site,y=ylim[1]+diff(ylim)/20),hmm,hjust=0.5,colour="#ca0",fill="black",background=TRUE,size=0.75,fontface="bold"))
      }
    }
  }
  
  g<-addG(g,dataLine(data.frame(x=xlim,y=c(0,0)),colour="black",linewidth=1.5))
  
  g<-addG(g,dataText(data.frame(x=max(xlim),y=max(ylim)-diff(ylim)*0.01),label="← Westwards ←",hjust = 1,vjust=1))
  g<-addG(g,dataText(data.frame(x=min(xlim),y=min(ylim)+diff(ylim)*0.01),label="→ Eastwards →",hjust = 0,vjust=0))
  return(g)
}

plotSpeeds<-function(input,data,fixedLimits=NA) {
  
  d<-data[[paste0("s",input$whichSite)]]
  fullresult<-getSpeeds(input,d$values)
  
  BrawOpts(graphicsType = "HTML")
  
  xlim<-c(0,max(fullresult$speeds)*1.05)
  
  if (is.na(fixedLimits) || fixedLimits==0)
    if (max(fullresult$counts)==0) ylim<-c(-1,1) else ylim<-c(-1,1)*max(fullresult$counts)*1.05
  else ylim<-c(-1,1)*fixedLimits
  g<-startPlot(xlim=xlim,
               ylim=ylim,
               xlabel="Speed",xticks=list(breaks=NULL,labels=NULL,logScale=FALSE),
               ylabel="Count",yticks=list(breaks=NULL,labels=NULL,logScale=FALSE)
               )
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
  use3<-max(which(result$x<=(d$speedLimit+15)))-1
  for (i in (use2+1):use3) {
    x<-c(result$x[i],result$x[i],result$x[i+1],result$x[i+1])
    y<-c(0,result$y[i],result$y[i],0)
    g<-addG(g,dataPolygon(data.frame(x=x,y=y),colour="none",fill="red"))
  }
  use4<-max(which(result$x<=(1000)))-1
  for (i in (use3+1):use4) {
    x<-c(result$x[i],result$x[i],result$x[i+1],result$x[i+1])
    y<-c(0,result$y[i],result$y[i],0)
    g<-addG(g,dataPolygon(data.frame(x=x,y=y),colour="none",fill="red"))
  }
  }
  
  g<-addG(g,dataLine(data.frame(x=xlim,y=c(0,0)),colour="grey"))
  
  g<-addG(g,dataText(data.frame(x=max(xlim),y=max(ylim)*0.6),label="← Westwards ←",hjust = 1,vjust=1))
  g<-addG(g,dataText(data.frame(x=max(xlim),y=min(ylim)*0.6),label="→ Eastwards →",hjust = 1,vjust=0))
  return(g)
}
plotNumbers<-function(d,fullresult,item,doPercent=FALSE,size=0.6,g) {
  bands<-c("green","orange","red","purple","black")
  for (direction in 1:2) {
    sn<-(direction-1.5)
    v<-sum(fullresult$counts[direction,])
    g<-addG(g,dataText(data.frame(x=item,y=6*sn),label=paste0("total=",format(v,digits=1)),colour="white",
                       size=size,hjust=0.5,vjust=0.5))
    for (i in 1:5) {
      use<-fullresult$speeds>=speedLowerBand(fullresult$speedLimit,bands[i]) & 
        fullresult$speeds<speedUpperBand(fullresult$speedLimit,bands[i]) 
      v<-sum(fullresult$counts[direction,use])
      if (doPercent) {
        v<-v/sum(fullresult$counts[direction,])
      g<-addG(g,dataText(data.frame(x=item,y=i*sn),label=format(v*100,digits=1),colour=speedFill(bands[i]),
                         size=size,hjust=0.5,vjust=0.5))
      } else {
        g<-addG(g,dataText(data.frame(x=item,y=i*sn),label=format(v,digits=1),colour=speedFill(bands[i]),
                           size=size,hjust=0.5,vjust=0.5))
      }
    }
  }
  return(g)
}

plotBars<-function(fullresult,volumes,item,filter,doPercent,g) {
  
  for (direction in 1:2) {
    use1<-(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,filter))
    ybase<-sign(direction-1.5)*c(0,1,1,0)
    if (doPercent)  ygain<-100/sum(fullresult$counts[direction,use1])
    else            ygain<-1
    for (band in c("black","purple","red","orange","green")) {
      use<-use1 & fullresult$speeds<speedUpperBand(fullresult$speedLimit,band)
      volume<-sum(fullresult$counts[direction,use])
      v<-data.frame(y=ybase*volume*ygain,x=c(0,0,1,1)+(item-0.5))
      g<-addG(g,dataPolygon(v,fill=speedFill(band),colour=NA))
      if (band==filter) break
    }
  }
  return(g)
}

plotNumbers<-function(d,fullresult,item,g) {
  mins<-c(0,fullresult$speedLimit,fullresult$speedLimit*1.1+2,speedLowerBand(fullresult$speedLimit,"purple"))
  maxs<-c(fullresult$speedLimit,fullresult$speedLimit*1.1+2,speedLowerBand(fullresult$speedLimit,"purple"),100)
  cols<-c("green","orange","red","purple")
  for (direction in 1:2) {
    sn<-(direction-1.5)
    v<-sum(fullresult$counts[direction,])
    g<-addG(g,dataText(data.frame(x=item,y=6*sn),label=format(v,digits=1),colour="white",hjust=0.5,vjust=0.5))
    for (i in 1:length(mins)) {
      use<-fullresult$speeds>=mins[i] & fullresult$speeds<maxs[i]
      v<-sum(fullresult$counts[direction,use])
      g<-addG(g,dataText(data.frame(x=item,y=i*sn),label=format(v,digits=1),colour=cols[i],hjust=0.5,vjust=0.5))
    }
  }
  return(g)
}

plotBars<-function(fullresult,volumes,item,filter,doPercent,g) {
  
  for (direction in 1:2) {
    use1<-(fullresult$speeds>=speedLowerBand(fullresult$speedLimit,filter))
    ybase<-sign(direction-1.5)*c(0,1,1,0)
    if (doPercent)  ygain<-100/sum(fullresult$counts[direction,])
    else ygain<-1
    for (band in c("black","purple","red","orange","green")) {
      use<-use1 & fullresult$speeds<=speedUpperBand(fullresult$speedLimit,band)
      volume<-sum(fullresult$counts[direction,use])
      v<-data.frame(y=ybase*volume*ygain,x=c(0,0,1,1)+(item-0.5))
      g<-addG(g,dataPolygon(v,fill=band,colour=NA))
      if (band==filter) break
    }
  }
  return(g)
}

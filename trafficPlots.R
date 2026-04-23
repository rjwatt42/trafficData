plotNumbers<-function(d,fullresult,item,g) {
  mins<-c(0,fullresult$speedLimit,fullresult$speedLimit*1.1+2,fullresult$speedLimit+10)
  maxs<-c(fullresult$speedLimit,fullresult$speedLimit*1.1+2,fullresult$speedLimit+10,100)
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
    switch(filter,
           "green"={use<-rep(TRUE,length(fullresult$speeds))},
           "black"=use<-(fullresult$speeds>=(fullresult$speedLimit+20)),
           "purple"=use<-(fullresult$speeds>=(fullresult$speedLimit+10)),
           "red"=use<-(fullresult$speeds>=(fullresult$speedLimit*1.1+2)),
           "orange"=use<-(fullresult$speeds>=(fullresult$speedLimit))
    )
    ybase<-sign(direction-1.5)*c(0,1,1,0)
    if (doPercent)  ygain<-100/sum(fullresult$counts[direction,])
    else ygain<-1
    volume1<-sum(fullresult$counts[direction,use])
    v<-data.frame(y=ybase*ygain*volume1,x=c(0,0,1,1)+(item-0.5))
    g<-addG(g,dataPolygon(v,fill="black",colour=NA))
    if (filter!="black") {
      use<-use & fullresult$speeds<(fullresult$speedLimit+20)
      volume2<-sum(fullresult$counts[direction,use])
      v<-data.frame(y=ybase*volume2*ygain,x=c(0,0,1,1)+(item-0.5))
      g<-addG(g,dataPolygon(v,fill="purple",colour=NA))
      if (filter!="purple") {
        use<-use & fullresult$speeds<(fullresult$speedLimit+10)
        volume2<-sum(fullresult$counts[direction,use])
        v<-data.frame(y=ybase*volume2*ygain,x=c(0,0,1,1)+(item-0.5))
        g<-addG(g,dataPolygon(v,fill="red",colour=NA))
        if (filter!="red") {
          use<-use & fullresult$speeds<(fullresult$speedLimit*1.1+2)
          volume3<-sum(fullresult$counts[direction,use])
          v<-data.frame(y=ybase*volume3*ygain,x=c(0,0,1,1)+(item-0.5))
          g<-addG(g,dataPolygon(v,fill="orange",colour=NA))
        if (filter!="orange") {
            use<-use & fullresult$speeds<fullresult$speedLimit
            volume4<-sum(fullresult$counts[direction,use])
            v<-data.frame(y=ybase*volume4*ygain,x=c(0,0,1,1)+(item-0.5))
            g<-addG(g,dataPolygon(v,fill="green",colour=NA))
        }
        }
      }
      }
    }
  return(g)
}

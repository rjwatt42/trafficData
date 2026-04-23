plotNumbers<-function(d,fullresult,item,g) {
  mins<-c(0,d$speedLimit,d$speedLimit*1.1+2,d$speedLimit+10)
  maxs<-c(d$speedLimit,d$speedLimit*1.1+2,d$speedLimit+10,100)
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

plotBars<-function(d,fullresult,volumes,item,filter,g) {
  
  switch(filter,
         "green"={use<-rep(TRUE,length(fullresult$speeds))},
         "purple"=use<-(fullresult$speeds>=(d$speedLimit+10)),
         "red"=use<-(fullresult$speeds>=(d$speedLimit*1.1+2)),
         "orange"=use<-(fullresult$speeds>=(d$speedLimit))
  )
  
  v<-data.frame(y=-c(0,1,1,0)*volumes[1,item],x=c(0,0,1,1)+(item-0.5))
  g<-addG(g,dataPolygon(v,fill="purple",colour=NA))
  v<-data.frame(y=c(0,1,1,0)*volumes[2,item],x=c(0,0,1,1)+(item-0.5))
  g<-addG(g,dataPolygon(v,fill="purple",colour=NA))
  if (filter!="purple") {
    for (direction in 1:2) {
      use<-use & fullresult$speeds<(d$speedLimit+10)
      volumes[direction,item]<-sum(fullresult$counts[direction,use])
    }
    v<-data.frame(y=-c(0,1,1,0)*volumes[1,item],x=c(0,0,1,1)+(item-0.5))
    g<-addG(g,dataPolygon(v,fill="red",colour=NA))
    v<-data.frame(y=c(0,1,1,0)*volumes[2,item],x=c(0,0,1,1)+(item-0.5))
    g<-addG(g,dataPolygon(v,fill="red",colour=NA))
    if (filter!="red") {
      for (direction in 1:2) {
        use<-use & fullresult$speeds<(d$speedLimit*1.1+2)
        volumes[direction,item]<-sum(fullresult$counts[direction,use])
      }
      v<-data.frame(y=-c(0,1,1,0)*volumes[1,item],x=c(0,0,1,1)+(item-0.5))
      g<-addG(g,dataPolygon(v,fill="orange",colour=NA))
      v<-data.frame(y=c(0,1,1,0)*volumes[2,item],x=c(0,0,1,1)+(item-0.5))
      g<-addG(g,dataPolygon(v,fill="orange",colour=NA))
      if (filter!="orange") {
        for (direction in 1:2) {
          use<-use & fullresult$speeds<d$speedLimit
          volumes[direction,item]<-sum(fullresult$counts[direction,use])
        }
        v<-data.frame(y=-c(0,1,1,0)*volumes[1,item],x=c(0,0,1,1)+(item-0.5))
        g<-addG(g,dataPolygon(v,fill="green",colour=NA))
        v<-data.frame(y=c(0,1,1,0)*volumes[2,item],x=c(0,0,1,1)+(item-0.5))
        g<-addG(g,dataPolygon(v,fill="green",colour=NA))
      }
    }
  }
  return(g)
}
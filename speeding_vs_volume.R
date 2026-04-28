
band<-"purple"

g<-startPlot(xlim = c(0,600),ylim=c(0,100),
             xticks='auto',yticks='auto',
             xlabel="vph",ylabel="%speeding",)


for (band in c("red","purple","black")) {
  volume<-c()
  speeding<-c()
  for (site in 1:9)
  for (day in 1:7)
    for (time in 0:23) {
      input<-list(whichSite=site,whichDay=day,whichTime=time,whichLimit="auto")
      fullresult<-getSpeeds(input,maintrafficdata)
      v<-sum(fullresult$counts[1,])
      volume<-c(volume,v)
      s<-fullresult$speeds>=speedLowerBand(fullresult$speedLimit,band)
      s<-sum(fullresult$counts[1,s])/v
      speeding<-c(speeding,s)
      v<-sum(fullresult$counts[2,])
      volume<-c(volume,v)
      s<-fullresult$speeds>=speedLowerBand(fullresult$speedLimit,band)
      s<-sum(fullresult$counts[2,s])/v
      speeding<-c(speeding,s)
    }

g<-addG(g,dataPoint(data.frame(x=volume,y=speeding*100),
             fill=speedFill(band),size=2))
}
showHTML(g)

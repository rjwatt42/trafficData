getSpeeds<-function(input,data) {
  
  if (is.null(input)) input<-list(whichDay="Monday",whichTime=" 9.00",whichSite=1)
  
  if (input$whichDay=="all days") day<-1:7
  else day<-which(input$whichDay==c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  if (input$whichTime=="all times") time<-0:23
  else time<-as.numeric(input$whichTime)
  
  site<-NULL
  if (input$whichSite=="all") site<-1:9
  if (input$whichSite=="all20") site<-3:6
  if (input$whichSite=="all30") site<-c(1:2,7:9)
  if (is.null(site)) site<-as.numeric(input$whichSite)
  
  allspeeds<-matrix(seq(5,60,5),1)
  allspeeds[1]<-0
  counts<-c()
  speeds<-c()
  speedLimits<-c()
  
  for (s in site) {
    d<-data[[paste0("s",s)]]
    values<-d$values
    allcounts<-c()
    for (direction in 1:2) {
    columnoff<-(direction-1)*31
    lcounts<-0
    denoms<-0
    for (i in 1:length(day)) {
      for (j in 1:length(time)) {
        rowoff<-(day[i]-1)*30+1068
        c1<-values[rowoff+time[j]+1,columnoff+3+(1:length(allspeeds))]
        # a lot of faffing around because of missing data for site 8 (9 for me)
        # and because of asterisks in site 5 that I don't understand
        c1<-gsub("\\*","",c1)
        notAvailable<-(is.na(c1) | c1=="-")
        c1[notAvailable]<-"-1"
        c1<-as.numeric(unlist(c1))
        denoms<-denoms+(c1>=0)
        c1[c1<0]<-0
        lcounts<-lcounts+c1
      }
    }
    denoms[denoms==0]<-1
    allcounts<-rbind(allcounts,lcounts/denoms)
    }
    counts<-cbind(counts,allcounts)
    speeds<-cbind(speeds,allspeeds)
    speedLimits<-cbind(speedLimits,matrix(d$speedLimit,1,length(allspeeds)))
    # speeds<-allspeeds
    # speedLimits<-rep(d$speedLimit,length(allspeeds))
  }
  
  return(list(speeds=speeds[1,],counts=counts,speedLimit=speedLimits[1,]))
}


speedUpperBand<-function(speedLimit,band) {
  switch(band,
         "black"=use<-speedLimit+50,
         "purple"=use<-speedLimit+15,
         "red"=use<-speedLimit+10,
         "orange"=use<-speedLimit*1.1+2,
         "green"=use<-speedLimit
  )
  return(use)
}
speedLowerBand<-function(speedLimit,band) {
  switch(band,
         "black"=use<-speedLimit+15,
         "purple"=use<-speedLimit+10,
         "red"=use<-speedLimit*1.1+2,
         "orange"=use<-speedLimit,
         "green"=use<-0
  )
  return(use)
}

speedFill<-function(band) {
  switch(band,
         "black"=use<-"magenta",
         "purple"=use<-"purple",
         "red"=use<-"red",
         "orange"=use<-"orange",
         "green"=use<-"green"
  )
  return(use)
}
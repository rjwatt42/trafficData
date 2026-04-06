getSpeeds<-function(input,values) {
  
  if (input$whichDay=="Average") day<-1:7
  else day<-which(input$whichDay==c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  if (input$whichTime=="Average") time<-0:23
  else time<-as.numeric(input$whichTime)
  
  speeds<-seq(5,60,5)
  speeds[1]<-0
  
  allcounts<-c()
  for (direction in 1:2) {
    columnoff<-(direction-1)*31
    counts<-0
    denoms<-0
    for (i in 1:length(day)) {
      for (j in 1:length(time)) {
        rowoff<-(day[i]-1)*30+1068
        c1<-values[rowoff+time[j]+1,columnoff+3+(1:length(speeds))]
        # a lot of faffing around because of missing data for site 8 (9 for me)
        # and because of asterisks in site 5 that I don't understand
        c1<-gsub("\\*","",c1)
        notAvailable<-(is.na(c1) | c1=="-")
        c1[notAvailable]<-"-1"
        c1<-as.numeric(unlist(c1))
        denoms<-denoms+(c1>=0)
        c1[c1<0]<-0
        counts<-counts+c1
      }
    }
    denoms[denoms==0]<-1
    allcounts<-rbind(allcounts,counts/denoms)
  }
  # allcounts<-allcounts/length(day)/length(time)
  
  return(list(speeds=speeds,counts=allcounts))
}

getSpeeds<-function(input,values) {
  
  if (input$whichDay=="Average") day<-1:7
  else day<-which(input$whichDay==c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  if (input$whichTime=="Average") time<-0:23
  else time<-as.numeric(input$whichTime)
  
  speeds<-seq(5,60,5)
  speeds[1]<-0
  
  direction<-which(input$whichDirection==c("Eastbound","Westbound","Both"))
  
  allcounts<-c()
  for (direction in 1:2) {
    columnoff<-(direction-1)*31
    counts<-0
    for (i in 1:length(day)) {
      for (j in 1:length(time)) {
        rowoff<-(day[i]-1)*30+1068
        c<-values[rowoff+time[j]+1,columnoff+3+(1:length(speeds))]
        notAvailable<-(is.na(c) | c=="-")
        c[notAvailable]<-"0"
        counts<-counts+as.numeric(unlist(c))
      }
    }
    allcounts<-rbind(allcounts,counts)
  }
  
  return(list(speeds=speeds,counts=allcounts))
}

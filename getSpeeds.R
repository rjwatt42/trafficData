getSpeeds<-function(input,values) {
  day<-which(input$whichDay==c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday","All"))
  direction<-which(input$whichDirection==c("Eastbound","Westbound","Both"))
  time<-as.numeric(input$whichTime)

  rowoff<-(day-1)*30+1068
  columnoff<-(direction-1)*31
  
  speeds<-seq(5,60,5)
  speeds[1]<-0
  counts<-values[rowoff+time+1,columnoff+3+(1:length(speeds))]
  notAvailable<-(is.na(counts) | counts=="-")
  counts[notAvailable]<-"0"
  return(list(speeds=speeds,counts=as.numeric(counts)))
}

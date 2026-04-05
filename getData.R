getData<-function(site=1){
  
  root<-"./Data/"
  
  root<-"https://github.com/rjwatt42/trafficData/blob/dec3d4dc9074afc5d7b1bce96c236b9eeac35863/"
  url1<-paste0(root,"4570-SCO_ATC_Site",site,"_12102025.xlsx")
  p1f <- tempfile(fileext=".xlsx")
  download.file(url1, p1f, mode="wb")
  
  values<-read_xlsx(p1f,"Values",range=cellranger::cell_cols(1:300),col_names=FALSE,col_types="text")
  speedLimit<-read_xlsx(p1f,"Dashboard","U6",col_names=FALSE)
  
  return(list(values=values,speedLimit=unlist(speedLimit)))
}
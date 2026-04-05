getData<-function(site=1){
  
  if (1==1) {
    root<-"./Data/"
    filename<-paste0(root,"4570-SCO_ATC_Site",site,"_12102025.xlsx")
  } else {
    root<-"https://github.com/rjwatt42/trafficData/blob/dec3d4dc9074afc5d7b1bce96c236b9eeac35863/"
    url1<-paste0(root,"4570-SCO_ATC_Site",site,"_12102025.xlsx")
    filename <- tempfile(fileext=".xlsx")
    download.file(url1, filename, mode="wb")
  }
  
  values<-read_xlsx(filename,"Values",range=cellranger::cell_cols(1:300),col_names=FALSE,col_types="text")
  speedLimit<-read_xlsx(filename,"Dashboard","U6",col_names=FALSE)
  
  return(list(values=values,speedLimit=unlist(speedLimit)))
}
getData<-function(site=1){
  
  root<-"./"
  filename<-paste0(root,"Data/4570-SCO_ATC_Site",site,"_12102025.xlsx")
  values<-read_xlsx(filename,"Values",range=cellranger::cell_cols(1:300),col_names=FALSE,col_types="text")
  speedLimit<-read_xlsx(filename,"Dashboard","U6",col_names=FALSE)
  
  return(list(values=values,speedLimit=unlist(speedLimit)))
}
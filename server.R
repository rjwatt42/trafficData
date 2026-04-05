
source('basicPlot.R')
source('HTMLWidget.R')
source('brawOpts.R')
source('getData.R')
source('getSpeeds.R')
source('plotSpeeds.R')
source('plotSites.R')
source('plotDays.R')
source('plotTimes.R')

library('ggplot2')
library('readxl')
library('shinyjs')

# data<-NULL
openTab<-1
startTab<-0

server <- function(input, output) {
  BrawOpts(graphicsType = "HTML")
  
  openTab<<-1
  observeEvent({c(input$filter)}, { 
    if (startTab>0) openTab<<-2 
    startTab<<-1
    })
  
  if (!exists("maintrafficdata")) {
    # if (getwd()=="/Users/rogerwatt/Documents/GitHub/trafficData" 
    #     && 1==2) {
      # withProgress(message = 'Loading data', value = 0, {
      #   data<-list()
      #   for (i in 1:9) {
      #     incProgress(1/9, detail = paste("Reading part", i))
      #     d<-getData(i)
      #     data<-c(data,list(d))
      #   }
      #   names(data)<-paste0("s",1:9)
      # }
      # )
      # maintrafficdata<<-data
      # saveRDS(maintrafficdata,"./maintrafficdata.rds")
    # } else {
      url1<-"https://github.com/rjwatt42/trafficData/raw/refs/heads/main/maintrafficdata.rds"
      filename <- tempfile(fileext=".xlsx")
      download.file(url1, filename, mode="wb")
      maintrafficdata<<-readRDS(filename)
    # }
  }
  
  
  observeEvent({c(input$whichSite,input$whichDay,input$whichTime,
                  input$plotType,input$filter)}, 
    {
      if (input$plotType=="f(sites)") shinyjs::disable("whichSite")
      else shinyjs::enable("whichSite")
      if (input$plotType=="f(days)") shinyjs::disable("whichDay")
      else shinyjs::enable("whichDay")
      if (input$plotType=="f(times)") shinyjs::disable("whichTime")
      else shinyjs::enable("whichTime")
      
      switch(input$plotType,
             "single"   ={g1<-plotSpeeds(input,maintrafficdata)},
             "f(sites)" ={g1<-plotSites(input,maintrafficdata)},
             "f(days)"  ={g1<-plotDays(input,maintrafficdata)},
             "f(times)" ={g1<-plotTimes(input,maintrafficdata)}
             )
            
      switch(input$plotType,
             "single"   ={g2<-" "},
             "f(sites)" ={g2<-plotSites(input,maintrafficdata,volume=TRUE,filter=input$filter)},
             "f(days)"  ={g2<-plotDays(input,maintrafficdata,volume=TRUE,filter=input$filter)},
             "f(times)" ={g2<-plotTimes(input,maintrafficdata,volume=TRUE,filter=input$filter)}
      )
      
      g<-generate_tab("Graphs",
                      tabs=c("Speed","Volume"),
                      tabContents=c(g1,g2),
                      open=openTab) 
    output$trafficHTML <- renderUI(HTML(g))
  })
  
}
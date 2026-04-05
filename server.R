
source('basicPlot.R')
source('HTMLWidget.R')
source('brawOpts.R')
source('getData.R')
source('getSpeeds.R')
source('plotSpeeds.R')
source('plotSites.R')

library('ggplot2')
library('readxl')

# data<-NULL

server <- function(input, output) {
  BrawOpts(graphicsType = "HTML")
  
  if (!exists("maintrafficdata"))
  {
  # maindata<<-readRDS("temp.rds")
    withProgress(message = 'Loading data', value = 0, {
      data<-list()
      for (i in 1:9) {
        incProgress(1/9, detail = paste("Reading part", i))
        d<-getData(i)
        data<-c(data,list(d))
      }
      names(data)<-paste0("s",1:9)
    }
    )
    maintrafficdata<<-data
    saveRDS(maintrafficdata,"maindata.rds")
  }

  observeEvent({c(input$whichSite,input$whichDay,input$whichDirection,input$whichTime,input$whichPlot)
    }, 
    {
      plotType<-"speeds"
      if (input$whichSite=="All") plotType<-"sites"
      switch(plotType,
             "speeds"={g1<-plotSpeeds(input,maintrafficdata)},
             "sites" ={g1<-plotSites(input,maintrafficdata)}
             )
            
      
      g<-generate_tab("Graphs",
                      tabs=c("Speed","Volume"),
                      tabContents=c(g1,"to be done"),
                      open=1) 
    output$trafficHTML <- renderUI(HTML(g))
  })
  
}
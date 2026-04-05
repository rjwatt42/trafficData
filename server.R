
source('basicPlot.R')
library('ggplot2')

# data<-NULL

server <- function(input, output) {
  BrawOpts(graphicsType = "HTML")
  
  if (!exists("data"))
  {  
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
  }

  observeEvent({c(input$whichSite,input$whichDay,input$whichDirection,input$whichTime)
    }, 
    {
      d<-data[[paste0("s",input$whichSite)]]
      result<-getSpeeds(input,d$values)
      g1<-plotSpeeds(result,d$speedLimit)      
      
      g<-generate_tab("Graphs",
                      tabs=c("Speed","Volume"),
                      tabContents=c(g1,"to be done"),
                      open=1) 
    output$trafficHTML <- renderUI(HTML(g))
  })
  
}
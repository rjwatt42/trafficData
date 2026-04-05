
source('basicPlot.R')
library('ggplot2')

data<-NULL

server <- function(input, output) {
  BrawOpts(graphicsType = "HTML")
  
  if (is.null(data))
  {  
    data<<-list(s1=getData(1),s2=getData(2),s3=getData(3),
                s4=getData(4),s5=getData(5),s6=getData(6),
                s7=getData(7),s8=getData(8),s9=getData(9)
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
                      tabContents=c(g1,"bbb"),
                      open=1) 
    output$trafficHTML <- renderUI(HTML(g))
  })
  
}
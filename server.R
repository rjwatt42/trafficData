
source('basicPlot.R')
library('ggplot2')

server <- function(input, output) {
  
  observeEvent({c(input$whichDay,input$whichDirection)
    }, 
    {

      g<-generate_tab("Graphs",
                      tabs=c("Speed","Volume"),
                      tabContents=c("aaa","bbb")) 
    output$linesPlot <- renderUI(HTML(g))
  })
  
}
fontSize=10
localStyle=paste0("font-size:",format(fontSize) ,"pt;text-align: right;padding:0px;margin:0px;margin-right:5px;")
refStyle=paste0("margin-left:20px;")
refrefStyle=paste0("margin-left:40px;")
colourChoices=list("black","white","red","green","blue","yellow")

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML( # labels 
      paste0(".label {font-size: ",fontSize ,"; font-weight:bold;text-align: right;margin:0px;padding:0px;}")
    )),
    tags$style(HTML( # textInput
      paste0(".form-control {font-size: ",fontSize ,"; height:24px; padding:0px 0px;margin:0px;text-align: right;}")
    )),
    tags$style(HTML( # selectInput
      paste0(".selectize-input {font-size: ",fontSize ,"; height:12px; width:60px; padding:0px; margin-right:-10px; margin-top:-5px;margin-bottom:-5px; min-height:10px;}"),
      paste0(".selectize-dropdown { font-size: ",fontSize ,";line-height:10px}")
    )),
    tags$style(HTML( # action button
      paste0(".col-sm-3 button {font-size:",fontSize , ";font-weight:Bold;color:white; background-color: #005886;height:24px;padding:0px;padding-left:10px;padding-right:10px;margin:0px;}")
    )),
    tags$style(HTML( # well panels
      ".well {padding:2px; margin:0px;margin-bottom:8px;margin-left:0px;margin-right:0px;background-color: #eeeeee;border-radius:0} "
    )),
    tags$style(HTML( # checkbox
      ".checkbox {line-height: 10px;margin:0px;padding:0px;padding-left:4px;}"
    )),
    tags$style(HTML(
      ".table label{ display: table-cell; text-align: center;vertical-align: middle; }  .form-group { display: table-row;}"
      )),
    
    tags$style(HTML('.popup {
      position: relative;
      display: inline-block;
      cursor: pointer;
    }
      .popup .popuptext {
        visibility: hidden;
        width: 160px;
        background-color: #555;
          color: #fff;
          text-align: center;
        border-radius: 6px;
        padding: 8px 0;
        position: absolute;
        z-index: 1;
        bottom: 125%;
        left: 50%;
        margin-left: -80px;
      }
      .popup .popuptext::after {
        content: "";
        position: absolute;
        top: 100%;
        left: 50%;
        margin-left: -5px;
        border-width: 5px;
        border-style: solid;
        border-color: #555 transparent transparent transparent;
      }
      .popup .show {
        visibility: visible;
      }'
    ))
  ),

  # App title ----
  # titlePanel("Callander Traffic Speeds 12/10/2025-18/10/2025"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      style = paste("background: ",'#fff',';width:240px;height:350px',";margin-left: 0px;margin-right: -21px;margin-top: 10px;padding-right: -21px;border-color:#fff;border-radius:0px;"),
      # Input: 
      verticalLayout(
        wellPanel(tags$div(style="margin-top:0px;font-weight:bold;","Select"),
                  tags$table(width="100%",class="MyTable",
                             tags$tr(
                               tags$td(width = "70%", tags$div(style=localStyle,'site:')),
                               tags$td(width = "50%", selectInput("whichSite", NULL,format(1:9),selected = "1",selectize=FALSE))
                             ),
                             tags$tr(
                               tags$td(width = "70%", tags$div(style=localStyle,'day:')),
                               tags$td(width = "50%", selectInput("whichDay", NULL,c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday","All"),selected = "Monday",selectize=FALSE))
                             ),
                             tags$tr(
                               tags$td(width = "70%", tags$div(style=localStyle,'direction:')),
                               tags$td(width = "50%", selectInput("whichDirection", NULL,c("Eastbound","Westbound","Both"),selected = "Eastbound",selectize=FALSE))
                             ),
                             tags$tr(
                               tags$td(width = "70%", tags$div(style=localStyle,'time:')),
                               tags$td(width = "50%", selectInput("whichTime", NULL,0:23,selected = 9,selectize=FALSE))
                             )
                  )
        ),
        wellPanel(tags$div(style="font-weight:bold;",'Links to Code and Data'),
                  tags$table(width="100%",
                             tags$tr(
                               tags$td(tags$div('  ')),
                               tags$td(
                                 tags$div(HTML('Code is <a href="https://github.com/rjwatt42/trafficData"><u>here</u></a>'))),
                               ),
                             tags$tr(
                               tags$td(tags$div('  ')),
                               tags$td(
                                 tags$div(HTML('Data is <a href="https://github.com/rjwatt42/trafficData/Data/"><u>here</u></a>'))),
                             )
                  )
        )
      ),
      width=3
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      style = paste("background: ",'#FFF',';','width:526px;height:500px;',"margin: 0px;margin-top:10px;margin-left:60px;padding: 0px;border-radius:0px;"),
      
      # Output: 
      htmlOutput(outputId = "trafficHTML",width="450px",height="3500px",border="0px",margin="0px"),
      width=5
    )
  )
)

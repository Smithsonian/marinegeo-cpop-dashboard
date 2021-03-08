# An R-Shiny app to visualize and download MarineGEO CPOP data. 
# Currently the application displays the most recent water quality data from MarineGEO's LoggerNet server. 
# Ultimately, this application will allow users to display, query, and download chemical and physical sensor data including water quality and meterological data across a range of quality control levels, from raw and unprocessed to highly curated.  
# UI dashboard  

fluidPage(
  titlePanel(div(id = "header",
                 splitLayout(
                   tags$h3(id = "title_string", "Chemical and Physical Observation Program Dashboard"),
                   div(id = "header_image", tags$img(src = "MarineGEO Smithsonian logo approved cropped.png", height = "80px")),
                   cellArgs = list(style = "height:90px;")
                 ),
                 tags$style(HTML("#header_image {float:right} #title_string {margin-top:0px;}"))
                                 #header {border-bottom: 2px solid black}"))
  ),
  windowTitle = "CPOP Visualization Dashboard"),
  
div(id = "options_div",
    splitLayout(
      style = "border: 1px solid black;",
      cellArgs = list(style = "padding: 20px;"),
      
      div(
        checkboxGroupInput("site_selection", "Select sites",
                         choices = unique(index$site_code), selected = unique(index$site_code)),
        actionButton("update_plot", "Update plot")
      ),
      uiOutput("data_type"),
      
      selectInput("var_selection", "Select variables to plot",
                  choices = initial_selections, 
                  selected = first(initial_selections), multiple = TRUE),
      
      selectInput("date_interval", label = "Select a date interval", 
                  choices = c("Previous 7 days", "Previous month", "Previous 24 hours", "All data"))
    ),
    
    tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))
    
  ),

div(id = "plot_div",
    plotOutput("plot_object"))
)

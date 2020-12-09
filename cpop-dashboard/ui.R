# An R-Shiny app to visualize and download MarineGEO CPOP data. 
# Currently the application displays the most recent water quality data from MarineGEO's LoggerNet server. 
# Ultimately, this application will allow users to display, query, and download chemical and physical sensor data including water quality and meterological data across a range of quality control levels, from raw and unprocessed to highly curated.  
# UI dashboard  

fluidPage(
  titlePanel(div(id = "header",
                 splitLayout(
                   tags$h3(id = "title_string", "Chemical and Physical Observation Program Dashboard"),
                   div(id = "header_image", tags$img(src = "Logomark_MarineGEO_Tennenbaum_RGB.png", height = "50px")),
                   cellArgs = list(style = "height:60px;")
                 ),
                 tags$style(HTML("#header_image {float:right} #title_string {margin-top:0px;}
                                 #header {border-bottom: 2px solid black}"))
  ),
  windowTitle = "CPOP Visualization Dashboard"),
  
           # Module containing select input and renderPlotly call
           sensorUI("plot1", "STRI Water Quality", pan_bdt_names),
           sensorUI("plot2", "SERC Water Quality", usa_mda_names)

)

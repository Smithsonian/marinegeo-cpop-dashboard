# MarineGEO CPOP Dashboard
An R-Shiny app to visualize and download MarineGEO CPOP data. Currently the application displays the most recent water quality data from MarineGEO's LoggerNet server. Ultimately, this application will allow users to display, query, and download chemical and physical sensor data including water quality and meterological data across a range of quality control levels, from raw and unprocessed to highly curated.  

Packages:  
shiny  
plyr  
lubridate  
readr  
tidyr  
dplyr  
ggplot2    

Data is transferred at regular intervals (5-15 minutes) from a loggernet server to a /data subdirectory in the application folder. This method will be replaced by a direct connection to an Apache Hadoop installation where data is loaded from Apache Hive tables.

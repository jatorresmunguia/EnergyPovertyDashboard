###############################
#### App on poverty energy ####
###############################

### Packages ###
if(!require("shiny")) install.packages("shiny")
if(!require("DT")) install.packages("DT")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("plotly")) install.packages("plotly")
if(!require("RColorBrewer")) install.packages("RColorBrewer")
if(!require("leaflet")) install.packages("leaflet")
if(!require("readr")) install.packages("readr")

### Datasets ###
dataenergy <- read.csv(url("https://raw.githubusercontent.com/jatorresmunguia/EnergyPoverty/main/MonthlyData.csv"), sep = ";")
colnames(dataenergy) <- c("#", "Country", "iso2", "iso3", "Category", "Description", "Type of actor",
                          "Start month", "End month", "Start first extension", "End first extension", 
                          "Start second extension", "End second extension", "Start third extension", "End third extension",
                          "March 2020", "April 2020", "May 2020", "June 2020", "July 2020", 
                          "August 2020", "September 2020", "October 2020", "November 2020", 
                          "December 2020", "January 2021", "February 2021", "March 2021")
energymeasurelong <- dataenergy[, c("Country", "iso3", "Category", 
                                    "March 2020", "April 2020", "May 2020", "June 2020", "July 2020", 
                                    "August 2020", "September 2020", "October 2020", "November 2020", 
                                    "December 2020", "January 2021", "February 2021", "March 2021")] %>% 
  pivot_longer(cols = 4:16,
               names_to = "Month",
               values_to = "YesNo")

energymeasurelong$Month <- as.factor(energymeasurelong$Month)
energymeasurelong$Month <- factor(energymeasurelong$Month,
                                  levels(energymeasurelong$Month)[c(8, 1, 10, 7, 6, 2, 13, 
                                                                    12, 11, 3, 5, 4, 9)])

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

temp <- tempfile()
download.file("https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/download/?format=shp&timezone=Europe/Berlin&lang=en/world-administrative-boundaries.zip", temp)
unzip(temp)
shpsf <- rgdal::readOGR("world-administrative-boundaries.shp")

### Dashboard ###
ui <- fluidPage(
  
  # Dashboard title 
  titlePanel("COVID-19 Emergency Measures on Household Energy Services"),
  
  # First row
  fluidRow(

    ## First row -> First column
    column(

      selectInput(inputId = "measure", 
                  label = "Type of measure:", 
                  choices = c("Deferred payment arrangements" = "Deferred payment arrangements",     
                              "Disconnection bans" = "Disconnection bans",                    
                              "Discounts or subsidies for energy supply" = "Discounts or subsidies for energy supply", 
                              "Free energy supply" = "Free energy supply",                       
                              "Personalized payment arrangements" = "Personalized payment arrangements",      
                              "Reconnection of supply" = "Reconnection of supply",                 
                              "Support for off-grid energy supplies" = "Support for off-grid energy supplies",   
                              "Tariff adjustments or freezes" = "Tariff adjustments or freezes",       
                              "Other measures" = "Other measures"),
                  selected = "Deferred payment arrangements"),
      
      selectInput(inputId = "month", 
                  label = "Month:", 
                  choices = c("March 2020", "April 2020", "May 2020", "June 2020", "July 2020", 
                              "August 2020", "September 2020", "October 2020", "November 2020",
                              "December 2020", "January 2021", "February 2021", "March 2021"),
                  selected = "March 2020"),

      width = 2),

    ## First row -> Second column
    column(
      
      plotlyOutput("monthlyplot"),
      
      width = 5),

    ## First row -> Third column
    column(

      leafletOutput("map"),

      width = 5)

  ),

  # Second row
  fluidRow(
    
    DT::dataTableOutput("dataset")
    
    )
  )

server <- function(input, output){
  
  output$dataset <- DT::renderDataTable(
    
            return(
          dataenergy %>%
            filter(Description != "") %>%
            filter(Category == input$measure) %>%
            select(Country, Category, Description, 'Type of actor', 'Start month', 'End month')
        ),
    rownames = FALSE
    
    )
  
  output$monthlyplot <- renderPlotly({
    
          data <- energymeasurelong %>% 
        filter(Category == input$measure) %>% 
        group_by(Month) %>%
        summarise(sumYesNo = sum(YesNo, na.rm = TRUE)) %>%
        mutate(Month2Highlight = ifelse(Month == input$month, "yes", "no"))
      
      barplot <- ggplot(data, 
                        aes(x = Month, y = sumYesNo, fill = Month2Highlight)) +
        geom_bar(stat = "identity", position = "dodge") + 
        ylab("Number of countries") +
        theme(axis.text.x = element_text(angle = 90), legend.position = "none") +
        #ggtitle(paste("Countries implementing", tolower(input$measure), "\n as a COVID-19 emergency measure on household energy services")) +
        scale_fill_brewer(palette = "PuRd")
      
      ggplotly(barplot,
               tooltip = c("sumYesNo"))
      
     
    
  })

  output$map <- renderLeaflet({
    
      data <- energymeasurelong %>% 
        filter(Category == input$measure, Month == input$month) %>% 
        group_by(Country, iso3, Month) %>%
        summarise(sumYesNo = sum(YesNo, na.rm = TRUE)) 
      shpsf2 <- shpsf
      shpsf2@data <- shpsf2@data %>%
        left_join(data, by = "iso3") 
      shpsf2@data$sumYesNo <- as.factor(shpsf2@data$sumYesNo)
      levels(shpsf2@data$sumYesNo) <- c("No", "Yes")
      shpsf2@data$sumYesNo <- factor(shpsf2@data$sumYesNo,
                                     levels = c("Yes", "No"))
      
      labels <- sprintf("<strong>%s</strong>", shpsf2$name) %>% lapply(htmltools::HTML)
      colpal <- colorFactor(palette = rev(brewer.pal(2, "PuRd")), shpsf2$sumYesNo, na.color = "#F5F5F5")
      
      leaflet(shpsf2) %>%
        setView(25.8668772, 22.4109323, zoom = 1.3) %>%
        addTiles() %>% 
        addPolygons(fillColor = ~colpal(sumYesNo),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlightOptions = highlightOptions(
                      weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        addLegend(pal = colpal, values = ~sumYesNo, opacity = 0.9, 
                  na.label = "Missing data",
                  title = paste0("Was the measure applied during ", input$month, "?"), position = "bottomleft")
      
   
  })
}

shinyApp(ui = ui, server = server)

runGitHub("shiny_example", "rstudio")

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(dplyr)
library(leaflet)

amtrak <- read_sf('data/amtrak_rails/amtrak_rails.shp')
#rs72017
amtrak_cbsa <- read_sf("data/amtrak_cbsa/amtrak_cbsa.shp")
conus_regions <- read_sf("data/conus_regions/conus_regions.shp")

center_conus <- st_centroid(conus_regions %>% st_union()) %>%
  st_coordinates() 

pal <- colorNumeric(
  palette = c("#edf8b1", "#2c7fb8"),
  domain = amtrak_cbsa$rs72017)

m <- leaflet() %>%
  addTiles()  %>%
  addPolygons(data = amtrak_cbsa,  
              fillColor = ~pal(rs72017),
              fillOpacity = .8,
              color = 'black', 
              weight = 1,
              label = ~paste(NAME, "'s Expected Population for 2017 was", rs72017)
  ) %>%
  addLegend("bottomleft", 
            data = amtrak_cbsa,
            pal = pal, 
            values = ~rs72017,
            title = "Est. Population (2017)",
            opacity = 1
  ) %>%
  addPolylines(data = amtrak,
               color = 'red', 
               weight = 3)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("centroid",
                     "Choose Centroid",
                     choices = c('CONUS', 'South', 'Northeast', 'West', 'North Central'))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("map")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$map <- renderLeaflet({
     if(input$centroid == 'CONUS'){
       myCentroid <- center_conus
       
       myZoom = 3
     } else {
       print(input$centroid)
       print(conus_regions %>% dplyr::filter(REGION == input$centroid))
       myCentroid <- conus_regions %>%
         dplyr::filter(REGION == input$centroid) %>%
         st_centroid() %>%
         st_coordinates()
       
       myZoom <- 5
     }
     
     print(myCentroid)
      # generate bins based on input$bins from ui.R
      m %>%
        setView(center_conus[1], center_conus[2], myZoom)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)


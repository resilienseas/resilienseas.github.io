########################
#ESM 244 Final Project
#Courtney Cochran and Madi Harris
#OA Monitoring Inventory Gaps Decision Tool
########################

library(shiny)
library(leaflet)
fullinvetory <- raster("full_invetory_gaps.tif")
pal <- colorRampPalette(c("#0C2C84", "#41B6C4", "#FFFFCC"))

fullinvetorygaps <- leaflet() %>%
  addTiles() %>%
  addProviderTiles('Esri.OceanBasemap') %>% 
  addRasterImage(fullinvetory)

coordinates(oahfocus) <-~Longitude+Latitude
class(oahfocus)
writeOGR(obj= oahfocus, dsn="gap_app/gap_app", layer="inventorycoords", driver="ESRI Shapefile")


SpatialPointsDataFrame(inventorycoords)
writeOGR(obj=deduped.coords, dsn="gap_app/gap_app", layer="inventorycoords", driver="ESRI Shapefile")

leaflet(data = oahfocus) %>% 
  addTiles() %>%
  addProviderTiles('Esri.OceanBasemap') %>% 
  addCircleMarkers(
    radius = 3, 
    color = "black",
    #popup = ~as.character(), label = ~as.character()
    stroke = FALSE, fillOpacity = 0.5) 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("West Coast Ocean Acidification Monitoring Inventory: Assets and Gaps"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
    ),
    
    # Show a map of monitoring gaps (raster)
    mainPanel(
      plotOutput("gaps")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$gap_map <- renderLeaflet({
    
    leaflet() %>% 
      addTiles() %>%
      addProviderTiles('Esri.OceanBasemap') %>% 
      addRasterImage(gaps, colors = pal) %>% 
      addLegend(
        pal = pal, values = values(gaps),
        title = "Monitoring Gaps")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


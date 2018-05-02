########################
#ESM 244 Final Project
#Courtney Cochran and Madi Harris
#OA Monitoring Inventory Gaps Decision Tool
########################

library(shiny)
library(raster)
library(leaflet)
#Convert gap analysis data layers to rasters 
fullinventory <- raster("full_invetory_gaps.tif")
carbonatecomplete <- raster("carbcomplete_gaps.tif")
highfreq <- raster("highfreq_gaps.tif")

#Colors for displaying gap analysis 
pal <- colorRampPalette(c("#0C2C84", "#41B6C4", "#FFFFCC"))

#Load inventory points and convert to spatial points dataframe
oahfocus <- read_csv("oahfocus.csv")
coordinates(oahfocus) <-~Longitude+Latitude

#Create other subsets of inventory points
carbpoints<-subset(oahfocus, DisCrbPmtr>1 | ISCrbPmtr > 1)
highfreqpoints<-subset(oahfocus, MeasFreq > 364)

#Create Color Palette for Inventory Points
asset_pal <- colorFactor(topo.colors(7), oahfocus$AssetType)
oahfocus$AssetID <- as.factor(oahfocus$AssetID)
unique(oahfocus$AssetType)
class(oahfocus$AssetID)


#########################################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("West Coast Ocean Acidification Monitoring Inventory: Assets and Gaps"),
  
  #Introduction with background information
  fluidRow(column(width=3),
           p(" "),
           column(width=12, offset=3),
           h4("Introduction"),
           p("The West Coast Ocean Acidification and Hypoxia (OAH) Monitoring Inventory is a joint project of the Pacific Coast Collaborative and the Interagency Working Group on Ocean Acidification (OA). It emerged as a project to identify and catalog all monitoring assets along the West Coast that are measuring metrics of OAH. The assets are owned and operated with different priorities by different institutions, but are brought together in an attempt to analyze how well we understand and are monitoring OA trends at a regional level."),
           
           #Description of data displayed in first output
           h4("The Monitoring Inventory"),
          p("
The monitoring network consists of various assets that come in the form of moorings, buoys, cruises, and other sensors - each with various aspects of data quality (temporal frequency of measurements and quality of data). An ideal monitoring network would have high frequency of data collection and also be 'carbonate complete' meaning that aragonite saturation state can be calculated.") 
           ), 
  
  #Create radio buttons to select subset of inventory points to display
  sidebarLayout(
    sidebarPanel(
      radioButtons("pointtype", "Select:", 
                   choices= c("All Assets", "Carbonate Complete Assets", "High Frequency Assets"))
    ),
    
    mainPanel(
      leafletOutput("point_map")
    )
    
  ),
  
  #Description of data in second output
  fluidRow(column(width=12),
           h4("Gap Analysis"),
           p("A main recommendation of the West Coast OAH Panel is that a gap analysis be conducted on the combined total monitoring network. This is crucial for identifying areas of future research and priorities for expanding the network. This gap analysis shows gaps across the entire network, gaps in carbonate complete monitoring, and gaps in high frequency monitoring.")),
  
  #Create dropdown menu to select which type of gaps to display
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput("gaptype", "Type of Monitoring Gap:",
                  choices= c("Generic Gaps", "Carbonate Complete Gaps", "High Frequency Gaps" ))
      
    ),
    
    # Show a map of monitoring gaps (raster)
    mainPanel(
      leafletOutput("gap_map")
    )
  )
  
 
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Map displaying monitoring assets
  output$point_map <- renderLeaflet({
    inventorypoints <- switch(input$pointtype,
                              "All Assets" = oahfocus,
                              "Carbonate Complete Assets" = carbpoints,
                              "High Frequency Assets" = highfreqpoints)
    
    leaflet(data = inventorypoints) %>% 
      addTiles() %>%
      addProviderTiles('Esri.OceanBasemap') %>% 
      addCircleMarkers(
        radius = 6, 
        weight=7,
        color = ~asset_pal(AssetType),
        popup = paste("Project ID: ", oahfocus$ProjectID, "<br>",
                      "Organization: ", oahfocus$Orgnztion, "<br>"),
          
        stroke=FALSE, fillOpacity = 1
  ) %>% 
      addLegend("topright", pal = asset_pal, values = oahfocus$AssetType,
                title = "Asset Type")
    
  })
  
 
  
  #Map displaying gaps in monitoring netwokr
  output$gap_map <- renderLeaflet({
    
    gappal <- colorRampPalette(c("slateblue4", "slateblue", "plum", "orangered2"))
    
   gaprasters <- switch(input$gaptype, 
                     "Generic Gaps" = fullinventory,
                     "Carbonate Complete Gaps" = carbonatecomplete,
                     "High Frequency Gaps" = highfreq)
    
    leaflet() %>% 
      addTiles() %>%
      addProviderTiles('Esri.OceanBasemap') %>% 
      addRasterImage(gaprasters, colors = gappal(4)) %>% 
      addLegend("topright", colors=gappal(4), values= values(gaprasters), labels = c("Sufficient Data", "Low Priority Gaps", "High Priority Gaps", "Severe Gaps"), title = "Severity of Gap")
      
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)


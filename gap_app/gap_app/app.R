########################
#ESM 244 Final Project
#Courtney Cochran and Madi Harris
#OA Monitoring Inventory Gaps Decision Tool
########################

library(shiny)

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


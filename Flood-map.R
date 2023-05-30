library("shiny")
library("raster")
library("tmap")
library("sf")
library("nngeo")
library("ggplot2")
library("terra")


# Set up shiny app interface
ui=shinyUI(fluidPage(
  options(shiny.maxRequestSize = 600*1024^2),
  headerPanel("Flood Prediction"),
  sidebarLayout(
    
    sidebarPanel(
      helpText("Predict Flood Areas Using a DEM and Water Level"),
        
      # Create an input for the beach dem file to be placed 
      fileInput('beach_dem', 'Choose Beach DEM Layer', multiple=FALSE, accept='tif'),
      
      # Create a slider which adjusts the flood level
      sliderInput("current_water_level",
                  "Water Level (m):",
                  min = 640,
                  max = 675,
                  step = 0.05,
                  value = 640)
    ),
    # Set the main panel to be a map of the inputed beach dem
    mainPanel(
      plotOutput("mapPlot")
    )
  )))



# Define server logic required to draw a histogram
server <- function(input, output) {
  # Assign the reactive beach dem to a variable so that further analysis can be performed
  beach_dem <- reactive({input$beach_dem$datapath})
  # Assign the reactive water level input to a variable so that further analysis can be performed
  current_water_level <- reactive({input$current_water_level})
  
  
  output$mapPlot <- renderPlot({
    #Rastify the beach dem input
    beach_dem <- rast(beach_dem())
    
    #Identify areas under inputed water level
    flood_area <- beach_dem<= current_water_level()
    
    # Reclassify new raster (0 = NODATA, 1 = 1)
    ## Create reclassification matrix
    m <- c(0, NA, 1, 1)
    rcl_matrix <- matrix(m, ncol = 2, byrow = TRUE)
    
    ## Reclassify the flooded area dem
    rcl_flood_area <- classify(flood_area, rcl_matrix, include.lowest = TRUE)
    
    
    ##Plot maps
    plot(beach_dem, col = terrain.colors(n=200, rev=FALSE), axes=FALSE, main = "Predicted Flood Area")
    plot(rcl_flood_area, col= "#036ffc", axes=FALSE, add=TRUE)
  })
}



# Run the application 
shinyApp(ui = ui, server = server)

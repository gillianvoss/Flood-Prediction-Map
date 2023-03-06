library("shiny")
library("raster")
library("tmap")
library("sf")
library("nngeo")
library("ggplot2")
library("terra")



ui=shinyUI(fluidPage(
    options(shiny.maxRequestSize = 600*1024^2),
    headerPanel("Flood Prediction"),
    sidebarLayout(

        sidebarPanel(
            helpText("Predict Flood Areas Using a DEM and Water Level"),
            
            fileInput('beach_dem', 'Choose Beach DEM Layer', multiple=FALSE, accept='tif'),
            
            numericInput(
                "dem_water_level",
                "DEM Water Level",
                value = 30,
                min = 20,
                max = 50,
                step = 0.05,
                width = NULL),
            
            sliderInput("current_water_level",
                        "Water Level (m):",
                        min = 20,
                        max = 50,
                        value = 30)
            ),
            
        mainPanel(
            plotOutput("mapPlot")
        )
    )))
   
    

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    library("terra")
    
    beach_dem <- reactive(input$beach_dem$datapath)
    dem_water_level <-reactive(input$dem_water_level)
    current_water_level <- reactive(input$current_water_level)
    
    
    output$mapPlot <- renderPlot({
        
        ##Identify lowest value in DEM
        beach_dem<- rast(beach_dem)
        dem_minmax <- minmax(beach_dem)
        min_dem <- dem_minmax[1]
        # calculate rise (current - dem water level)
        rise<-(current_water_level - dem_water_level)
        
        # Raster calculator (dem <= dem+rise)
        flood_area <- beach_dem<=(min_dem+rise)
        
        # Reclassify new raster (0 = NODATA, 1 = 1)
        ## Create reclassification matrix
        m <- c(0, NA, 1, 1)
        rcl_matrix <- matrix(m, ncol = 2, byrow = TRUE)
        ## Reclassify the flooded area dem
        rcl_flood_area <- classify(flood_area, rcl_matrix, include.lowest = TRUE)
        # Map new raster overlaying dem
        plot(beach_dem, axes=FALSE, main = "Predicted Flood Area")
        plot(rcl_flood_area, col= "#036ffc", axes=FALSE, add=TRUE)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)

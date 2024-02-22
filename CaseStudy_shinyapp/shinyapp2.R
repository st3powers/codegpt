# Install required packages if not installed
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

if (!requireNamespace("spocc", quietly = TRUE)) {
  install.packages("spocc")
}

# Load libraries
library(shiny)
library(spocc)

# Define UI
ui <- fluidPage(
  titlePanel("Species Occurrence Map"),
  sidebarLayout(
    sidebarPanel(
      textInput("speciesInput", "Enter species name:", value = "Erica plena"),
      actionButton("plotButton", "Plot Map")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server
server <- function(input, output) {
  
  observeEvent(input$plotButton, {
    
    # Download occurrence data
    species_occ <- spocc::occ(query = input$speciesInput)
    
    # Remove records with NA coordinates
    species_occ <- species_occ[complete.cases(species_occ$latitude, species_occ$longitude), ]
    
    # Plot map
    output$map <- renderLeaflet({
      leaflet(species_occ) %>%
        addTiles() %>%
        addMarkers(~longitude, ~latitude, popup = ~paste("Species: ", species))
    })
  })
  
}

# Run the app
shinyApp(ui, server)

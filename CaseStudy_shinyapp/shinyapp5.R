# Install required packages if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("leaflet", quietly = TRUE)) install.packages("leaflet")
if (!requireNamespace("spocc", quietly = TRUE)) install.packages("spocc")

# Load required libraries
library(shiny)
library(leaflet)
library(spocc)

# Define UI
ui <- fluidPage(
  titlePanel("Species Occurrence Map"),
  sidebarLayout(
    sidebarPanel(
      textInput("species_input", "Enter species name:", value = "Erica plena"),
      actionButton("submit_button", "Submit")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server
server <- function(input, output) {
  
  observeEvent(input$submit_button, {
    species_name <- isolate({input$species_input})
    
    # Download species occurrence data
    occurrence_data <- suppressWarnings({
      occurrences(query = species_name, from = "gbif")
    })
    
    # Remove records with NA values for coordinates
    occurrence_data <- occurrence_data[complete.cases(occurrence_data$decimalLongitude, occurrence_data$decimalLatitude), ]
    
    # Plot the map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addMarkers(data = occurrence_data, 
                   lat = ~decimalLatitude, 
                   lng = ~decimalLongitude,
                   popup = ~paste("Species: ", species_name))
    })
  })
}

# Run the application
shinyApp(ui, server)

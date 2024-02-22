# Install required packages if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("spocc", quietly = TRUE)) install.packages("spocc")
if (!requireNamespace("leaflet", quietly = TRUE)) install.packages("leaflet")

# Load required libraries
library(shiny)
library(spocc)
library(leaflet)

# Define UI
ui <- fluidPage(
  titlePanel("Species Occurrence Map"),
  sidebarLayout(
    sidebarPanel(
      textInput("species_input", "Enter Species:", value = "Erica plena"),
      actionButton("download_button", "Download Data"),
      checkboxInput("remove_na", "Remove NA Coordinates", value = TRUE)
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Reactive values to store downloaded data
  species_data <- reactiveVal(NULL)
  
  observeEvent(input$download_button, {
    # Download data when button is clicked
    species <- input$species_input
    occurrences <- spocc::occ(species = species)
    
    if (input$remove_na) {
      # Remove records with NA values for coordinates
      occurrences <- occurrences[complete.cases(occurrences$lat, occurrences$lon), ]
    }
    
    # Update reactive values
    species_data(occurrences)
  })
  
  output$map <- renderLeaflet({
    # Render map based on downloaded data
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)
    
    data <- species_data()
    
    if (!is.null(data)) {
      map <- addMarkers(map, data = data, lat = ~lat, lng = ~lon,
                        popup = ~paste("Species: ", species_data()$species))
    }
    
    map
  })
}

# Run the Shiny app
shinyApp(ui, server)

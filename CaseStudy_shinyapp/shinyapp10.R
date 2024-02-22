# Install required packages if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("spocc", quietly = TRUE)) install.packages("spocc")
if (!requireNamespace("leaflet", quietly = TRUE)) install.packages("leaflet")

# Load libraries
library(shiny)
library(spocc)
library(leaflet)

# Define UI
ui <- fluidPage(
  titlePanel("Species Occurrence Map"),
  sidebarLayout(
    sidebarPanel(
      textInput("species", "Enter species name:", value = "Erica plena"),
      actionButton("goButton", "Go!")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$goButton, {
    # Get species data using spocc package
    species_data <- spocc::occ(query = input$species, from = "gbif", has_coords = TRUE)
    
    # Remove records with NA values for coordinates
    species_data <- species_data[complete.cases(species_data$latitude, species_data$longitude), ]
    
    # Plot the map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addMarkers(data = species_data, ~longitude, ~latitude, popup = ~paste(name, "<br>", species), clusterOptions = markerClusterOptions())
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)

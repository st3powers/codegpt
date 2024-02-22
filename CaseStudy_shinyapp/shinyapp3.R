# Install required packages if not installed
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
if (!requireNamespace("leaflet", quietly = TRUE)) {
  install.packages("leaflet")
}
if (!requireNamespace("spocc", quietly = TRUE)) {
  install.packages("spocc")
}

# Load required packages
library(shiny)
library(leaflet)
library(spocc)

# Define UI
ui <- fluidPage(
  titlePanel("Species Occurrence Map"),
  sidebarLayout(
    sidebarPanel(
      textInput("species_input", "Enter species name:", value = "Erica plena"),
      actionButton("download_button", "Download Data"),
      width = 3
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive function to download data
  downloaded_data <- eventReactive(input$download_button, {
    # Download data for the specified species
    occurrence_data <- occ(query = input$species_input, from = "gbif", has_coords = TRUE)
    
    # Remove records with NA values for coordinates
    occurrence_data <- occurrence_data[complete.cases(occurrence_data[, c("decimalLatitude", "decimalLongitude")]), ]
    
    return(occurrence_data)
  })
  
  # Update map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = downloaded_data(), 
                 ~decimalLongitude, ~decimalLatitude,
                 popup = ~paste("Species: ", species, "<br>Date: ", eventDate))
  })
}

# Run the application
shinyApp(ui, server)

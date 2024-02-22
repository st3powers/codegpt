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
      textInput("species_input", "Enter species:", value = "Erica plena"),
      actionButton("submit_btn", "Submit")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server
server <- function(input, output) {
  
  observeEvent(input$submit_btn, {
    
    # Download occurrence data using spocc
    species_data <- suppressMessages({
      get_species(input$species_input)
    })
    
    # Remove records with NA values for coordinates
    species_data <- species_data[complete.cases(species_data$coordinates), ]
    
    # Plot map
    output$map <- renderLeaflet({
      leaflet(species_data) %>%
        addTiles() %>%
        addMarkers(
          lng = ~coordinates[1],
          lat = ~coordinates[2],
          popup = ~name
        )
    })
  })
}

# Run the application
shinyApp(ui, server)

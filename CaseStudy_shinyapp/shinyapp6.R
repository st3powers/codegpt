# Install required packages if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
if (!requireNamespace("spocc", quietly = TRUE)) {
  install.packages("spocc")
}

# Load required libraries
library(shiny)
library(spocc)

# Define UI
ui <- fluidPage(
  titlePanel("Species Occurrence Map"),
  sidebarLayout(
    sidebarPanel(
      textInput("species", "Enter species:", value = "Erica plena"),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$submit, {
    species <- input$species
    
    # Download occurrence data
    occurrences <- get_occ(input$species)
    
    # Remove records with NA values for coordinates
    occurrences <- occurrences[complete.cases(occurrences$longitude, occurrences$latitude), ]
    
    # Render the map
    output$map <- renderLeaflet({
      leaflet(occurrences) %>%
        addTiles() %>%
        addMarkers(
          lng = ~longitude,
          lat = ~latitude,
          popup = ~paste("Species: ", species)
        )
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)

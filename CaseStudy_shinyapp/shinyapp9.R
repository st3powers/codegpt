# Install required packages if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

if (!requireNamespace("spocc", quietly = TRUE)) {
  install.packages("spocc")
}

if (!requireNamespace("leaflet", quietly = TRUE)) {
  install.packages("leaflet")
}

# Load libraries
library(shiny)
library(spocc)
library(leaflet)

# Define UI
ui <- fluidPage(
  titlePanel("Species Occurrence Map"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("speciesInput", "Enter species:", value = "Erica plena"),
      actionButton("plotButton", "Plot Map")
    ),
    mainPanel(
      leafletOutput("occurrenceMap")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$plotButton, {
    species <- input$speciesInput
    
    # Download occurrence data using spocc package
    occurrences <- spocc::occ(species, from = "gbif", gbifopts = list(hasCoordinate = TRUE))
    
    # Remove records with NA values for coordinates
    occurrences <- occurrences[complete.cases(occurrences$latitude, occurrences$longitude), ]
    
    # Create leaflet map
    output$occurrenceMap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addMarkers(data = occurrences, ~longitude, ~latitude, popup = ~name)
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)

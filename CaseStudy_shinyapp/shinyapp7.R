# Install and load necessary packages
if (!requireNamespace("spocc", quietly = TRUE)) {
  install.packages("spocc")
}

if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

library(spocc)
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Species Occurrence Map"),
  sidebarLayout(
    sidebarPanel(
      textInput("speciesInput", "Enter species name:", value = "Erica plena"),
      actionButton("submitBtn", "Submit")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$submitBtn, {
    isolate({
      # Get species data using spocc package
      species_data <- spocc::occ(query = input$speciesInput)
      
      # Remove records with NA values for coordinates
      species_data <- na.omit(species_data)
      
      # Create leaflet map
      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addMarkers(data = species_data, ~decimalLongitude, ~decimalLatitude,
                     popup = ~paste("Species: ", species))
      })
    })
  })
}

# Run the Shiny app
shinyApp(ui, server)

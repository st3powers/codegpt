library(shiny)
library(spocc)
library(leaflet)

# Define UI
ui <- fluidPage(
  titlePanel("Species Occurrence Mapper"),
  sidebarLayout(
    sidebarPanel(
      textInput("species", "Species:", "Erica plena"),
      actionButton("plotButton", "Plot")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output) {
  observeEvent(input$plotButton, {
    species_name <- input$species
    
    # Function to get occurrence data
    get_occurrence_data <- function(species) {
      occurrences <- occ(query = species, from = "gbif", has_coords = TRUE)
      return(occurrences)
    }
    
    # Filter NA coordinates
    filter_coordinates <- function(data) {
      data <- data[complete.cases(data$decimalLongitude, data$decimalLatitude), ]
      return(data)
    }
    
    # Plot map
    output$map <- renderLeaflet({
      species_data <- filter_coordinates(get_occurrence_data(species_name))
      leaflet(species_data) %>% addTiles() %>%
        addMarkers(~decimalLongitude, ~decimalLatitude, popup = ~paste("Species:", species_name))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

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

# Load required libraries
library(shiny)
library(spocc)
library(leaflet)

# Define UI
ui <- fluidPage(
  titlePanel("Species Occurrence Map"),
  sidebarLayout(
    sidebarPanel(
      textInput("species", "Enter species name:", value = "Erica plena"),
      actionButton("updateBtn", "Update Map")
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Reactive function to get occurrence data
  get_occurrence_data <- reactive({
    species_name <- input$species
    occurrences <- spocc::occ(query = species_name)
    occurrences_df <- occurrences$data
    occurrences_df <- na.omit(occurrences_df[c("latitude", "longitude")])
    return(occurrences_df)
  })
  
  # Update map based on user input
  observeEvent(input$updateBtn, {
    output$map <- renderLeaflet({
      occurrences_df <- get_occurrence_data()
      leaflet() %>%
        addTiles() %>%
        addMarkers(data = occurrences_df)
    })
  })
  
  # Initialize map with default species
  output$map <- renderLeaflet({
    occurrences_df <- get_occurrence_data()
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = occurrences_df)
  })
}

# Run the Shiny app
shinyApp(ui, server)

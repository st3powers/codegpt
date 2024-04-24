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
#    occurrences <- spocc::occ(species = species)
    occurrences <- spocc::occ(query = species,from ='gbif',limit=5)
    print("got occurrences")
    print(occurrences)
    occurrences<-occurrences$gbif$data %>% as.data.frame()
    speciesprefix<-paste(input$species_input,".",sep="")
    speciesprefix<-gsub(" ","_",speciesprefix)
    print(speciesprefix)
    print("okay")
    names(occurrences)<-gsub(speciesprefix,"",names(occurrences))
    print("gotten")
    
    if (input$remove_na) {
      # Remove records with NA values for coordinates
#      occurrences <- occurrences[complete.cases(occurrences$lat, occurrences$lon), ]
      if(!is.null(occurrences) & length(occurrences)>0){
        print("length(occurrences[,1])>0")
      occurrences <- occurrences[complete.cases(occurrences$latitude, occurrences$longitude), ]
      }
    }
    
    # Update reactive values
    print("length(occurrences[,1]")
    print(length(occurrences))
#    if(!is.null(occurrences) & length(occurrences>0)){
    print("update reactive data")
    species_data(occurrences)
#    }
  })
  
  output$map <- renderLeaflet({
    # Render map based on downloaded data
    print("generate leaflet base map")
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)
    
    data <- species_data()
#    print(data)
    
    print("add markers to leaflet")
#    if (!is.null(data)) {
#    print(length(data[,1]))
    condition<-!is.null(data) & length(data)>0
   print("condition")
    print(condition)
#    print(data)
#    print(length(data[,1]))
    
    if(condition==FALSE){ # reset basemap when no results found
      print("redo base map")
      map <- leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)
    }
    
    if (condition==TRUE) {
      print("update map")
      print(names(data))
      print(data)
      map <- addMarkers(map, data = data, lat = ~latitude, lng = ~longitude)#,
#                        popup = ~paste("Species: ", species_data()$name))
    }
    
    map
  })
}

# Run the Shiny app
shinyApp(ui, server)

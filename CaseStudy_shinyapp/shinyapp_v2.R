# Install required packages if not already installed
#if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
#if (!requireNamespace("spocc", quietly = TRUE)) install.packages("spocc")
#if (!requireNamespace("leaflet", quietly = TRUE)) install.packages("leaflet")
#if (!requireNamespace("ldata.table", quietly = TRUE)) install.packages("data.table")

# Load required libraries
library(shiny)
library(spocc)
library(leaflet)
#library(data.table)
library(DT)
library(shinydashboard)
library(shinyjs)
library(dplyr)
library(htmltools)
library(shinyFeedback)
#library(shinyWidgets)

myModalData <- function() {
  div(id = "download_all_data",
      modalDialog(downloadButton("download1","Download csv"),
                  easyClose = TRUE, title = "Download Table")
  )
}

url_GBIF_format <- a("https://www.gbif.org/",style="font-weight:bold",href="https://www.gbif.org/",
                           target="_blank")

url_spocc_format <- a("https://docs.ropensci.org/spocc/",style="font-weight:bold",href="https://docs.ropensci.org/spocc/",
                     target="_blank")

# Define UI
ui <- fluidPage(
  titlePanel("Species Occurrence Dashboard"),
  sidebarLayout(
    sidebarPanel(
#      p("This app retrieves data from GBIF- Global Biodiversity Information Facility"),#, https://www.gbif.org/"),
#      tags$a(href= "https://www.gbif.org/", "https://www.gbif.org/"),
      
      p(span("This app retrieves species location data from GBIF- Global Biodiversity Information Facility ",style="font-weight:bold"),url_GBIF_format),
      p(span('See Owens H, Barve V, Chamberlain S (2024). spocc: Interface to Species Occurrence Data Sources. R package version 1.2.3', url_spocc_format)),
      p(),
      fluidRow(
        column(width = 8,
      textInput("species_input", "Type species scientific name:", value = "Alligator mississippiensis"),
      numericInput("recordlimit", "Set record limit", value = 500)
               )),
#      actionButton("download_button", "Get data")#,
      loadingButton("download_button", "Get data")#,
#      checkboxInput("remove_na", "Remove NA Coordinates", value = FALSE)
    ),
    mainPanel(
      leafletOutput("map"),
      
      textOutput(outputId="message_largetable"),
      fluidRow(
        column(width = 12,
               box(width = NULL,
                   collapsible = TRUE,
#                   title = "Data",
                   dataTableOutput("tabledata")
               ) # close box
        ) # close column
      )
      
    )
    
      
  )
)

# Define server
server <- function(input, output) {
  
  reactive_message_largetable<-reactiveVal()
  
  # Reactive values to store downloaded data
  species_data <- reactiveVal(NULL)
  
  observeEvent(input$download_button, {
    # Download data when button is clicked
#    species <- input$species_input
    print(input$species_input)
    species <-  unlist(strsplit(input$species_input,","))
#    species[grep("* ",species)]<-substr(species[grep("* ",species)],1,
#                                        nchar(species[grep("* ",species)]))
#    species <- paste(input$species_input,collapse=",")
    print(species)
    # Revised below line, which threw an error
#    occurrences <- spocc::occ(species = species) 
#    occurrences <- spocc::occ(query = species,from ='gbif')#,limit=5) # new code
#    occurrences <- spocc::occ(query = species,from ='gbif',limit=10000) # new code
    occurrences <- spocc::occ(query = species,from ='gbif',limit=input$recordlimit)
#    occurrences <- bind_rows(occurrences$data)
#    spocc::occ(query = 'Haliaeetus leucocephalus',limit=5) 
#    spocc::occ(query = c('Haliaeetus leucocephalus','Oncorhynchus mykiss'))
    
    occurrences<- bind_rows(occurrences$gbif$data) %>% as.data.frame()
    
    # Added the below lines, to remove species name from column names. spocc::occ() puts the species name as prefix in column names.
    speciesprefix<-paste(input$species_input,".",sep="") # new code
    speciesprefix<-gsub(" ","_",speciesprefix) # new code
    names(occurrences)<-gsub(speciesprefix,"",names(occurrences))  # new code

#    if (input$remove_na) {
      # Remove records with NA values for coordinates
      # Revise the below line, which throws an error when no occurrence records are found
#      occurrences <- occurrences[complete.cases(occurrences$lat, occurrences$lon), ]

#      if(length(occurrences)>0){ # new code
#      occurrences <- occurrences[complete.cases(occurrences$latitude, occurrences$longitude), ] # new code, with column names corrected
#      } # new code
#    }
    
    # Update reactive values
    species_data(occurrences)
  })
  
  output$map <- renderLeaflet({
    # Render map based on downloaded data
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)
    
    data <- species_data()

    
    
    # The below code had a few problems
    # The if statement is error prone. Data can be a data frame with 0 columns and 0 rows, which returns FALSE when is.null(data) is called. 
    # Also, some of of the column names were incorrectly specified - latitude rather than lat, longitude rather than lon
    
    # if (!is.null(data)) {
    # map <- addMarkers(map, data = data, lat = ~lat, lng = ~lon,
    #                        popup = ~paste("Species: ", species_data()$species))
    # }    
    
    # if (!is.null(data)) { 
    if(length(data)>0) { # revised code
      
      if(nrow(data)<=1000){
        reactive_message_largetable("")
      }
      
      if(nrow(data)>1000){
        reactive_message_largetable("Large table- Only a subset of site locations are mapped above. The complete set of records is shown in the table below.")
        data<-data[1:1000,]
      }
      
      labs<-lapply(seq(nrow(data)), function(i) {
        paste0( data[i, "name"], '<br>', 
                data[i, "verbatimLocality"], '<br>', 
                "identified by ",data[i, "identifiedBy"],' on ',
                data[i, "eventDate"]) 
      })
#      map <- addMarkers(map, data = data, lat = ~lat, lng = ~lon,
      map <- addMarkers(map, data = data, lat = ~latitude, lng = ~longitude,# # revised code
#                        popup = ~paste("Species: ", species_data()$name))  # sure, but we can just call data, which is identical to species_data()
                        #label = ~paste(paste(data$name,", ",data$verbatimLocality,", identified by ", data$identifiedBy,sep=""))) # revised code
#                        label = ~paste(paste(data$name,"</p><p>",data$verbatimLocality,"</p><p>","identified by ", data$identifiedBy," on ", data$eventDate, sep=""))) # revised code
                label=lapply(labs, htmltools::HTML))  
    }
    resetLoadingButton("download_button") 
    map
    
  })
  
 
  
  output$message_largetable<- renderText({
    reactive_message_largetable()
  })
  
  output$tabledata <- renderDataTable(#{
    datatable(species_data(), 
              extensions = 'Buttons', selection = 'none',
              options = list(dom = 'Bfrtip', scrollX = TRUE, scrollCollapse = TRUE,
                             scrollY = "400px",  paging = FALSE,
                             buttons = list('copy', #'print',
                                            list(
                                              extend = 'collection',
                                              buttons = c('csv', 'excel', 'pdf'),
                                              text = 'Download')
)
)
)
)

  
#  updateLoadingButton(session, "download_button", label = "Load Data")

#  output$download1 <- downloadHandler(
#    filename = function() {
#      paste("Data_", gsub("-","_",Sys.Date()), ".csv", sep="")
#    },
#    content = function(file) {
#      write.csv(species_data(), file)
#    })
    
}

# Run the Shiny app
shinyApp(ui, server)

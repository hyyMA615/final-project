library(shiny)
library(mapsapi)
library(googleway)
library(leaflet)
library(lubridate)

ui <- fluidPage(
  textInput(inputId = "name1", label = "Origin Stop"),
  textInput(inputId = "name2", label = "Destination Stop"),
  selectInput(inputId = "transportway", label = "modes of transportation", choices = c("driving", "walking", "bicycling", "transit")),
  verbatimTextOutput("distancetime"),
  leafletOutput("mymap")
)

server <- function(input, output, session) {
  
  
  # Getting the directions from the Google Maps Directions API
  
  
  output$mymap <- renderLeaflet({
    key = "AIzaSyBH-3cO3XwG-uZJIxh0Pmd5W1uB48tulyE"
    
    line1 = reactive({mp_get_routes(mp_directions(
      origin = input$name1,
      destination = input$name2,
      mode = input$transportway,
      alternatives = TRUE,
      key = key,
      quiet = TRUE
    ))})
    
    palette = colorFactor(palette = "Set2", domain = line1()$alternative_id)
    
    leaflet() %>% 
    addProviderTiles(provider = providers$CartoDB.Positron) %>%
    addPolylines(data = line1(), 
                 opacity = 1, 
                 weight = 7, 
                 color = ~palette(alternative_id),
                 label = ~distance_text,
                 labelOptions = labelOptions(noHide = TRUE))
  })
  

  string1 <- reactive({paste0("Distance:", line1()$distance_text)})
  string2 <- reactive({paste0("Need time:", seconds_to_period(line1()$duration_s))})
  output$distancetime <- renderPrint({
    string1()
    string2()
  })
    }
shinyApp(ui, server)



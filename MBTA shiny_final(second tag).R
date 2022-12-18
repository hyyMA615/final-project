library(shiny)
library(mapsapi)
library(googleway)
library(leaflet)
library(lubridate)
key = "AIzaSyBH-3cO3XwG-uZJIxh0Pmd5W1uB48tulyE"

ui <- fluidPage(
  textInput(inputId = "name1", label = "Origin Stop"),
  textInput(inputId = "name2", label = "Destination Stop"),
  selectInput(inputId = "transportway", label = "modes of transportation", choices = c("driving", "walking", "bicycling", "transit")),
  textOutput("text"),
  verbatimTextOutput("distancetime"),
  leafletOutput("mymap")
)

server <- function(input, output, session) {
  
  output$text <- renderText({"Please input Origin Stop, Destination Stop, modes of transportation, then you will get result."})
  
  doc = reactive({mp_directions(
    origin = input$name1,
    destination = input$name2,
    mode = "driving",
    alternatives = TRUE,
    key = key,
    quiet = TRUE
  )})
  
  routes = reactive({mp_get_routes(doc())})
  
  palette = reactive({colorFactor(palette = "Set2", domain = routes()$alternative_id)})
  
  
  output$mymap <- renderLeaflet({
    
    leaflet() %>% 
    addProviderTiles(provider = providers$CartoDB.Positron) %>%
    addPolylines(data = routes(), 
                 opacity = 1, 
                 weight = 7, 
                 color = ~palette()(alternative_id),
                 label = ~distance_text,
                 labelOptions = labelOptions(noHide = TRUE))
  })
  
  a <- reactive({mp_get_routes(mp_directions(
    origin = input$name1,
    destination = input$name2,
    mode = input$transportway,
    alternatives = TRUE,
    key = key,
    quiet = TRUE
  ))$distance_text})
  b <- reactive({mp_get_routes(mp_directions(
    origin = input$name1,
    destination = input$name2,
    mode = input$transportway,
    alternatives = TRUE,
    key = key,
    quiet = TRUE
  ))$duration_s})
  string1 <- reactive({paste0("Distance:", a())})
  string2 <- reactive({paste0("Need time:", b())})
  output$distancetime <- renderPrint({
    string1()
    string2()
  })
    }
shinyApp(ui, server)



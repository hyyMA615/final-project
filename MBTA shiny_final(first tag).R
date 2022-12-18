library(shiny)
library(MBTAr)
library(leaflet)
library(mapsapi)

dataselectroute <- read.csv("/Users/brenda/Desktop/CS615/homework/final project/final/dataselectroute.csv")
key = "AIzaSyBH-3cO3XwG-uZJIxh0Pmd5W1uB48tulyE"

ui <- fluidPage(
    selectInput(inputId = "line", label = "Route_Name", choices = c("Green-B", "Green-C", "Green-D", "Green-E","Mattapan","Orange","Blue","Red")),
    uiOutput("select1"),
    uiOutput("select2"),
    leafletOutput("mymap1"), 
    textOutput("subwaytime")
)

server <- function(input, output, session) {

  data <- reactive({dataselectroute[dataselectroute$route_id %in% input$line,]})
  
  output$select1 <- renderUI({
    selectizeInput(inputId = "start_stop", label = "Origin Stop", choices = data()$stop_name)
  })
  output$select2 <- renderUI({
    selectizeInput(inputId = "finish_stop", label = "Destination Stop", choices = data()$stop_name)
  })
  
  #routes
  doc1 <-  reactive({mp_directions(
    origin = input$start_stop,
    destination = input$finish_stop,
    mode = "driving",
    alternatives = TRUE,
    key = key,
    quiet = TRUE
  )})
  
  line <-  reactive({mp_get_routes(doc1())})

  palette1 <-  reactive({colorFactor(palette = "Set2", domain = line()$alternative_id)})
  
  output$mymap1 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = providers$CartoDB.Positron) %>%
      addPolylines(data = line(),
                   opacity = 1,
                   weight = 7,
                   color = ~palette1()(alternative_id),
                   labelOptions = labelOptions(noHide = TRUE))
  })
  
  #compute time
  dataid1 <- reactive({
    dataselectroute[dataselectroute$stop_name %in% input$start_stop,]
  })
  dataid2 <- reactive({
    dataselectroute[dataselectroute$stop_name %in% input$finish_stop, ]
  })
  
  placetime <- reactive({Ttraveltimes(from_stop_id = dataid1()$stop_id, to_stop_id = dataid2()$stop_id, route_id = dataid1()$route_id,
                            from_datetime = (Sys.time() - 3600),
                            to_datetime = Sys.time(),
                            "wX9NwuHnZU2ToO7GmGR9uw")})
  ct <- reactive({placetime()$benchmark_travel_time_sec[1]})
  rt <- reactive({paste0("Need time:", ct())})
  output$subwaytime <- renderText({
    rt()
  })
}

shinyApp(ui, server)



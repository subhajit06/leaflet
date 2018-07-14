library(shiny)
library(leaflet)
library(zipcode)

get_lat_long <-function(zip){
  
  data("zipcode")
  k = which (zipcode$zip == zip)
  
  zip_lat_long = {}
  zip_lat_long$zip = zip
  zip_lat_long$lng = zipcode$longitude[k]
  zip_lat_long$lat = zipcode$latitude[k]
  zip_lat_long$dat = cbind(zip_lat_long$lng,zip_lat_long$lat)
  zip_lat_long$datStr = paste0("Latitide = ",zip_lat_long$lat,
                               "\n","Longitude = ",zip_lat_long$lng)
  return (zip_lat_long)      
}

ui <- fluidPage(
  
  # Application title
  titlePanel("Enter Zipcode"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      
      
      numericInput("zip", "Zipcode:", 60201, min = 1, max = 99999),
      verbatimTextOutput("value"),
      
      #numericInput('CN_high', 'CN Low or High ', 'low',
      #             min = 0, max = 1),
      
      submitButton("Show the Zipcode on the map")
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      h3(textOutput("res", container = span)),
      leafletOutput("map",  width = 600, height = 500)
      
    )
  )
)





server <- function(input, output, session) {
  
  get_data <- reactive({
    get_lat_long(input$zip)
    
  })
  
  output$res = renderText(get_data()$datStr)
  
  output$map = 
    renderLeaflet({
      info = get_data()
      leaflet() %>% setView(lng = info$lng, lat = info$lat, zoom = 11) %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
        #addMarkers(data = info$dat) 
        addPopups(info$lng, info$lat, as.character(info$zip), 
                  options = popupOptions(minWidth = 15, 
                            closeOnClick = TRUE, closeButton = TRUE))
    })
}

shinyApp(ui, server)

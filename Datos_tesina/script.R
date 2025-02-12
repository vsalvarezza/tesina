library(shiny)
library(leaflet)
library(osrm)
#library(rsconnect)
options(java.parameters = "-Xmx4G")  # Ajusta la memoria según necesidad

# Configurar el servidor OSRM local a través de Ngrok
options(osrm.server = "https://0bed-181-28-208-239.ngrok-free.app/")
options(osrm.profile = "car")


ui <- fluidPage(
  titlePanel("Shiny App con OSRM Local"),
  leafletOutput("mapa"),
  verbatimTextOutput("info")
)

server <- function(input, output, session) {
  # Coordenadas iniciales en Rosario
  punto1 <- c(-60.65762, -32.94961)  
  punto2 <- c(-60.68048, -32.94423)
  
  # Obtener la ruta
  ruta <- osrmRoute(src = punto1, dst = punto2, overview = "full")
  
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolylines(data = ruta, color = "blue") %>%
      addMarkers(lng = punto1[1], lat = punto1[2], popup = "Inicio") %>%
      addMarkers(lng = punto2[1], lat = punto2[2], popup = "Destino")
  })
  
  output$info <- renderPrint({
    ruta  # Muestra las coordenadas en la app
  })
}

shinyApp(ui, server)

library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(osrm)

# Cargar datos de calles
datos_calles <- readRDS("calles_rosario.rds")

ui <- fluidPage(
  titlePanel("Comparación de rutas OSRM: Corto vs Rápido"),
  sidebarLayout(
    sidebarPanel(
      div(
        h4("Seleccionar el tipo de recorrido:", style = "font-weight: bold; font-size: 18px; margin-bottom: 20px;"),
        checkboxGroupInput("servidores", NULL,
                           choices = c("Rápido" = "http://localhost:5012/",
                                       "Corto" = "http://localhost:5011/"))
      ),
      
      h4("Seleccionar la intersección de origen:", style = "font-weight: bold; font-size: 18px;"),
      fluidRow(
        column(6, selectInput("origen_interseccion_1",
                              label = HTML('<span style="font-weight: normal;">Calle 1:</span>'),
                              choices = c("Seleccione una calle" = "", unique(datos_calles$name)))),
        column(6, selectInput("origen_interseccion_2", 
                              label = HTML('<span style="font-weight: normal;">Calle 2:</span>'), 
                              choices = c("Seleccione una calle" = "", unique(datos_calles$name))))
      ),
      h4("Seleccionar la intersección de destino:", style = "font-weight: bold; font-size: 18px;"),
      fluidRow(
        column(6, selectInput("destino_interseccion_1", 
                              label = HTML('<span style="font-weight: normal;">Calle 1:</span>'),
                              choices = c("Seleccione una calle" = "", unique(datos_calles$name)), 
                              selected = "")),
        column(6, selectInput("destino_interseccion_2", 
                              label = HTML('<span style="font-weight: normal;">Calle 2:</span>'), 
                              choices = c("Seleccione una calle" = "", unique(datos_calles$name))))
      ),
      actionButton("generar_ruta", "Generar Ruta", class = "btn-primary")
    ),
    
    mainPanel(
      leafletOutput("mapa"),
      br(),
      uiOutput("info_rutas")
    )
  )
)

server <- function(input, output, session) {
  coords <- reactiveVal(NULL)
  
  observeEvent(input$origen_interseccion_1, {
    calle1_origen <- input$origen_interseccion_1
    
    if (calle1_origen != "") {
      geom_calle1 <- datos_calles %>%
        filter(name == calle1_origen) %>%
        st_union() %>%
        st_as_sf()
      
      calles_intersectadas <- st_intersects(geom_calle1, datos_calles, sparse = FALSE)
      idx_intersectadas <- which(calles_intersectadas[1, ])
      
      calles_cruzadas <- unique(datos_calles$name[idx_intersectadas])
      calles_cruzadas <- setdiff(calles_cruzadas, calle1_origen)
      
      updateSelectInput(session, "origen_interseccion_2",
                        choices = c("Seleccione una calle" = "", calles_cruzadas),
                        selected = "")
    } else {
      updateSelectInput(session, "origen_interseccion_2",
                        choices = c("Seleccione una calle" = "", unique(datos_calles$name)),
                        selected = "")
    }
  })
  
  observeEvent(input$destino_interseccion_1, {
    calle1_destino <- input$destino_interseccion_1
    
    if (calle1_destino != "") {
      geom_calle1 <- datos_calles %>%
        filter(name == calle1_destino) %>%
        st_union() %>%
        st_as_sf()
      
      calles_intersectadas <- st_intersects(geom_calle1, datos_calles, sparse = FALSE)
      idx_intersectadas <- which(calles_intersectadas[1, ])
      
      calles_cruzadas <- unique(datos_calles$name[idx_intersectadas])
      calles_cruzadas <- setdiff(calles_cruzadas, calle1_destino)
      
      updateSelectInput(session, "destino_interseccion_2",
                        choices = c("Seleccione una calle" = "", calles_cruzadas),
                        selected = "")
    } else {
      updateSelectInput(session, "destino_interseccion_2",
                        choices = c("Seleccione una calle" = "", unique(datos_calles$name)),
                        selected = "")
    }
  })
  
  observeEvent(input$generar_ruta, {
    interseccion1 <- st_intersection(
      datos_calles %>% filter(name == input$origen_interseccion_1),
      datos_calles %>% filter(name == input$origen_interseccion_2)
    )
    
    interseccion2 <- st_intersection(
      datos_calles %>% filter(name == input$destino_interseccion_1),
      datos_calles %>% filter(name == input$destino_interseccion_2)
    )
    
    if (nrow(interseccion1) > 0 & nrow(interseccion2) > 0) {
      coords1 <- st_coordinates(interseccion1)[1, ]
      coords2 <- st_coordinates(interseccion2)[1, ]
      coords(list(origen = coords1, destino = coords2))
    } else {
      showNotification("No se encontraron intersecciones válidas", type = "error")
      coords(NULL)
    }
  })
  
  rutas <- reactive({
    puntos <- coords()
    req(puntos)
    
    rutas_lista <- list()
    
    for (srv in input$servidores) {
      options(osrm.server = srv)
      ruta <- osrmRoute(
        src = data.frame(lon = puntos$origen[1], lat = puntos$origen[2]),
        dst = data.frame(lon = puntos$destino[1], lat = puntos$destino[2]),
        osrm.server = srv
      )
      rutas_lista[[srv]] <- ruta
    }
    
    rutas_lista
  })
  
  output$mapa <- renderLeaflet({
    puntos <- coords()
    req(puntos)
    rutas_lista <- rutas()
    
    mapa <- leaflet() %>%
      addTiles() %>%
    
    # Marcadores para inicio y fin
    leaflet::addMarkers(lng = puntos$origen[1], 
                        lat = puntos$origen[2], 
                        icon = icono_inicio, label = "Origen", 
                        labelOptions = labelOptions(noHide = TRUE, direction = "auto")) %>%
      
      leaflet::addMarkers(lng = puntos$destino[1], 
                          lat = puntos$destino[2], 
                          icon = icono_fin, label = "Destino", 
                          labelOptions = labelOptions(noHide = TRUE, direction = "auto"))
    
    for (srv in names(rutas_lista)) {
      ruta <- rutas_lista[[srv]]
      
      if (srv == "http://localhost:5012/") {
        # Ruta rápida: doble línea con borde
        mapa <- mapa %>%
          addPolylines(data = ruta, color = "#000080", weight = 7, opacity = 1) %>%  # Borde oscuro
          addPolylines(data = ruta, color = "#0000FF", weight = 4, opacity = 0.7, 
                       label = "Ruta más Rápida")
      } else if (srv == "http://localhost:5011/") {
        # Ruta corta: línea verde
        mapa <- mapa %>%
          addPolylines(data = ruta, color = "#008000", weight = 7, opacity = 1) %>%  # Borde oscuro
          addPolylines(data = ruta, color = "#00FF00", weight = 4, opacity = 0.7, 
                       label = "Ruta  más Corta")
      }
    }
    
    mapa
  })
  
  formatear_distancia <- function(distancia_km) {
    paste0(round(distancia_km, 2), " km")
  }
  
  formatear_duracion <- function(duracion_min) {
    paste0(round(duracion_min, 1), " min")
  }
  
  output$info_rutas <- renderUI({
    rutas_lista <- rutas()
    req(rutas_lista)
    
    bloques <- list()
    
    if ("http://localhost:5012/" %in% names(rutas_lista)) {
      ruta_rapida <- rutas_lista[["http://localhost:5012/"]]
      bloques <- append(bloques, list(
        HTML(paste0(
          "<div style='background: #f0f8ff; padding: 10px; border-left: 5px solid #0000FF; margin-bottom: 10px;'>",
          "<b style='color:#000080'>Ruta más rápida</b><br>",
          "Distancia: ", formatear_distancia(ruta_rapida$distance), "<br>",
          "Duración: ", formatear_duracion(ruta_rapida$duration),
          "</div>"
        ))
      ))
    }
    
    if ("http://localhost:5011/" %in% names(rutas_lista)) {
      ruta_corta <- rutas_lista[["http://localhost:5011/"]]
      bloques <- append(bloques, list(
        HTML(paste0(
          "<div style='background: #f0fff0; padding: 10px; border-left: 5px solid #00CC00; margin-bottom: 10px;'>",
          "<b style='color:#008000'>Ruta más corta</b><br>",
          "Distancia: ", formatear_distancia(ruta_corta$distance), "<br>",
          "Duración: ", formatear_duracion(ruta_corta$duration),
          "</div>"
        ))
      ))
    }
    
    do.call(tagList, bloques)
  })
  
  
}

shinyApp(ui, server)

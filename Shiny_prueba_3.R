library(shiny)
library(sf)
library(dplyr)
library(leaflet)
library(osrm)
library(bslib)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(rsconnect)

# Cargar datos de calles
datos_calles <- readRDS("calles_rosario.rds")

ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#3F6E9A"  # azul más claro (podés probar otros)
  ),
  title = div(
    style = "display: flex; align-items: center; max-height: 60px; overflow: hidden;",
    tags$img(
      src = "icono_auto2.png", 
      height = "80px", 
      style = "margin-right: 0px; margin-top: 10px;"
    ),
    h4("Elegí tu camino: priorizá tiempo o distancia", style = "margin: 0;")
  ),
  sidebar = sidebar(
    width = 500,
    useShinyjs(),
    
    # 🔽 BLOQUE CSS AGREGADO
    tags$head(
      tags$style(HTML("
    /* Caja principal del pickerInput */
    .bootstrap-select .dropdown-toggle {
      background-color: white !important;
      color: black !important;
      border: 1px solid #ced4da !important; /* gris claro */
      border-radius: 4px !important;
    }

    /* Menú desplegable */
    .bootstrap-select .dropdown-menu {
      background-color: white !important;
      color: black !important;
      border: 1px solid #ced4da !important; /* gris claro */
    }

    /* Opciones dentro del menú */
    .bootstrap-select .dropdown-menu li a {
      color: black !important;
    }

    /* Opción al pasar el mouse */
    .bootstrap-select .dropdown-menu li a:hover {
      background-color: #f8f9fa !important; /* gris muy clarito */
      color: black !important;
    }
  ")),
      tags$script(HTML("
    $(document).on('shown.bs.select', function () {
      $('.dropdown-menu.inner li a').attr('tabindex', 0);
    });

    $(document).on('keydown', '.dropdown-menu.inner li a', function(e) {
      if (e.key === 'Enter') {
        $(this).click();
      }
    });
  "))
    ),
    
    div(
      h4("Seleccionar el tipo de recorrido:", style = "font-weight: bold; font-size: 18px; margin-bottom: 20px;"),
      fluidRow(
        column(12, pickerInput(
          inputId = "tipo_recorrido",
          label = HTML('<span style="font-weight: normal;">Tipo de recorrido:</span>'),
          choices = c("Seleccione un tipo de recorrido" = "", "Corto", "Rápido", "Corto y Rápido"),
          options = list(`style` = "btn-white")
        ))
      )
    ) ,
    h4("Seleccionar la intersección de origen:", style = "font-weight: bold; font-size: 18px;"),
    fluidRow(
      column(6, pickerInput("origen_interseccion_1",
                            label = HTML('<span style="font-weight: normal;">Calle 1:</span>'),
                            choices = c("Seleccione una calle" = "", sort(unique(datos_calles$name))),
                            options = list(`live-search` = TRUE))),
      column(6, pickerInput("origen_interseccion_2", 
                            label = HTML('<span style="font-weight: normal;">Calle 2:</span>'), 
                            choices = c("Seleccione una calle" = "", sort(unique(datos_calles$name))),
                            options = list(`live-search` = TRUE)))
    ),
    h4("Seleccionar la intersección de destino:", style = "font-weight: bold; font-size: 18px;"),
    fluidRow(
      column(6, pickerInput("destino_interseccion_1", 
                            label = HTML('<span style="font-weight: normal;">Calle 1:</span>'),
                            choices = c("Seleccione una calle" = "", sort(unique(datos_calles$name))), 
                            selected = "", options = list(`live-search` = TRUE))),
      column(6, pickerInput("destino_interseccion_2", 
                            label = HTML('<span style="font-weight: normal;">Calle 2:</span>'), 
                            choices = c("Seleccione una calle" = "", sort(unique(datos_calles$name))),
                            options = list(`live-search` = TRUE)))
    ),
    actionButton("generar_ruta", 
                 label = HTML('<span class="spinner-border spinner-border-sm me-2 d-none" role="status" id="spinner-icon" style="vertical-align: middle;"></span><span>Generar Ruta</span>'), 
                 class = "btn-primary")
  ),
  layout_columns(
    layout_column_wrap(
      width = 1,
      heights_equal = "row",
      layout_columns(
        width = 9,
        card(
          style = "height: 100%;",
          withSpinner(leafletOutput("mapa", height = "400px"), type = 3, color = "#0d6efd", color.background = "white")
        )
      ),
      layout_columns(
        width = 3,
        card(
          style = "height: 100%; overflow-y: auto;",
          withSpinner(uiOutput("info_rutas"), type = 3, color = "#0d6efd", color.background = "white")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Tipo de recorrido
  servidores_seleccionados <- reactiveVal(NULL)
  
  observeEvent(input$tipo_recorrido, {
    tipo <- input$tipo_recorrido
    
    servidores <- switch(tipo,
                         "Rápido" = "http://149.50.149.229:81/",
                         "Corto" = "http://149.50.149.229/",
                         "Corto y Rápido" = c("http://149.50.149.229:81/", "http://149.50.149.229/")
    )
    
    servidores_seleccionados(servidores)
  })
  
  
  
  # Para la calle de origen
  coords <- reactiveVal(NULL)
  
  observeEvent(input$origen_interseccion_1, {
    calle1_origen <- input$origen_interseccion_1
    
    if (calle1_origen != "") {
      # Seleccionar la geometría de la calle origen
      geom_calle1 <- datos_calles %>%
        filter(name == calle1_origen) %>%
        st_union() %>%
        st_as_sf()
      
      # Obtener las calles que se cruzan con la calle seleccionada
      calles_intersectadas <- st_intersects(geom_calle1, datos_calles, sparse = FALSE)
      idx_intersectadas <- which(calles_intersectadas[1, ])
      
      # Filtrar las calles cruzadas
      calles_cruzadas <- sort(unique(datos_calles$name[idx_intersectadas]))
      calles_cruzadas <- setdiff(calles_cruzadas, calle1_origen)
      
      # Actualizar las opciones de las calles en el pickerInput
      updatePickerInput(session, "origen_interseccion_2",
                        choices = c("Seleccione una calle" = "", calles_cruzadas),
                        selected = "")
    } else {
      updatePickerInput(session, "origen_interseccion_2",
                        choices = c("Seleccione una calle" = "", sort(unique(datos_calles$name))),
                        selected = "")
    }
  })
  
  # Para la calle de destino
  observeEvent(input$destino_interseccion_1, {
    calle1_destino <- input$destino_interseccion_1
    
    if (calle1_destino != "") {
      # Seleccionar la geometría de la calle destino
      geom_calle1 <- datos_calles %>%
        filter(name == calle1_destino) %>%
        st_union() %>%
        st_as_sf()
      
      # Obtener las calles que se cruzan con la calle seleccionada
      calles_intersectadas <- st_intersects(geom_calle1, datos_calles, sparse = FALSE)
      idx_intersectadas <- which(calles_intersectadas[1, ])
      
      # Filtrar las calles cruzadas
      calles_cruzadas <- sort(unique(datos_calles$name[idx_intersectadas]))
      calles_cruzadas <- setdiff(calles_cruzadas, calle1_destino)
      
      # Actualizar las opciones de las calles en el pickerInput
      updatePickerInput(session, "destino_interseccion_2",
                        choices = c("Seleccione una calle" = "", calles_cruzadas),
                        selected = "")
    } else {
      updatePickerInput(session, "destino_interseccion_2",
                        choices = c("Seleccione una calle" = "", sort(unique(datos_calles$name))),
                        selected = "")
    }
  })
  
  loading <- reactiveVal(FALSE)
  
  observeEvent(input$generar_ruta, {
    loading(TRUE)  # Empieza a cargar
    
    runjs("document.getElementById('spinner-icon').classList.remove('d-none');")
    
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
    
    runjs("document.getElementById('spinner-icon').classList.add('d-none');")
    loading(FALSE)  # Termina la carga
  })
  
  
  
  rutas <- reactive({
    puntos <- coords()
    req(puntos)
    
    rutas_lista <- list()
    
    for (srv in servidores_seleccionados()) {
      options(osrm.server = srv)
      ruta <- tryCatch({
        osrmRoute(
          src = data.frame(lon = puntos$origen[1], lat = puntos$origen[2]),
          dst = data.frame(lon = puntos$destino[1], lat = puntos$destino[2]),
          osrm.server = srv
        )
      }, error = function(e) {
        showNotification(paste("Error al generar la ruta para el servidor", srv), type = "error")
        NULL
      })
      rutas_lista[[srv]] <- ruta
    }
    
    rutas_lista
  })
  
  
  
  output$mapa <- renderLeaflet({
    
    req(!loading())  # Espera a que no esté cargando
    puntos <- coords()
    req(puntos)
    rutas_lista <- rutas()
    
    icono_origen <- leaflet::makeIcon(
      iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png", 
      iconWidth = 25, 
      iconHeight = 41,
      iconAnchorX = 12, 
      iconAnchorY = 41
    )
    
    icono_destino <- leaflet::makeIcon(
      iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png", 
      iconWidth = 25, 
      iconHeight = 41,
      iconAnchorX = 12, 
      iconAnchorY = 41
    )
    
    mapa <- leaflet() %>%
      addTiles()
    
    for (srv in names(rutas_lista)) {
      ruta <- rutas_lista[[srv]]
      
      if (!is.null(ruta)) {
        coords_ruta <- st_coordinates(ruta)
        
        mapa <- mapa %>%
          addMarkers(lng = coords_ruta[1, 1], lat = coords_ruta[1, 2],
                     icon = icono_origen, label = "Origen",
                     labelOptions = labelOptions(noHide = TRUE, direction = "auto")) %>%
          addMarkers(lng = coords_ruta[nrow(coords_ruta), 1], lat = coords_ruta[nrow(coords_ruta), 2],
                     icon = icono_destino, label = "Destino",
                     labelOptions = labelOptions(noHide = TRUE, direction = "auto"))
        
        if (srv == "http://149.50.149.229:81/") {
          mapa <- mapa %>%
            addPolylines(data = ruta, color = "#2400D8", weight = 7, opacity = 1) %>%
            addPolylines(data = ruta, color = "#1965FF", weight = 4, opacity = 0.7,
                         label = "Ruta más Rápida")
        } else if (srv == "http://149.50.149.229/") {
          mapa <- mapa %>%
            addPolylines(data = ruta, color = "#008B00", weight = 7, opacity = 1) %>%
            addPolylines(data = ruta, color = "#32CD32", weight = 4, opacity = 0.7,
                         label = "Ruta más Corta")
        }
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
    
    req(!loading())  # Espera a que no esté cargando
    rutas_lista <- rutas()
    req(rutas_lista)
    
    bloques <- list()
    
    if ("http://149.50.149.229:81/" %in% names(rutas_lista)) {
      ruta_rapida <- rutas_lista[["http://149.50.149.229:81/"]]
      if (!is.null(ruta_rapida)) {
        bloques <- append(bloques, list(
          HTML(paste0(
            "<div style='background: #f0f8ff; padding: 10px; border-left: 5px solid #1965FF; margin-bottom: 10px;'>",
            "<b style='color:#2400D8'>Ruta más rápida</b><br>",
            "Distancia: ", formatear_distancia(ruta_rapida$distance), "<br>",
            "Duración: ", formatear_duracion(ruta_rapida$duration),
            "</div>"
          ))
        ))
      }
    }
    
    if ("http://149.50.149.229/" %in% names(rutas_lista)) {
      ruta_corta <- rutas_lista[["http://149.50.149.229/"]]
      if (!is.null(ruta_corta)) {
        bloques <- append(bloques, list(
          HTML(paste0(
            "<div style='background: #f0fff0; padding: 10px; border-left: 5px solid #32CD32; margin-bottom: 10px;'>",
            "<b style='color:#008B00'>Ruta más corta</b><br>",
            "Distancia: ", formatear_distancia(ruta_corta$distance), "<br>",
            "Duración: ", formatear_duracion(ruta_corta$duration),
            "</div>"
          ))
        ))
      }
    }
    
    do.call(tagList, bloques)
  })
}

shinyApp(ui, server)
# Librerias
options(java.parameters = "-Xmx4G")  # Ajusta el tamaño según sea necesario
#library(r5r)
library(devtools)
library(sf) # para manejar datos espaciales
library(osmdata)
library(dplyr)
library(leaflet)  # Para hacer mapas interactivos
library(osrm)

# Los datos los saque de: https://extract.bbbike.org/ que tienen solo información de Rosario


# --------------------------- OBTENER INTERSECCIONES --------------------------- 


# Descargar datos de OSM para Rosario
rosario_osm <- osmdata::opq(bbox = "Rosario, Argentina") %>%  # permite extraer datos de Rosario.
               osmdata::add_osm_feature(key = "highway") %>%  # este filtro da todas las líneas que representan calles.
               osmdata::osmdata_sf()  # descarga los datos en formato sf (Simple Features)

# Extraer las líneas (calles) y los nodos (intersecciones)
calles <- rosario_osm$osm_lines

# Primera interseccion
Nueve_julio <- calles %>% filter(name == "9 de Julio")
Santiago <- calles %>% filter(name == "Santiago")
 
# Nodos de intersección
interseccion1 <- sf::st_intersection(Nueve_julio, Santiago)

# Segunda interseccion
Alberdi <- calles %>% filter(name == "Avenida Alberdi")
Jjpaso <- calles %>% filter(name == "Avenida Juan José Paso")

# Nodos de intersección
interseccion2 <- sf::st_intersection(Alberdi, Jjpaso)

# Extraer las coordenadas de las intersecciones
coords_interseccion1 <- sf::st_coordinates(interseccion1)[1, ]
coords_interseccion2 <- sf::st_coordinates(interseccion2)[1, ]


# ----------------------------- GRÁFICAR RECORRIDO ----------------------------- 


# Define una variable para el modo de transporte ("car", "bike", o "foot")
modo_transporte <- "car"  

# Calcular la ruta usando OSRM con el perfil seleccionado
ruta <- osrm::osrmRoute(
        src = coords_interseccion1,
        dst = coords_interseccion2,
        osrm.profile = modo_transporte,  # Aplica el perfil seleccionado
        overview = "full", #  Devuelve la ruta completa con todas las coordenadas detalladas. 
)

#Función para formatear la duración en minutos o segundos
formatear_duracion <- function(duracion) {
  minutos <- duracion %/% 60  # Minutos completos
  segundos <- duracion %% 60  # Segundos restantes
  if (minutos > 0) {
    sprintf("%d minutos y %.0f segundos", minutos, segundos)
  } else {
    sprintf("%.0f segundos", segundos)
  }
}

# Función para formatear la distancia en km o metros
formatear_distancia <- function(distancia) {
  if (distancia >= 1000) {
    sprintf("%.2f km", distancia / 1000)
  } else {
    sprintf("%.0f metros", distancia)
  }
}


# Definir iconos para inicio y fin
icono_inicio <- leaflet::makeIcon(
                iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png", 
                iconWidth = 25, 
                iconHeight = 41,
                iconAnchorX = 12, 
                iconAnchorY = 41
)

icono_fin <- leaflet::makeIcon(
             iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png", 
             iconWidth = 25, 
             iconHeight = 41,
             iconAnchorX = 12, 
             iconAnchorY = 41
)

# Crear un mapa con las intersecciones y la ruta

# Ruta más corta en azul con borde oscuro
leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  
  # Ruta más corta en azul con borde oscuro
  leaflet::addPolylines(data = ruta, 
                        color = "#000080",  # Color más oscuro para el borde
                        weight = 7,  # Peso del borde
                        opacity = 1) %>% 
  leaflet::addPolylines(data = ruta, 
                        color = "#0000FF",  # Color de la línea principal
                        weight = 4, 
                        opacity = 0.7, 
                        label = "Ruta") %>% 
  
  
  # Añadir marcador para el punto de Partida con icono verde
  leaflet::addMarkers(lng = coords_interseccion1[1], 
             lat = coords_interseccion1[2], 
             icon = icono_inicio,  # Icono personalizado
             label = "Partida",  # Etiqueta para el punto de inicio
             labelOptions = labelOptions(noHide = TRUE, direction = "auto")) %>%  # Añade un marcador en el punto de inicio de la ruta
  
  # Añadir marcador para el punto de llegada con icono rojo
  leaflet::addMarkers(lng = coords_interseccion2[1], 
             lat = coords_interseccion2[2], 
             icon = icono_fin,  # Icono personalizado
             label = "Llegada",  # Etiqueta para el punto de fin
             labelOptions = labelOptions(noHide = TRUE, direction = "auto"))  %>%  # Añade un marcador en el punto de fin de la ruta.
  
  # Añadir recuadro con el tiempo y distancia recorrida
  leaflet::addControl(html = paste("<div style='background: white; padding: 5px; border: 1px solid black; color: blue'>",
                                   "<b>Ruta:</b><br>",
                                   "Distancia: ", formatear_distancia(ruta$distance), "<br>",
                                   "Duración: ", formatear_duracion(ruta$duration)), 
                      position = "bottomright") 
    
# ver de poner las opciones de ir caminando y en bicicleta también
# ver si hay una columna que indique si es una avenida
# https://excalidraw.com/ para diseñar la app


# -------------------------------- OTRA OPCIÓN -------------------------------- 


# Define una lista de modos de transporte para simular rutas rápidas y cortas
modos_transporte <- c("car", "bike")

# Crear una lista para almacenar rutas
rutas <- list()

# Calcular rutas para cada perfil y almacenar en la lista
for (modo in modos_transporte) {
  rutas[[modo]] <- osrm::osrmRoute(
                   src = coords_interseccion1,
                   dst = coords_interseccion2,
                   osrm.profile = modo,  # Cambia el perfil para aproximar distintas optimizaciones
                   overview = "full"     # Detalles completos de la ruta
  )
}

# Extraer la ruta con menor duración y menor distancia entre las rutas que se generan caminando, en auto o en bici
ruta_mas_corta <- rutas[[which.min(sapply(rutas, function(r) r$distance))]]
ruta_mas_rapida <- rutas[[which.min(sapply(rutas, function(r) r$duration))]]


# Definir íconos para inicio y fin
icono_inicio <- leaflet::makeIcon(
                iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png", 
                iconWidth = 25, iconHeight = 41,
                iconAnchorX = 12, iconAnchorY = 41
)

icono_fin <- makeIcon(
             iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png", 
             iconWidth = 25, iconHeight = 41,
             iconAnchorX = 12, iconAnchorY = 41
)

# Función para formatear la duración en minutos y segundos
formatear_duracion <- function(duracion) {
  minutos <- duracion %/% 60  # Minutos completos
  segundos <- duracion %% 60  # Segundos restantes
  if (minutos > 0) {
    sprintf("%d minutos y %.0f segundos", minutos, segundos)
  } else {
    sprintf("%.0f segundos", segundos)
  }
}

# Función para formatear la distancia en km o metros
formatear_distancia <- function(distancia) {
  if (distancia >= 1000) {
    sprintf("%.2f km", distancia / 1000)
  } else {
    sprintf("%.0f metros", distancia)
  }
}

# Crear el mapa con ajustes en unidades
leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  
  # Ruta más corta en azul con borde oscuro
  leaflet::addPolylines(data = ruta_mas_corta, 
                        color = "#000080",  # Color más oscuro para el borde
                        weight = 7,  # Peso del borde
                        opacity = 1) %>% 
  leaflet::addPolylines(data = ruta_mas_corta, 
                        color = "#0000FF",  # Color de la línea principal
                        weight = 4, 
                        opacity = 0.7, 
                        label = "Ruta más corta") %>% 
  
  # Ruta más rápida en verde con borde oscuro
  leaflet::addPolylines(data = ruta_mas_rapida, 
                        color = "#008000",  # Color más oscuro para el borde
                        weight = 7,  # Peso del borde
                        opacity = 1) %>% 
  leaflet::addPolylines(data = ruta_mas_rapida, 
                        color = "#00FF00",  # Color de la línea principal
                        weight = 4, 
                        opacity = 0.7, 
                        label = "Ruta más rápida") %>% 
  
  # Marcadores para inicio y fin
  leaflet::addMarkers(lng = coords_interseccion1[1], 
                      lat = coords_interseccion1[2], 
                      icon = icono_inicio, label = "Partida", 
                      labelOptions = labelOptions(noHide = TRUE, direction = "auto")) %>%
  
  leaflet::addMarkers(lng = coords_interseccion2[1], 
                      lat = coords_interseccion2[2], 
                      icon = icono_fin, label = "Llegada", 
                      labelOptions = labelOptions(noHide = TRUE, direction = "auto")) %>%
  
  # Control para mostrar la distancia y duración de ambas rutas con unidades adecuadas
  leaflet::addControl(html = paste("<div style='background: white; padding: 5px; border: 1px solid black; color: blue'>",
                                   "<b>Ruta más corta:</b><br>",
                                   "<div style='background: white; padding: 5px'>",
                                   "Distancia: ", formatear_distancia(ruta_mas_corta$distance), "<br>",
                                   "Duración: ", formatear_duracion(ruta_mas_corta$duration), "<br><br>",
                                   
                                   "<div style='background: white; color: green'>",
                                   "<b>Ruta más rápida:</b><br>",
                                   "<div style='background: white; padding: 5px'>",
                                   "Distancia: ", formatear_distancia(ruta_mas_rapida$distance), "<br>",
                                   "Duración: ", formatear_duracion(ruta_mas_rapida$duration),
                                   "</div>"), 
                      position = "bottomright")

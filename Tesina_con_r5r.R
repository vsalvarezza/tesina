# Instalar paquetes
#install.packages('r5r')

# Librerias
options(java.parameters = "-Xmx4G")  # Ajusta el tamaño según sea necesario
library(r5r)
library(devtools)
library(sf) # para manejar datos espaciales
library(osmdata)
library(dplyr)
library(leaflet)  # Para hacer mapas interactivos
library(osrm)

# Los daros los saque de: https://extract.bbbike.org/ que tienen solo información de Rosario

# Configurar R5R
path <- "C:\\Users\\MarceloZalva\\Documents\\tesina\\Datos_tesina\\"
r5r_core <- setup_r5(data_path = path, verbose = FALSE) # objeto que contiene el motor de R5 para calcular rutas o matrices de tiempos de viaje.

# Obtener intersecciones -----------------------------------------------------

# Descargar datos de OSM para Rosario
rosario_osm <- opq(bbox = "Rosario, Argentina") %>% # permite extraer datos de Rosario.
  add_osm_feature(key = "highway") %>% # este filtro da todas las líneas que representan calles.
  osmdata_sf() # descarga los datos en formato sf (Simple Features)

# Extraer las líneas (calles) y los nodos (intersecciones)
calles <- rosario_osm$osm_lines

# Primera interseccion
Tres_febrero <- calles %>% filter(name == "3 de Febrero")
Santiago <- calles %>% filter(name == "Santiago")

# Nodos de intersección
interseccion1 <- st_intersection(Tres_febrero, Santiago)

# Segunda interseccion
Mendoza <- calles %>% filter(name == "Mendoza")
Avellaneda <- calles %>% filter(name == "Bulevar Nicolás Avellaneda")

# Nodos de intersección
interseccion2 <- st_intersection(Mendoza, Avellaneda)

# Crear un mapa y añadir las intersecciones
leaflet() %>%
  addTiles() %>%
  addCircles(data = interseccion1, color = "red", weight = 2, radius = 5) %>%
  addCircles(data = interseccion2, color = "green", weight = 2, radius = 5)

# Obtener intersecciones -----------------------------------------------------

# Extraer las coordenadas de las intersecciones
coords_interseccion1 <- st_coordinates(interseccion1)[1, ]
coords_interseccion2 <- st_coordinates(interseccion2)[1, ]

# Calcular la ruta entre las intersecciones
ruta <- osrmRoute(src = coords_interseccion1, dst = coords_interseccion2, returnclass = "sf")

# Calcular el tiempo de viaje en minutos
tiempo_viaje_minutos <- ruta$duration / 60  # Convierte de segundos a minutos

# Calcular la distancia en kilómetros
distancia_km <- ruta$distance / 1000  # Convierte de metros a kilómetros

# Definir iconos para inicio y fin
icono_inicio <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-green.png", 
  iconWidth = 25, iconHeight = 41,
  iconAnchorX = 12, iconAnchorY = 41
)

icono_fin <- makeIcon(
  iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png", 
  iconWidth = 25, iconHeight = 41,
  iconAnchorX = 12, iconAnchorY = 41
)

# Crear un mapa y añadir las intersecciones y la ruta
leaflet() %>%
  addTiles() %>%
  addPolylines(data = ruta, color = "blue", weight = 4, opacity = 0.7) %>%  # Dibuja la ruta calculada en el mapa
  
  # Añadir marcador para el punto de Partida con icono verde
  addMarkers(lng = coords_interseccion1[1], 
             lat = coords_interseccion1[2], 
             icon = icono_inicio,  # Icono personalizado
             label = "Partida",  # Etiqueta para el punto de inicio
             labelOptions = labelOptions(noHide = TRUE, direction = "auto")) %>%  # Añade un marcador en el punto de inicio de la ruta
  
  # Añadir marcador para el punto de llegada con icono rojo
  addMarkers(lng = coords_interseccion2[1], 
             lat = coords_interseccion2[2], 
             icon = icono_fin,  # Icono personalizado
             label = "Llegada",  # Etiqueta para el punto de fin
             labelOptions = labelOptions(noHide = TRUE, direction = "auto"))  %>%  # Añade un marcador en el punto de fin de la ruta.
  
  addControl(html = paste("<div style='background: white; padding: 5px; border: 1px solid black;'>",
                          "Tiempo estimado de viaje: ", round(tiempo_viaje_minutos, 2), " minutos<br>",
                          "Distancia a recorrer: ", round(distancia_km, 3), " km</div>"), 
             position = "bottomright") 

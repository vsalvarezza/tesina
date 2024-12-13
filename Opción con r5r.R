# Librerias
options(java.parameters = "-Xmx4G")  # Ajusta el tamaño según sea necesario
library(r5r)
library(sf)
library(dplyr)
library(leaflet)

# Configurar R5R
r5r_core <- setup_r5(data_path = "Datos_tesina", verbose = FALSE)


# --------------------------- OBTENER INTERSECCIONES --------------------------- 


# Descargar datos de OSM para Rosario
rosario_osm <- osmdata::opq(bbox = "Rosario, Argentina") %>%  # permite extraer datos de Rosario.
  osmdata::add_osm_feature(key = "highway") %>%  # este filtro da todas las líneas que representan calles.
  osmdata::osmdata_sf()  # descarga los datos en formato sf (Simple Features)

# Extraer las líneas (calles) y los nodos (intersecciones)
calles <- rosario_osm$osm_lines

# Primera interseccion
Tres_febrero <- calles %>% filter(name == "3 de Febrero")
Santiago <- calles %>% filter(name == "Santiago")

# Nodos de intersección
interseccion1 <- sf::st_intersection(Tres_febrero, Santiago)

# Segunda interseccion
Mendoza <- calles %>% filter(name == "Mendoza")
Avellaneda <- calles %>% filter(name == "Bulevar Nicolás Avellaneda")

# Nodos de intersección
interseccion2 <- sf::st_intersection(Mendoza, Avellaneda)

# Extraer las coordenadas de las intersecciones
coords_interseccion1 <- sf::st_coordinates(interseccion1)[1, ]
coords_interseccion2 <- sf::st_coordinates(interseccion2)[1, ]

# Crear dataframes para origen y destino
origen <- data.frame(id = "origen", lon = coords_interseccion1[1], lat = coords_interseccion1[2])
destino <- data.frame(id = "destino", lon = coords_interseccion2[1], lat = coords_interseccion2[2])


# ----------------------------- GRÁFICAR RECORRIDO ----------------------------- 


# Calcular itinerarios detallados usando r5r
detalle_rutas <- detailed_itineraries(
  r5r_core = r5r_core,
  origins = origen,
  destinations = destino,
  mode = "CAR",  # Modo de transporte
)

# Convertir la geometría en coordenadas separadas
coords <- sf::st_coordinates(detalle_rutas$geometry)

# Crear el mapa interactivo
leaflet() %>%
  addTiles() %>%
  addPolylines(
    lng = coords[, "X"],  # Extraer la columna de longitudes
    lat = coords[, "Y"],  # Extraer la columna de latitudes
    color = "blue",
    weight = 4,
    opacity = 0.7
  ) %>%
  addMarkers(
    lng = coords_interseccion1[1],
    lat = coords_interseccion1[2],
    label = "Partida",
    labelOptions = labelOptions(noHide = TRUE, direction = "auto")
  ) %>%
  addMarkers(
    lng = coords_interseccion2[1],
    lat = coords_interseccion2[2],
    label = "Llegada",
    labelOptions = labelOptions(noHide = TRUE, direction = "auto")
  ) %>%
  addControl(
    html = paste(
      "<div style='background: white; padding: 5px; border: 1px solid black;'>",
      "Tiempo estimado de viaje: ", round(detalle_rutas$total_duration / 60, 2), " minutos<br>",
      "Distancia a recorrer: ", round(detalle_rutas$total_distance / 1000, 3), " km</div>"
    ),
    position = "bottomright"
  )


# -------------------------------- OTRA FUNCIÓN --------------------------------

mode <- c("WALK", "TRANSIT")
max_walk_time <- 30   # minutes
max_trip_duration <- 60 # minutes

ettm <- expanded_travel_time_matrix(r5r_core = r5r_core,
                                   origins = origen,
                                   destinations = destino,
                                   mode = mode,
                                   max_walk_time = max_walk_time,
                                   max_trip_duration = max_trip_duration)

leaflet() %>%
  addTiles() %>%
  addPolylines(
    lng = c(coords_interseccion1[1], coords_interseccion2[1]),
    lat = c(coords_interseccion1[2], coords_interseccion2[2]),
    color = "blue",
    weight = 2,
    opacity = 0.8
  ) %>%
  addCircleMarkers(
    lng = coords_interseccion1[1],
    lat = coords_interseccion1[2],
    color = "blue",
    label = "Origen"
  ) %>%
  addCircleMarkers(
    lng = coords_interseccion2[1],
    lat = coords_interseccion2[2],
    color = "red",
    label = "Destino"
  )

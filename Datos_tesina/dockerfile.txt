# Usar una imagen base de OSRM con R
FROM rocker/r-ver:latest  

# Instalar dependencias
RUN apt-get update && apt-get install -y osrm-backend wget 

# Instalar paquetes de R
RUN R -e "install.packages(c('devtools', 'sf', 'osmdata', 'dplyr', 'leaflet', 'osrm'), repos='https://cloud.r-project.org/')"

# Crear carpeta de trabajo
WORKDIR /app

# Descargar el mapa de Argentina (si no está en el repo)
RUN wget -O argentina-latest.osm.pbf https://download.geofabrik.de/south-america/argentina-latest.osm.pbf

# Preparar OSRM
RUN osrm-extract -p /opt/car.lua argentina-latest.osm.pbf && \
    osrm-partition argentina-latest.osrm && \
    osrm-customize argentina-latest.osrm

# Copiar el código de R
COPY script.R /app/script.R

# Iniciar OSRM y luego ejecutar el script de R
CMD osrm-routed --algorithm MLD argentina-latest.osrm & Rscript /app/script.R

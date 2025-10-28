library(shiny)
library(igraph)
library(DT)

# Crear grafo dirigido y ponderado con doble dirección en algunas aristas
vertices <- data.frame(name = c("A","B","C","D","E","F"))

aristas <- data.frame(
  from   = c("A","D","A","B","C","C","E","F","E","D","D"),
  to     = c("B","A","F","C","B","E","C","C","D","E","F"),
  weight = c(4, 1, 39, 100, 8, 8, 3, 6, 8, 12, 8)
)

grafo <- graph_from_data_frame(aristas, directed=TRUE, vertices=vertices)

# Coordenadas fijas para replicar el diseño del gráfico original
layout_fijo <- matrix(c(
  0, 0.5,    # A
  1, 2,      # B
  3, 2,      # C
  0.5, -0.5, # D
  3, -0.5,   # E
  2, 0.5     # F
), ncol=2, byrow=TRUE)

ui <- fluidPage(
  titlePanel("Dijkstra Paso a Paso"),
  sidebarLayout(
    sidebarPanel(
      selectInput("origen", "Nodo Origen:", choices = V(grafo)$name, selected = "A"),
      selectInput("destino", "Nodo Destino:", choices = V(grafo)$name, selected = "F"),
      actionButton("paso", "Siguiente Paso")
    ),
    mainPanel(
      plotOutput("grafico", height = "500px"),
      DTOutput("tabla")
    )
  )
)

server <- function(input, output, session) {
  distancias <- reactiveVal()
  predecesor <- reactiveVal()
  visitado <- reactiveVal()
  pasoActual <- reactiveVal(0)
  colores_aristas <- reactiveVal(rep("gray", ecount(grafo)))
  tabla_global <- reactiveVal(NULL)
  camino_final <- reactiveVal(NULL)
  valores_fijos <- reactiveVal(character())
  
  observeEvent(input$origen, {
    origen <- input$origen
    nodos <- V(grafo)$name
    dist_ini <- setNames(rep(Inf, length(nodos)), nodos)
    dist_ini[origen] <- 0
    distancias(dist_ini)
    predecesor(setNames(rep(NA, length(nodos)), nodos))
    visitado(setNames(rep(FALSE, length(nodos)), nodos))
    pasoActual(0)
    colores_aristas(rep("gray", ecount(grafo)))
    camino_final(NULL)
    valores_fijos(character())
    
    tabla_ini <- data.frame(
      Nodo = nodos,
      K0 = sapply(nodos, function(n) {
        if(n == origen) paste0("<span style='color:#d62728'>0; ", origen, "</span>") else "∞"
      })
    )
    tabla_global(tabla_ini)
  })
  
  observeEvent(input$paso, {
    origen <- input$origen
    destino <- input$destino
    dist <- distancias()
    pred <- predecesor()
    visited <- visitado()
    current_step <- pasoActual()
    colores <- colores_aristas()
    fijos <- valores_fijos()
    
    candidatos <- names(dist)[!visited]
    if(length(candidatos) == 0) return()
    d_vals <- dist[candidatos]
    u <- candidatos[which.min(d_vals)]
    visited[u] <- TRUE
    visitado(visited)
    
    vecinos <- aristas[aristas$from == u & !visited[aristas$to], ]
    if(nrow(vecinos) > 0) {
      for(i in 1:nrow(vecinos)) {
        v <- vecinos$to[i]
        w <- vecinos$weight[i]
        idx <- which(aristas$from == u & aristas$to == v)
        if(length(idx) > 0) colores[idx] <- "#ff7f0e"
        if(dist[u] + w < dist[v]) {
          dist[v] <- dist[u] + w
          pred[v] <- u
        }
      }
    }
    
    if(!is.na(pred[u])) {
      idx <- which(aristas$from == pred[u] & aristas$to == u)
      if(length(idx) > 0) colores[idx] <- "#d62728"
    }
    
    distancias(dist); predecesor(pred); colores_aristas(colores)
    current_step <- current_step + 1
    pasoActual(current_step)
    fijos <- unique(c(fijos, u))
    valores_fijos(fijos)
    
    col_name <- paste0("K", current_step)
    nueva_col <- sapply(names(dist), function(n) {
      if(!visited[n] && is.infinite(dist[n])) return("∞")
      if(is.na(pred[n])) {
        if(n == origen) paste0("<span style='color:#d62728'>", dist[n], "; ", n, "</span>") else "∞"
      } else {
        val <- paste0(dist[n], "; ", pred[n])
        if(n %in% fijos) paste0("<span style='color:#d62728'>", val, "</span>") else paste0("<span style='color:#ff7f0e'>", val, "</span>")
      }
    })
    
    tabla <- tabla_global()
    tabla[[col_name]] <- nueva_col
    tabla_global(tabla)
    
    output$tabla <- renderDT({
      datatable(tabla, rownames=FALSE, escape = FALSE, options=list(dom='t'))
    })
    
    if(u == destino) {
      camino <- c()
      actual <- destino
      while (!is.na(actual)) {
        camino <- c(actual, camino)
        actual <- pred[actual]
      }
      camino_final(camino)
      for(i in seq_len(length(camino) - 1)) {
        from <- camino[i]
        to <- camino[i + 1]
        idx <- which(aristas$from == from & aristas$to == to)
        if(length(idx) > 0) colores[idx] <- "#d62728"
      }
      colores_aristas(colores)
    }
    
    output$grafico <- renderPlot({
      plot(grafo, layout = layout_fijo, asp=1,
           vertex.size=40,
           vertex.color=ifelse(visited, "lightgreen", "lightblue"),
           vertex.frame.color="black",
           vertex.label.color="black",
           vertex.label.font=2,
           edge.arrow.size=0.6,
           edge.curved=0.2,
           edge.label=E(grafo)$weight,
           edge.label.cex=0.9,
           edge.color=colores_aristas())
    })
    
    if(nrow(vecinos) > 0) {
      for(i in 1:nrow(vecinos)) {
        v <- vecinos$to[i]
        idx <- which(aristas$from == u & aristas$to == v)
        if(length(idx) > 0 && colores[idx] == "#ff7f0e") {
          colores[idx] <- "gray"
        }
      }
    }
    colores_aristas(colores)
  })
}

shinyApp(ui, server)

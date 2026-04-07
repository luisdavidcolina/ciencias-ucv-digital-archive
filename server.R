library(shiny)

# --- FASE DE PRUEBA: Carga desde archivo local enriquecido ---
datos_crudos <- read.csv("datos_test.csv", stringsAsFactors = FALSE)

server <- function(input, output, session) {
  
  # Reactive dataset para alimentar la vista
  datos_reactivos <- reactive({
    datos <- datos_crudos
    
    # Texto Search
    if (!is.null(input$search_text) && input$search_text != "") {
      term <- tolower(input$search_text)
      datos <- datos[grepl(term, tolower(datos$titulo)) | 
                     grepl(term, tolower(datos$autor)) | 
                     grepl(term, tolower(datos$resumen)), ]
    }
    
    # Colección Facet
    if (input$modulo_filter != "Todos") {
      datos <- datos[datos$modulo == input$modulo_filter, ]
    }
    
    # Tipo Doc Facet
    if (length(input$doc_type_filter) > 0) {
      datos <- datos[datos$tipo_documento %in% input$doc_type_filter, ]
    }
    
    return(datos)
  })
  
  output$dspace_item_list <- renderUI({
    datos <- datos_reactivos()
    
    if (nrow(datos) == 0) {
      return(tags$div(class = "alert alert-secondary", "No results were found that meet your search criteria."))
    }
    
    # Construcción de la lista de tarjetas HTML como DSpace
    tarjetas <- lapply(1:nrow(datos), function(i) {
      fila <- datos[i, ]
      
      # `<ds-item-list-element>` mimetismo
      tags$div(class = "ds-item-card",
               
        # Miniatura Izquierda Genérica
        tags$div(class = "ds-item-thumbnail",
           tags$i(class = "fas fa-file-pdf")
        ),
        
        # Bloque de Metadatos Derecho
        tags$div(class = "ds-item-metadata",
           tags$a(class = "ds-item-title", href="#", fila$titulo),
           tags$div(class = "ds-item-authors", fila$autor),
           tags$div(class = "ds-item-publisher", paste("Publisher:", fila$editor)),
           tags$div(class = "ds-item-date", paste("Date Issued:", fila$fecha)),
           tags$div(class = "ds-item-abstract", fila$resumen),
           tags$span(class = "ds-badge", fila$tipo_documento)
        )
      )
    })
    
    # Retornamos el empaquetado de resultados total
    tagList(
       tags$div(class="ds-results-header",
          tags$h2(class="ds-results-title", "Search Results"),
          tags$span(class="ds-pagination-info", paste("1-", nrow(datos), " of ", nrow(datos), sep=""))
       ),
       tarjetas
    )
  })
}

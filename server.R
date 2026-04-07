library(shiny)

# --- FASE DE PRUEBA: Carga desde archivo local enriquecido ---
datos_crudos <- read.csv("datos_test.csv", stringsAsFactors = FALSE)

server <- function(input, output, session) {
  
  # Reactive dataset para alimentar la vista
  datos_reactivos <- reactive({
    # Obligamos a que actialice el filtro con los inputs (en un flujo real seria un button-trigger, pero reaccionara en vivo para mejor UX)
    datos <- datos_crudos
    
    # Texto
    if (!is.null(input$search_text) && input$search_text != "") {
      term <- tolower(input$search_text)
      datos <- datos[grepl(term, tolower(datos$titulo)) | 
                     grepl(term, tolower(datos$autor)) | 
                     grepl(term, tolower(datos$resumen)), ]
    }
    
    # Colección
    if (input$modulo_filter != "Todos") {
      datos <- datos[datos$modulo == input$modulo_filter, ]
    }
    
    # Tipo Doc
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
      
      tags$div(class = "ds-item-card",
               
        # Miniatura Izquierda
        tags$div(class = "ds-item-thumbnail",
           tags$i(class = "fas fa-file-pdf")
        ),
        
        # Bloque de Metadatos Derecho
        tags$div(class = "ds-item-metadata",
           tags$div(class = "ds-item-title", fila$titulo),
           tags$div(class = "ds-item-authors", fila$autor),
           tags$div(class = "ds-item-publisher", paste("Publisher:", fila$editor)),
           tags$div(class = "ds-item-date", paste("Date Issued:", fila$fecha)),
           tags$div(class = "ds-item-abstract", fila$resumen),
           tags$div(class = "ds-badge", fila$tipo_documento)
        )
      )
    })
    
    # Retornar todo el bloque al frontend
    tagList(
       tags$div(style="border-bottom: 2px solid #0056b3; padding-bottom: 5px; margin-bottom: 15px;",
          tags$span(style="font-size: 18px; font-weight: 500; color: #495057;", "Results "),
          tags$span(style="font-size: 14px; color: #6c757d;", paste("1-", nrow(datos), " of ", nrow(datos), sep=""))
       ),
       tarjetas
    )
  })
}

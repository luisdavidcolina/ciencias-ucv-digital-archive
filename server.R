library(shiny)

datos_crudos <- read.csv("datos_test.csv", stringsAsFactors = FALSE)

server <- function(input, output, session) {
  
  datos_reactivos <- reactive({
    datos <- datos_crudos
    if (!is.null(input$search_text) && input$search_text != "") {
      term <- tolower(input$search_text)
      datos <- datos[grepl(term, tolower(datos$titulo)) | 
                     grepl(term, tolower(datos$autor)) | 
                     grepl(term, tolower(datos$resumen)), ]
    }
    if (input$modulo_filter != "Todos") {
      datos <- datos[datos$modulo == input$modulo_filter, ]
    }
    if (length(input$doc_type_filter) > 0) {
      datos <- datos[datos$tipo_documento %in% input$doc_type_filter, ]
    }
    return(datos)
  })
  
  # ----------------------------------------------------
  # MOTOR RENDER PÚBLICO
  # ----------------------------------------------------
  output$dspace_item_list <- renderUI({
    datos <- datos_reactivos()
    if (nrow(datos) == 0) { return(tags$div(class = "alert alert-secondary", "No results were found.")) }
    
    tarjetas <- lapply(1:nrow(datos), function(i) {
      fila <- datos[i, ]
      tags$div(class = "ds-item-card",
        tags$div(class = "ds-item-thumbnail", tags$i(class = "fas fa-file-pdf")),
        tags$div(class = "ds-item-metadata",
           tags$a(class = "ds-item-title", href="#", fila$titulo),
           tags$div(class = "ds-item-authors", fila$autor),
           tags$div(class = "ds-item-publisher", paste("Publisher:", fila$editor)),
           tags$div(class = "ds-item-date", paste("Date:", fila$fecha)),
           tags$div(class = "ds-item-abstract", fila$resumen),
           tags$span(class = "ds-badge", fila$tipo_documento)
        )
      )
    })
    
    tagList(
       tags$div(class="ds-results-header",
          tags$h2(class="ds-results-title", "Resultados"),
          tags$span(class="ds-pagination-info", paste("1-", nrow(datos), " of ", nrow(datos), sep=""))
       ),
       tarjetas
    )
  })
  
  # ----------------------------------------------------
  # MOTOR RENDER ADMINISTRATIVO (Bandeja MyDSpace)
  # ----------------------------------------------------
  output$admin_workflow_list <- renderUI({
    # Simularemos tomar solo los 3 primeros documentos estaticos como "Pendientes"
    datos_cola <- head(datos_crudos, 3)
    
    # Construimos iteración de lista de flujo
    tareas <- lapply(1:nrow(datos_cola), function(i) {
      fila <- datos_cola[i, ]
      
      tags$div(style = "padding: 15px; border-bottom: 1px solid #dee2e6; display: flex; justify-content: space-between; align-items: center;",
        # Info Documento
        tags$div(
          tags$strong(fila$titulo, style="font-size: 1.1rem; color: #2b4e72;"), tags$br(),
          tags$span(style="color:#6c757d; font-size: 0.9rem;", paste("Subido por:", fila$autor,"| Extensión Módulo:", fila$modulo))
        ),
        # Controles
        tags$div(
          actionButton(paste0("btn_approve_", i), "Aprobar", icon=icon("check"), class="btn btn-success btn-sm"),
          actionButton(paste0("btn_reject_", i), "Rechazar", icon=icon("times"), class="btn btn-danger btn-sm", style="margin-left: 5px;")
        )
      )
    })
    
    tagList(tareas)
  })
}

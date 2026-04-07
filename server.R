library(shiny)

# Carga Condicional Separada
db_ext <- read.csv("datos_extension.csv", stringsAsFactors = FALSE)
db_rrhh <- read.csv("datos_rrhh.csv", stringsAsFactors = FALSE)

server <- function(input, output, session) {
  
  # --- LOGICA EXTENSION ---
  dat_ext_react <- reactive({
    datos <- db_ext
    if (!is.null(input$search_ext) && input$search_ext != "") {
      term <- tolower(input$search_ext)
      datos <- datos[grepl(term, tolower(datos$titulo)) | grepl(term, tolower(datos$autor)), ]
    }
    if (length(input$ext_doc_type) > 0) {
      datos <- datos[datos$doc_type %in% input$ext_doc_type, ]
    }
    return(datos)
  })
  
  output$list_extension <- renderUI({
    datos <- dat_ext_react()
    if (nrow(datos) == 0) return(tags$div(class = "alert alert-secondary", "No se encontraron proyectos académicos."))
    
    tarjetas <- lapply(1:nrow(datos), function(i) {
      fila <- datos[i, ]
      tags$div(class = "ds-item-card",
        tags$div(class = "ds-item-thumbnail", tags$i(class = "fas fa-file-alt")),
        tags$div(class = "ds-item-metadata",
           tags$a(class = "ds-item-title", href="#", fila$titulo),
           tags$div(class = "ds-item-authors", paste("Responsable:", fila$autor)),
           tags$div(class = "ds-item-date", paste("Fecha Emisión:", fila$fecha)),
           tags$div(class = "ds-item-abstract", fila$abstract),
           tags$span(class = "ds-badge", fila$doc_type)
        )
      )
    })
    tagList(tags$div(class="ds-results-header", tags$h2(class="ds-results-title", "Archivo Extensión"), tags$span(class="ds-pagination-info", paste(nrow(datos), "Resultados"))), tarjetas)
  })
  
  # --- LOGICA RRHH ---
  dat_rrhh_react <- reactive({
    datos <- db_rrhh
    if (!is.null(input$search_rrhh) && input$search_rrhh != "") {
      term <- tolower(input$search_rrhh)
      datos <- datos[grepl(term, tolower(datos$empleado)) | grepl(term, tolower(datos$cedula)), ]
    }
    if (input$rrhh_estatus != "Todos") {
      datos <- datos[datos$estatus == input$rrhh_estatus, ]
    }
    if (length(input$rrhh_doc_type) > 0) {
      datos <- datos[datos$doc_type %in% input$rrhh_doc_type, ]
    }
    return(datos)
  })
  
  output$list_rrhh <- renderUI({
    datos <- dat_rrhh_react()
    if (nrow(datos) == 0) return(tags$div(class = "alert alert-secondary", "No se encontraron expedientes laborales."))
    
    tarjetas <- lapply(1:nrow(datos), function(i) {
      fila <- datos[i, ]
      
      # Lógica de Color para Estatus
      color_status <- "green"
      if(fila$estatus == "Jubilado") color_status <- "purple"
      if(fila$estatus == "Inactivo") color_status <- "red"
      
      tags$div(class = "ds-item-card",
        tags$div(class = "ds-item-thumbnail", tags$i(class = "fas fa-user-lock", style="color:#dc3545;")), # Candado rojo por privacidad
        tags$div(class = "ds-item-metadata",
           tags$a(class = "ds-item-title", href="#", paste("Expediente:", fila$empleado)),
           tags$div(class = "ds-item-authors", tags$strong(paste("C.I.:", fila$cedula))),
           tags$div(class = "ds-item-publisher", paste("Adscripción:", fila$departamento)),
           tags$div(class = "ds-item-date", paste("Ingreso:", fila$fecha_ingreso)),
           tags$span(style=sprintf("color: white; background-color: %s; padding: 2px 6px; border-radius: 4px; font-size: 12px; margin-right: 5px;", color_status), fila$estatus),
           tags$span(class = "ds-badge", style="background-color: #6c757d;", fila$doc_type)
        )
      )
    })
    tagList(tags$div(class="ds-results-header", tags$h2(class="ds-results-title", "Expedientes Privados (RRHH)"), tags$span(class="ds-pagination-info", paste(nrow(datos), "Resultados"))), tarjetas)
  })
  
  # --- LOGICA MY DSPACE ---
  output$admin_workflow_list <- renderUI({
    
    # Fake Workflow: 1 from Extensión, 1 from RRHH
    pendientes <- list(
      list(titulo = db_ext$titulo[2], source = "Extensión"),
      list(titulo = db_rrhh$doc_type[1], source = "RRHH")
    )
    
    tareas <- lapply(1:length(pendientes), function(i) {
      item <- pendientes[[i]]
      tags$div(style = "padding: 15px; border-bottom: 1px solid #dee2e6; display: flex; justify-content: space-between; align-items: center;",
        tags$div(
          tags$strong(item$titulo, style="font-size: 1.1rem; color: #2b4e72;"), tags$br(),
          tags$span(style="color:#6c757d; font-size: 0.9rem;", paste("Comunidad origen:", item$source))
        ),
        tags$div(
          actionButton(paste0("btn_approve_", i), "Aprobar", icon=icon("check"), class="btn btn-success btn-sm"),
          actionButton(paste0("btn_reject_", i), "Rechazar", icon=icon("times"), class="btn btn-danger btn-sm", style="margin-left: 5px;")
        )
      )
    })
    tagList(tareas)
  })
}

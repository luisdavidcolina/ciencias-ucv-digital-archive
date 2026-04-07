library(shiny)

db_ext <- read.csv("datos_extension.csv", stringsAsFactors = FALSE)
db_rrhh <- read.csv("datos_rrhh.csv", stringsAsFactors = FALSE)

ITEMS_PER_PAGE <- 5 # Cota de paginación para demo

server <- function(input, output, session) {
  
  # ==========================================
  # ESTADOS REACTIVOS GLOBALES DE PAGINACIÓN
  # ==========================================
  p_ext <- reactiveVal(1)
  p_rrhh <- reactiveVal(1)
  
  # ==========================================
  # LÓGICA EXTENSIÓN
  # ==========================================
  # Reseteos: Si el usuario busca o filtra, volvemos a la Pág 1
  observeEvent(c(input$btn_s_ext, input$btn_update_ext), { p_ext(1) }, ignoreInit = TRUE)
  
  dat_ext_react <- reactive({
    datos <- db_ext
    # Trigger explicito de dependencias (para reset)
    input$btn_s_ext
    input$btn_update_ext
    
    if (!is.null(input$search_ext) && input$search_ext != "") {
      term <- tolower(input$search_ext)
      datos <- datos[grepl(term, tolower(datos$titulo)) | grepl(term, tolower(datos$autor)), ]
    }
    if (length(input$ext_doc_type) > 0) {
      datos <- datos[datos$doc_type %in% input$ext_doc_type, ]
    }
    
    # Aplicar ordenamiento
    if (!is.null(input$sort_ext)) {
      if (input$sort_ext == "Título A-Z") {
        datos <- datos[order(datos$titulo), ]
      } else if (input$sort_ext == "Fecha de Emisión (Asc)") {
        datos <- datos[order(datos$fecha), ]
      } else if (input$sort_ext == "Fecha de Emisión (Desc)") {
        datos <- datos[order(datos$fecha, decreasing = TRUE), ]
      }
    }
    
    return(datos)
  })
  
  # Escuchas de los botones Siguiente/Anterior
  observeEvent(input$ext_prev, { p_ext(max(1, p_ext() - 1)) })
  observeEvent(input$ext_next, { 
    rpp <- as.numeric(input$rpp_ext)
    if (is.na(rpp)) rpp <- 5
    tot_pags <- ceiling(nrow(dat_ext_react()) / rpp)
    p_ext(min(tot_pags, p_ext() + 1)) 
  })
  
  output$list_extension <- renderUI({
    datos <- dat_ext_react()
    tot_items <- nrow(datos)
    if (tot_items == 0) return(tags$div(class = "alert alert-secondary", "No se encontraron proyectos académicos."))
    
    # Cálculos Paginación Dinámica
    rpp <- as.numeric(input$rpp_ext)
    if (is.na(rpp)) rpp <- 5
    tot_pags <- ceiling(tot_items / rpp)
    
    # Ajuste de seguridad si se achicó rpp
    if (p_ext() > tot_pags) p_ext(max(1, tot_pags))
    
    pag_actual <- p_ext()
    idx_inicio <- (pag_actual - 1) * rpp + 1
    idx_fin <- min(pag_actual * rpp, tot_items)
    
    datos_view <- datos[idx_inicio:idx_fin, ]
    
    tarjetas <- lapply(1:nrow(datos_view), function(i) {
      fila <- datos_view[i, ]
      tags$div(class = "ds-item-card",
        tags$div(class = "ds-item-thumbnail", tags$i(class = "fas fa-file-alt")),
        tags$div(class = "ds-item-metadata",
           tags$a(class = "ds-item-title", href="#", fila$titulo),
           tags$div(class = "ds-item-authors", paste("Responsable:", fila$autor)),
           tags$div(class = "ds-item-date", paste("Fecha Emisión:", fila$fecha)),
           tags$div(class = "ds-item-publisher", tags$i(class="fas fa-archive"), paste(" Ubicación:", fila$ubicacion)),
           tags$div(class = "ds-item-abstract", fila$abstract),
           tags$span(class = "ds-badge", fila$doc_type)
        )
      )
    })
    
    paginador <- tags$div(class = "d-flex justify-content-center mt-4",
       tags$ul(class = "pagination",
         tags$li(class = ifelse(pag_actual == 1, "page-item disabled", "page-item"), 
                 actionLink("ext_prev", " Anterior", icon = icon("angle-double-left"), class="page-link ds-page-link")),
         tags$li(class = "page-item active", tags$span(class="page-link ds-page-active", paste("Pág", pag_actual, "de", tot_pags))),
         tags$li(class = ifelse(pag_actual >= tot_pags, "page-item disabled", "page-item"), 
                 actionLink("ext_next", "Siguiente ", icon = icon("angle-double-right"), class="page-link ds-page-link"))
       )
    )
    
    tagList(
      tags$div(class="ds-results-header", 
               tags$h2(class="ds-results-title", "Archivo Extensión"), 
               tags$span(class="ds-pagination-info", paste("Mostrando", idx_inicio, "-", idx_fin, "de", tot_items, "Resultados"))
      ), 
      tarjetas,
      paginador
    )
  })
  
  # ==========================================
  # LÓGICA RRHH
  # ==========================================
  observeEvent(c(input$btn_s_rrhh, input$btn_update_rrhh), { p_rrhh(1) }, ignoreInit = TRUE)
  
  dat_rrhh_react <- reactive({
    datos <- db_rrhh
    input$btn_s_rrhh
    input$btn_update_rrhh
    
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
    
    # Aplicar ordenamiento
    if (!is.null(input$sort_rrhh)) {
      if (input$sort_rrhh == "Empleado A-Z") {
        datos <- datos[order(datos$empleado), ]
      } else if (input$sort_rrhh == "Fecha Ingreso (Asc)") {
        datos <- datos[order(datos$fecha_ingreso), ]
      } else if (input$sort_rrhh == "Fecha Ingreso (Desc)") {
        datos <- datos[order(datos$fecha_ingreso, decreasing = TRUE), ]
      }
    }
    
    return(datos)
  })
  
  observeEvent(input$rrhh_prev, { p_rrhh(max(1, p_rrhh() - 1)) })
  observeEvent(input$rrhh_next, { 
    rpp <- as.numeric(input$rpp_rrhh)
    if (is.na(rpp)) rpp <- 5
    tot_pags <- ceiling(nrow(dat_rrhh_react()) / rpp)
    p_rrhh(min(tot_pags, p_rrhh() + 1)) 
  })
  
  output$list_rrhh <- renderUI({
    datos <- dat_rrhh_react()
    tot_items <- nrow(datos)
    if (tot_items == 0) return(tags$div(class = "alert alert-secondary", "No se encontraron expedientes laborales."))
    
    rpp <- as.numeric(input$rpp_rrhh)
    if (is.na(rpp)) rpp <- 5
    tot_pags <- ceiling(tot_items / rpp)
    
    if (p_rrhh() > tot_pags) p_rrhh(max(1, tot_pags))
    
    pag_actual <- p_rrhh()
    idx_inicio <- (pag_actual - 1) * rpp + 1
    idx_fin <- min(pag_actual * rpp, tot_items)
    datos_view <- datos[idx_inicio:idx_fin, ]
    
    tarjetas <- lapply(1:nrow(datos_view), function(i) {
      fila <- datos_view[i, ]
      color_status <- ifelse(fila$estatus == "Jubilado", "purple", ifelse(fila$estatus == "Inactivo", "red", "green"))
      
      tags$div(class = "ds-item-card",
        tags$div(class = "ds-item-thumbnail", tags$i(class = "fas fa-user-lock", style="color:#dc3545;")),
        tags$div(class = "ds-item-metadata",
           tags$a(class = "ds-item-title", href="#", paste("Expediente:", fila$empleado)),
           tags$div(class = "ds-item-authors", tags$strong(paste("C.I.:", fila$cedula))),
           tags$div(class = "ds-item-publisher", paste("Adscripción:", fila$departamento)),
           tags$div(class = "ds-item-date", paste("Ingreso:", fila$fecha_ingreso)),
           tags$div(class = "ds-item-publisher", tags$i(class="fas fa-box"), paste(" Retención Física:", fila$ubicacion)),
           tags$span(style=sprintf("color: white; background-color: %s; padding: 2px 6px; border-radius: 4px; font-size: 12px; margin-right: 5px;", color_status), fila$estatus),
           tags$span(class = "ds-badge", style="background-color: #6c757d;", fila$doc_type)
        )
      )
    })
    
    paginador <- tags$div(class = "d-flex justify-content-center mt-4",
       tags$ul(class = "pagination",
         tags$li(class = ifelse(pag_actual == 1, "page-item disabled", "page-item"), 
                 actionLink("rrhh_prev", " Anterior", icon = icon("angle-double-left"), class="page-link ds-page-link")),
         tags$li(class = "page-item active", tags$span(class="page-link ds-page-active", paste("Pág", pag_actual, "de", tot_pags))),
         tags$li(class = ifelse(pag_actual >= tot_pags, "page-item disabled", "page-item"), 
                 actionLink("rrhh_next", "Siguiente ", icon = icon("angle-double-right"), class="page-link ds-page-link"))
       )
    )
    
    tagList(
      tags$div(class="ds-results-header", 
               tags$h2(class="ds-results-title", "Expedientes Privados (RRHH)"), 
               tags$span(class="ds-pagination-info", paste("Mostrando", idx_inicio, "-", idx_fin, "de", tot_items, "Resultados"))
      ), 
      tarjetas,
      paginador
    )
  })
  
  # ==========================================
  # LÓGICA MY DSPACE (Admin Flow)
  # ==========================================
  output$admin_workflow_list <- renderUI({
    pendientes <- list(
      list(titulo = db_ext$titulo[2], source = "Extensión"),
      list(titulo = db_rrhh$doc_type[1], source = "RRHH")
    )
    tareas <- lapply(1:length(pendientes), function(i) {
      item <- pendientes[[i]]
      tags$div(style = "padding: 15px; border-bottom: 1px solid #dee2e6; display: flex; justify-content: space-between; align-items: center;",
        tags$div(tags$strong(item$titulo, style="font-size: 1.1rem; color: #2b4e72;"), tags$br(), tags$span(style="color:#6c757d; font-size: 0.9rem;", paste("Comunidad origen:", item$source))),
        tags$div(actionButton(paste0("btn_approve_", i), "Aprobar", icon=icon("check"), class="btn btn-success btn-sm"), actionButton(paste0("btn_reject_", i), "Rechazar", icon=icon("times"), class="btn btn-danger btn-sm", style="margin-left: 5px;"))
      )
    })
    tagList(tareas)
  })
}

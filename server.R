library(shiny)
library(bs4Dash)

db_ext <- read.csv("datos_extension.csv", stringsAsFactors = FALSE)
db_rrhh <- read.csv("datos_rrhh.csv", stringsAsFactors = FALSE)
db_users <- read.csv("usuarios.csv", stringsAsFactors = FALSE)

server <- function(input, output, session) {
  
  # ==========================================
  # ESTADO DE NÚCLEO (ZERO TRUST)
  # ==========================================
  session_state <- reactiveValues(
    logged = FALSE,
    username = NULL,
    modulo = NULL,
    rol = NULL
  )
  
  p_ext <- reactiveVal(1)
  p_rrhh <- reactiveVal(1)

  # EXPORTAR ESTADO PARA CONDITIONALPANEL
  output$is_logged <- reactive({ isTRUE(session_state$logged) })
  outputOptions(output, "is_logged", suspendWhenHidden = FALSE)
  
  output$nav_username <- renderText({
      req(session_state$logged)
      paste("ID:", session_state$username)
  })
  
  # ==========================================
  # GESTIÓN DE ACCESO (LOGIN)
  # ==========================================
  observeEvent(input$login_btn, {
    req(input$login_user, input$login_pass)
    match <- db_users[db_users$usuario == input$login_user & db_users$password == input$login_pass, ]
    
    if (nrow(match) == 1) {
      session_state$logged <- TRUE
      session_state$username <- match$usuario[1]
      session_state$modulo <- match$modulo[1]
      session_state$rol <- match$rol[1]
    } else {
      showNotification("Credenciales inválidas. Acceso denegado.", type = "error")
    }
  })
  
  observeEvent(input$logout_btn, {
    session_state$logged <- FALSE
    session_state$modulo <- NULL
    session_state$rol <- NULL
  })
  
  # ==========================================
  # RENDERIZADO DINÁMICO DE SIDEBAR
  # ==========================================
  output$sidebar_items <- renderUI({
    req(session_state$logged)
    lista_menu <- list(bs4SidebarHeader("Comunidades"))
    
    if (session_state$modulo == "Extensión") {
      lista_menu[[length(lista_menu) + 1]] <- bs4SidebarMenuItem("Archivo Extensión", tabName = "tab_extension", icon = icon("university"))
    }
    if (session_state$modulo == "RRHH") {
      lista_menu[[length(lista_menu) + 1]] <- bs4SidebarMenuItem("Expedientes RRHH", tabName = "tab_rrhh", icon = icon("lock"))
    }
    if (session_state$rol == "Admin") {
      lista_menu[[length(lista_menu) + 1]] <- bs4SidebarHeader("Funciones Base")
      lista_menu[[length(lista_menu) + 1]] <- bs4SidebarMenuItem("Panel de Control", tabName = "mydspace_tab", icon = icon("rocket"), badgeLabel = "!", badgeColor = "success")
    }
    
    do.call(tagList, lista_menu)
  })

  # ==========================================
  # LÓGICA DE DATOS: EXTENSIÓN & RRHH (Restaurada)
  # ==========================================
  observeEvent(c(input$btn_s_ext, input$btn_update_ext), { p_ext(1) }, ignoreInit = TRUE)
  dat_ext_react <- reactive({
    datos <- db_ext
    input$btn_s_ext
    input$btn_update_ext
    if (!is.null(input$search_ext) && input$search_ext != "") {
      term <- tolower(input$search_ext)
      datos <- datos[grepl(term, tolower(datos$titulo)) | grepl(term, tolower(datos$autor)), ]
    }
    if (length(input$ext_doc_type) > 0) { datos <- datos[datos$doc_type %in% input$ext_doc_type, ] }
    if (!is.null(input$sort_ext)) {
      if (input$sort_ext == "Título A-Z") { datos <- datos[order(datos$titulo), ] } 
      else if (input$sort_ext == "Fecha de Emisión (Asc)") { datos <- datos[order(datos$fecha), ] } 
      else if (input$sort_ext == "Fecha de Emisión (Desc)") { datos <- datos[order(datos$fecha, decreasing = TRUE), ] }
    }
    return(datos)
  })
  
  observeEvent(input$ext_prev, { p_ext(max(1, p_ext() - 1)) })
  observeEvent(input$ext_next, { 
    rpp <- as.numeric(input$rpp_ext); tot_pags <- ceiling(nrow(dat_ext_react()) / rpp)
    p_ext(min(tot_pags, p_ext() + 1)) 
  })
  
  output$list_extension <- renderUI({
    datos <- dat_ext_react(); if (nrow(datos) == 0) return(div(class = "alert alert-secondary", "No se encontraron proyectos."))
    rpp <- as.numeric(input$rpp_ext); tot_pags <- ceiling(nrow(datos) / rpp)
    pag_actual <- p_ext(); idx_inicio <- (pag_actual - 1) * rpp + 1; idx_fin <- min(pag_actual * rpp, nrow(datos))
    datos_view <- datos[idx_inicio:idx_fin, ]
    tarjetas <- lapply(1:nrow(datos_view), function(i) {
      fila <- datos_view[i, ]
      btn_desc <- if (session_state$rol == "Admin") tags$button(class="btn btn-sm btn-outline-primary mt-2", tags$i(class="fas fa-download"), " Extraer Acta") else NULL
      div(class = "ds-item-card", div(class = "ds-item-thumbnail", tags$i(class = "fas fa-file-alt")),
        div(class = "ds-item-metadata", tags$a(class = "ds-item-title", href="#", fila$titulo),
           div(class = "ds-item-authors", paste("Responsable:", fila$autor)),
           div(class = "ds-item-date", paste("Fecha Emisión:", fila$fecha)),
           div(class = "ds-item-abstract", fila$abstract),
           tags$span(class = "ds-badge", fila$doc_type), br(), btn_desc))
    })
    tagList(tarjetas)
  })

  observeEvent(c(input$btn_s_rrhh, input$btn_update_rrhh), { p_rrhh(1) }, ignoreInit = TRUE)
  dat_rrhh_react <- reactive({
    datos <- db_rrhh
    if (!is.null(input$search_rrhh) && input$search_rrhh != "") {
      term <- tolower(input$search_rrhh)
      datos <- datos[grepl(term, tolower(datos$empleado)) | grepl(term, tolower(datos$cedula)), ]
    }
    return(datos)
  })
  
  output$list_rrhh <- renderUI({
    datos <- dat_rrhh_react(); if (nrow(datos) == 0) return(div(class = "alert alert-secondary", "No hay expedientes."))
    tarjetas <- lapply(1:nrow(datos), function(i) {
      fila <- datos[i, ]
      div(class = "ds-item-card", div(class = "ds-item-thumbnail", tags$i(class = "fas fa-user-lock", style="color:#dc3545;")),
        div(class = "ds-item-metadata", tags$a(class = "ds-item-title", href="#", paste("Expediente:", fila$empleado)),
           div(class = "ds-item-authors", tags$strong(paste("C.I.:", fila$cedula))),
           div(class = "ds-item-publisher", paste("Adscripción:", fila$departamento)),
           tags$span(class = "ds-badge", style="background-color: #6c757d;", fila$doc_type)))
    })
    tagList(tarjetas)
  })

  # ==========================================
  # CUERPO PRINCIPAL DEL DASHBOARD
  # ==========================================
  output$main_body <- renderUI({
    req(session_state$logged)
    lista_tabs <- list()
    
    if (session_state$modulo == "Extensión") {
      lista_tabs[[length(lista_tabs) + 1]] <- bs4TabItem(tabName = "tab_extension",
         fluidRow(column(12, div(class = "ds-breadcrumb-wrapper", div(class = "ds-breadcrumb", "Comunidades / Extensión / Búsqueda")))),
         fluidRow(
           column(width=3,
             bs4Card(title="Filtros Académicos", status="secondary", width=12, checkboxGroupInput("ext_doc_type", "Tipología:", choices = c("Proyecto de Investigación", "Plano Arquitectónico", "Acta", "Convenio")), actionButton("btn_update_ext", "Aplicar", class="btn ds-btn-primary w-100 mt-2")),
             bs4Card(title="Vista", status="secondary", width=12, class="mt-3", selectInput("sort_ext", "Recientes", choices=c("Lo más relevante", "Título A-Z")), selectInput("rpp_ext", "Pág:", choices=c("5", "10"), selected="5"))
           ),
           column(width=9,
             div(class="ds-search-bar", div(class="input-group", tags$input(id="search_ext", type="text", class="form-control ds-search-input", placeholder="Buscar..."), div(class="input-group-append", actionButton("btn_s_ext", label=NULL, icon=icon("search"), class="btn ds-btn-primary")))),
             uiOutput("list_extension")
           )
         )
      )
    }
    
    if (session_state$modulo == "RRHH") {
      lista_tabs[[length(lista_tabs) + 1]] <- bs4TabItem(tabName = "tab_rrhh",
         fluidRow(column(12, div(class = "ds-breadcrumb-wrapper", div(class = "ds-breadcrumb", "Comunidades / RRHH / Privado")))),
         fluidRow(
           column(width=3, bs4Card(title="Filtros", status="secondary", width=12, radioButtons("rrhh_estatus", "Condición:", choices = c("Todos", "Activo", "Jubilado")))),
           column(width=9, uiOutput("list_rrhh"))
         )
      )
    }
    
    if (session_state$rol == "Admin") {
      lista_tabs[[length(lista_tabs) + 1]] <- bs4TabItem(tabName = "mydspace_tab",
         tags$h3(style="font-family: 'Nunito', sans-serif; margin-bottom:20px;", "Panel de Control Administrativo"),
         tabsetPanel(id = "admin_workspace_tabs", type = "pills",
            tabPanel("Directorio Local", icon = icon("list-ul"), tags$br(), bs4Card(title="Base de Datos Activa", status="info", width=12, collapsible=F, uiOutput("admin_control_table"))),
            tabPanel("Configuración", icon = icon("cog"), tags$br(), "Opciones de sistema avanzadas.")
         )
      )
    }
    
    do.call(bs4TabItems, lista_tabs)
  })

  # Admin table helper
  output$admin_control_table <- renderUI({
      df <- if (session_state$modulo == "Extensión") db_ext else db_rrhh
      filas <- lapply(1:min(5, nrow(df)), function(i) {
          f <- df[i, ]
          id <- if(session_state$modulo=="Extensión") f$titulo else f$empleado
          tags$tr(tags$td(id), tags$td(f$doc_type), tags$td(actionButton(paste0("e_",i), NULL, icon=icon("edit"), class="btn btn-sm btn-outline-info")))
      })
      tags$table(class="table table-hover bg-white", tags$thead(tags$tr(tags$th("Item"), tags$th("Tipo"), tags$th("Acción"))), tags$tbody(filas))
  })
}

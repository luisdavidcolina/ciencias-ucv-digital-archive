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
  # LÓGICA DE DATOS: EXTENSIÓN
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
    if (length(input$ext_doc_type) > 0) {
      datos <- datos[datos$doc_type %in% input$ext_doc_type, ]
    }
    if (!is.null(input$sort_ext)) {
      if (input$sort_ext == "Título A-Z") { datos <- datos[order(datos$titulo), ] } 
      else if (input$sort_ext == "Fecha de Emisión (Asc)") { datos <- datos[order(datos$fecha), ] } 
      else if (input$sort_ext == "Fecha de Emisión (Desc)") { datos <- datos[order(datos$fecha, decreasing = TRUE), ] }
    }
    return(datos)
  })
  
  observeEvent(input$ext_prev, { p_ext(max(1, p_ext() - 1)) })
  observeEvent(input$ext_next, { 
    rpp <- as.numeric(input$rpp_ext)
    if (is.na(rpp)) rpp <- 5
    tot_pags <- ceiling(nrow(dat_ext_react()) / rpp)
    p_ext(min(tot_pags, p_ext() + 1)) 
  })
  
  output$list_extension <- renderUI({
    datos <- dat_ext_react()
    if (nrow(datos) == 0) return(tags$div(class = "alert alert-secondary", "No se encontraron proyectos académicos."))
    
    rpp <- as.numeric(input$rpp_ext)
    if (is.na(rpp)) rpp <- 5
    tot_pags <- ceiling(nrow(datos) / rpp)
    if (p_ext() > tot_pags) p_ext(max(1, tot_pags))
    pag_actual <- p_ext()
    idx_inicio <- (pag_actual - 1) * rpp + 1
    idx_fin <- min(pag_actual * rpp, nrow(datos))
    
    datos_view <- datos[idx_inicio:idx_fin, ]
    tarjetas <- lapply(1:nrow(datos_view), function(i) {
      fila <- datos_view[i, ]
      
      # BOTÓN ADMIN CONDICIONAL
      btn_descarga <- NULL
      if (session_state$rol == "Admin") {
        btn_descarga <- tags$button(class="btn btn-sm btn-outline-primary", style="margin-top:10px;", tags$i(class="fas fa-download"), " Extraer Acta/Proyecto Físico")
      }
      
      tags$div(class = "ds-item-card",
        tags$div(class = "ds-item-thumbnail", tags$i(class = "fas fa-file-alt")),
        tags$div(class = "ds-item-metadata",
           tags$a(class = "ds-item-title", href="#", fila$titulo),
           tags$div(class = "ds-item-authors", paste("Responsable:", fila$autor)),
           tags$div(class = "ds-item-date", paste("Fecha Emisión:", fila$fecha)),
           tags$div(class = "ds-item-publisher", tags$i(class="fas fa-archive"), paste(" Ubicación:", fila$ubicacion)),
           tags$div(class = "ds-item-abstract", fila$abstract),
           tags$span(class = "ds-badge", fila$doc_type),
           br(), btn_descarga
        )
      )
    })
    
    paginador <- tags$div(class = "d-flex justify-content-center mt-4",
       tags$ul(class = "pagination",
         tags$li(class = ifelse(pag_actual == 1, "page-item disabled", "page-item"), actionLink("ext_prev", " Anterior", icon = icon("angle-double-left"), class="page-link ds-page-link")),
         tags$li(class = "page-item active", tags$span(class="page-link ds-page-active", paste("Pág", pag_actual, "de", tot_pags))),
         tags$li(class = ifelse(pag_actual >= tot_pags, "page-item disabled", "page-item"), actionLink("ext_next", "Siguiente ", icon = icon("angle-double-right"), class="page-link ds-page-link"))
       )
    )
    tagList(tags$div(class="ds-results-header", tags$h2(class="ds-results-title", "Archivo Extensión"), tags$span(class="ds-pagination-info", paste("Mostrando", idx_inicio, "-", idx_fin, "de", nrow(datos), "Resultados"))), tarjetas, paginador)
  })
  
  # ==========================================
  # LÓGICA DE DATOS: RRHH
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
    if (length(input$rrhh_doc_type) > 0) { datos <- datos[datos$doc_type %in% input$rrhh_doc_type, ] }
    if (!is.null(input$sort_rrhh)) {
      if (input$sort_rrhh == "Empleado A-Z") { datos <- datos[order(datos$empleado), ] } 
      else if (input$sort_rrhh == "Fecha Ingreso (Asc)") { datos <- datos[order(datos$fecha_ingreso), ] } 
      else if (input$sort_rrhh == "Fecha Ingreso (Desc)") { datos <- datos[order(datos$fecha_ingreso, decreasing = TRUE), ] }
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
    if (nrow(datos) == 0) return(tags$div(class = "alert alert-secondary", "No se encontraron expedientes laborales."))
    
    rpp <- as.numeric(input$rpp_rrhh)
    if (is.na(rpp)) rpp <- 5
    tot_pags <- ceiling(nrow(datos) / rpp)
    if (p_rrhh() > tot_pags) p_rrhh(max(1, tot_pags))
    pag_actual <- p_rrhh()
    idx_inicio <- (pag_actual - 1) * rpp + 1
    idx_fin <- min(pag_actual * rpp, nrow(datos))
    
    datos_view <- datos[idx_inicio:idx_fin, ]
    tarjetas <- lapply(1:nrow(datos_view), function(i) {
      fila <- datos_view[i, ]
      color_status <- ifelse(fila$estatus == "Jubilado", "purple", ifelse(fila$estatus == "Inactivo", "red", "green"))
      
      # BOTÓN ADMIN CONDICIONAL PARA PRIVADOS
      btn_descarga <- NULL
      if (session_state$rol == "Admin") {
        btn_descarga <- tags$button(class="btn btn-sm btn-outline-danger", style="margin-top:10px;", tags$i(class="fas fa-file-pdf"), " Desencriptar CV Completo")
      }
      
      tags$div(class = "ds-item-card",
        tags$div(class = "ds-item-thumbnail", tags$i(class = "fas fa-user-lock", style="color:#dc3545;")),
        tags$div(class = "ds-item-metadata",
           tags$a(class = "ds-item-title", href="#", paste("Expediente:", fila$empleado)),
           tags$div(class = "ds-item-authors", tags$strong(paste("C.I.:", fila$cedula))),
           tags$div(class = "ds-item-publisher", paste("Adscripción:", fila$departamento)),
           tags$div(class = "ds-item-date", paste("Ingreso:", fila$fecha_ingreso)),
           tags$div(class = "ds-item-publisher", tags$i(class="fas fa-box"), paste(" Retención Física:", fila$ubicacion)),
           tags$span(style=sprintf("color: white; background-color: %s; padding: 2px 6px; border-radius: 4px; font-size: 12px; margin-right: 5px;", color_status), fila$estatus),
           tags$span(class = "ds-badge", style="background-color: #6c757d;", fila$doc_type),
           br(), btn_descarga
        )
      )
    })
    
    paginador <- tags$div(class = "d-flex justify-content-center mt-4",
       tags$ul(class = "pagination",
         tags$li(class = ifelse(pag_actual == 1, "page-item disabled", "page-item"), actionLink("rrhh_prev", " Anterior", icon = icon("angle-double-left"), class="page-link ds-page-link")),
         tags$li(class = "page-item active", tags$span(class="page-link ds-page-active", paste("Pág", pag_actual, "de", tot_pags))),
         tags$li(class = ifelse(pag_actual >= tot_pags, "page-item disabled", "page-item"), actionLink("rrhh_next", "Siguiente ", icon = icon("angle-double-right"), class="page-link ds-page-link"))
       )
    )
    tagList(tags$div(class="ds-results-header", tags$h2(class="ds-results-title", "Expedientes Privados (RRHH)"), tags$span(class="ds-pagination-info", paste("Mostrando", idx_inicio, "-", idx_fin, "de", nrow(datos), "Resultados"))), tarjetas, paginador)
  })
  
  output$admin_workflow_list <- renderUI({
    pendientes <- list(list(titulo = db_ext$titulo[2], source = "Extensión"), list(titulo = db_rrhh$doc_type[1], source = "RRHH"))
    tareas <- lapply(1:length(pendientes), function(i) {
      item <- pendientes[[i]]
      tags$div(style = "padding: 15px; border-bottom: 1px solid #dee2e6; display: flex; justify-content: space-between; align-items: center;",
        tags$div(tags$strong(item$titulo, style="font-size: 1.1rem; color: #2b4e72;"), tags$br(), tags$span(style="color:#6c757d; font-size: 0.9rem;", paste("Contexto:", item$source))),
        tags$div(actionButton(paste0("btn_approve_", i), "Aprobar", icon=icon("check"), class="btn btn-success btn-sm"), actionButton(paste0("btn_reject_", i), "Rechazar", icon=icon("times"), class="btn btn-danger btn-sm", style="margin-left: 5px;"))
      )
    })
    tagList(tareas)
  })
  
  # ==========================================
  # RENDERIZADO MAESTRO DE NAVEGACIÓN
  # ==========================================
  output$page_content <- renderUI({
    if (!session_state$logged) {
      div(class="d-flex justify-content-center align-items-center", style="min-height: 100vh; background-color: #f7f9fa;",
          div(class="card p-5", style="width: 450px; box-shadow: 0 10px 20px rgba(0,0,0,0.1); border-top: 5px solid #2b4e72; text-align: center;",
              tags$i(class="fas fa-lock", style="font-size: 3rem; color: #dc3545; margin-bottom: 20px;"),
              tags$h3(style="font-family: 'Nunito', sans-serif; color: #2b4e72; font-weight: bold;", "Directorio Restringido"),
              tags$p(class="text-muted mb-4", "Sistema protegido para Extensión y RRHH"),
              div(style="text-align: left;",
                  textInput("login_user", "Credencial Institucional", placeholder="Ej: ext_admin"),
                  passwordInput("login_pass", "Contraseña", placeholder="********")
              ),
              actionButton("login_btn", "Validar Acceso Seguro", icon=icon("shield-alt"), class="btn w-100", style="background-color: #2b4e72; color: white; margin-top: 15px; border-radius: 20px; padding: 10px; font-weight: bold;")
          )
      )
    } else {
      
      # Construcción Quirúrgica de Pestañas y Componentes según permisos
      lista_menu <- list(bs4SidebarHeader("Comunidades"))
      lista_tabs <- list()
      
      if (session_state$modulo == "Extensión") {
        lista_menu[[length(lista_menu) + 1]] <- bs4SidebarMenuItem("Archivo Extensión", tabName = "tab_extension", icon = icon("university"))
        lista_tabs[[length(lista_tabs) + 1]] <- bs4TabItem(tabName = "tab_extension",
           fluidRow(column(12, div(class = "ds-breadcrumb-wrapper", div(class = "ds-breadcrumb", "Comunidades / Extensión / Búsqueda")))),
           fluidRow(
             column(width=3,
               bs4Card(title="Filtros Académicos", status="secondary", solidHeader=F, collapsible=F, width=12, checkboxGroupInput("ext_doc_type", "Tipología Documental:", choices = c("Proyecto de Investigación", "Plano Arquitectónico", "Acta", "Convenio"), selected = character(0)), div(style="margin-top:15px;", actionButton("btn_update_ext", "Aplicar", class="btn ds-btn-primary w-100"))),
               bs4Card(title="Ajustes de Vista", status="secondary", solidHeader=F, collapsible=F, width=12, class="mt-3", selectInput("sort_ext", "Ordenar por:", choices=c("Lo más relevante", "Título A-Z", "Fecha de Emisión (Asc)", "Fecha de Emisión (Desc)"), selectize=F), selectInput("rpp_ext", "Resultados por página:", choices=c("5", "10", "20"), selected="5", selectize=F))
             ),
             column(width=9,
               div(class="ds-search-bar", div(class="input-group", tags$input(id="search_ext", type="text", class="form-control ds-search-input", placeholder="Buscar proyectos..."), div(class="input-group-append", actionButton("btn_s_ext", label=NULL, icon=icon("search"), class="btn ds-btn-primary")))),
               uiOutput("list_extension")
             )
           )
        )
      }
      
      if (session_state$modulo == "RRHH") {
        lista_menu[[length(lista_menu) + 1]] <- bs4SidebarMenuItem("Expedientes RRHH", tabName = "tab_rrhh", icon = icon("lock"))
        lista_tabs[[length(lista_tabs) + 1]] <- bs4TabItem(tabName = "tab_rrhh",
           fluidRow(column(12, div(class = "ds-breadcrumb-wrapper", div(class = "ds-breadcrumb", "Comunidades / RRHH / Privado")))),
           fluidRow(
             column(width=3,
               bs4Card(title="Filtros Laborales", status="secondary", solidHeader=F, collapsible=F, width=12, radioButtons("rrhh_estatus", "Condición:", choices = c("Todos", "Activo", "Jubilado"), selected = "Todos"), tags$hr(), checkboxGroupInput("rrhh_doc_type", "Documento:", choices = c("Hoja de Vida", "Contrato", "Evaluación Desempeño", "Nómina"), selected = character(0)), div(style="margin-top:15px;", actionButton("btn_update_rrhh", "Aplicar", class="btn ds-btn-primary w-100"))),
               bs4Card(title="Ajustes de Vista", status="secondary", solidHeader=F, collapsible=F, width=12, class="mt-3", selectInput("sort_rrhh", "Ordenar por:", choices=c("Lo más relevante", "Empleado A-Z", "Fecha Ingreso (Asc)", "Fecha Ingreso (Desc)"), selectize=F), selectInput("rpp_rrhh", "Resultados por página:", choices=c("5", "10", "20"), selected="5", selectize=F))
             ),
             column(width=9,
               div(class="ds-search-bar", div(class="input-group", tags$input(id="search_rrhh", type="text", class="form-control ds-search-input", placeholder="Cédula o Empleado..."), div(class="input-group-append", actionButton("btn_s_rrhh", label=NULL, icon=icon("search"), class="btn ds-btn-primary")))),
               uiOutput("list_rrhh")
             )
           )
        )
      }
      
      if (session_state$rol == "Admin") {
        lista_menu[[length(lista_menu) + 1]] <- bs4SidebarHeader("Funciones Base")
        lista_menu[[length(lista_menu) + 1]] <- bs4SidebarMenuItem("Panel de Control", tabName = "mydspace_tab", icon = icon("inbox"), badgeLabel = "!", badgeColor = "danger")
        lista_tabs[[length(lista_tabs) + 1]] <- bs4TabItem(tabName = "mydspace_tab",
           tags$div(style="margin-bottom: 20px;", tags$h3(style="font-family: 'Nunito', sans-serif; color: #212529;", "Bandeja Administrativa")),
           fluidRow(bs4ValueBox(value="2", subtitle="Flujo de Trabajo", icon=icon("clipboard-check"), color="warning", width=4), bs4ValueBox(value="18", subtitle="Archivados", icon=icon("archive"), color="success", width=4)),
           fluidRow(column(width=12, bs4Card(title="Carga Documental Crítica", status="primary", solidHeader=F, width=12, collapsible=F, uiOutput("admin_workflow_list"))))
        )
      }
      
      # Armado del Dashboard Post-Login
      bs4DashPage(
        title = "Archivo Digital UCV",
        dark = NULL,
        header = bs4DashNavbar(
          status = "white", skin = "light",
          title = bs4DashBrand(title = "Archivo Digital UCV", color = "white", href = "#"),
          rightUi = tagList(
            tags$li(class="nav-item", tags$span(style="margin-top:10px; display:inline-block; margin-right:15px; font-weight:bold; color:#dc3545;", tags$i(class="fas fa-user-circle"), paste("ID:", session_state$username))),
            tags$li(class="nav-item", actionButton("logout_btn", "Cerrar", icon=icon("sign-out-alt"), class="btn btn-outline-secondary btn-sm", style="margin-top: 5px; margin-right: 15px;"))
          )
        ),
        sidebar = bs4DashSidebar(
          skin = "dark", status = "primary", elevation = 3,
          # Inyectamos solo los menús a los que el rol tiene acceso
          do.call(bs4SidebarMenu, c(list(id = "sidebarmenu"), lista_menu))
        ),
        body = bs4DashBody(
          tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
            tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")
          ),
          # Inyectamos solo los paneles (código html) a los que el rol tiene acceso
          do.call(bs4TabItems, lista_tabs)
        )
      )
    }
  })
}

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

    # Forzar tab inicial al autenticarse para aterrizar en buscadores.
    observeEvent(session_state$logged, {
        if (!isTRUE(session_state$logged)) return()

        target_tab <- if (session_state$modulo == "Extensión") {
            "tab_extension"
        } else if (session_state$modulo == "RRHH") {
            "tab_rrhh"
        } else if (session_state$rol == "Admin") {
            "mydspace_tab"
        } else {
            NULL
        }

        if (!is.null(target_tab)) {
            updatebs4SidebarMenu(session, inputId = "sidebar_tabs", selected = target_tab)
        }
    }, ignoreInit = TRUE)
  
  # ==========================================
  # RENDERIZADO DINÁMICO DE SIDEBAR
  # ==========================================
  output$sidebar_items <- renderUI({
        if (!isTRUE(session_state$logged)) {
            return(NULL)
        }
        tab_default <- if (session_state$modulo == "Extensión") {
            "tab_extension"
        } else if (session_state$modulo == "RRHH") {
            "tab_rrhh"
        } else if (session_state$rol == "Admin") {
            "mydspace_tab"
        } else {
            NULL
        }

    lista_menu <- list()
    
    if (session_state$modulo == "Extensión") {
            lista_menu[[length(lista_menu) + 1]] <- bs4SidebarMenuItem(
                "Archivo Extensión",
                tabName = "tab_extension",
                icon = icon("university"),
                selected = identical(tab_default, "tab_extension")
            )
    }
    if (session_state$modulo == "RRHH") {
            lista_menu[[length(lista_menu) + 1]] <- bs4SidebarMenuItem(
                "Expedientes RRHH",
                tabName = "tab_rrhh",
                icon = icon("lock"),
                selected = identical(tab_default, "tab_rrhh")
            )
    }
    if (session_state$rol == "Admin") {
            lista_menu[[length(lista_menu) + 1]] <- bs4SidebarMenuItem(
                "Panel de Control",
                tabName = "mydspace_tab",
                icon = icon("rocket"),
                badgeLabel = "!",
                badgeColor = "success",
                selected = identical(tab_default, "mydspace_tab")
            )
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
         # --- ENCABEZADO ---
         fluidRow(column(12, div(class = "ds-breadcrumb-wrapper", div(class = "ds-breadcrumb", tags$i(class="fas fa-shield-alt"), " Panel de Control / Administración")))),
         
         # --- KPIS ---
         fluidRow(
           column(3, uiOutput("kpi_total_docs")),
           column(3, uiOutput("kpi_total_categorias")),
           column(3, uiOutput("kpi_total_usuarios")),
           column(3, uiOutput("kpi_ultimo_ingreso"))
         ),
         
         # --- TABS DE GESTIÓN ---
         tabsetPanel(id = "admin_workspace_tabs", type = "pills",
            
            # TAB 1: NUEVO INGRESO (SUBMISSION WORKSPACE)
            tabPanel("Nuevo Ingreso", icon = icon("plus-circle"), tags$br(),
               fluidRow(
                  column(width=8,
                      bs4Card(title=tags$span(tags$i(class="fas fa-pen-fancy"), " Formulario de Metadatos Dublin Core"), status="primary", solidHeader=FALSE, width=12, collapsible=FALSE,
                          uiOutput("admin_submit_form"),
                          tags$hr(),
                          tags$div(class="d-flex justify-content-between align-items-center",
                              tags$span(class="text-muted", style="font-size:0.85rem;", tags$i(class="fas fa-info-circle"), " Los campos marcados con * son obligatorios"),
                              actionButton("btn_submit_workspace", "Registrar en Archivo", class="btn btn-success", icon=icon("cloud-upload-alt"), style="border-radius: 8px; font-weight: bold; padding: 10px 30px; box-shadow: 0 4px 6px rgba(40, 167, 69, 0.3);")
                          )
                      )
                  ),
                  column(width=4,
                      bs4Card(title=tags$span(tags$i(class="fas fa-cloud-upload-alt"), " Zona de Carga"), status="secondary", solidHeader=FALSE, width=12, collapsible=FALSE,
                          tags$div(style="border: 2px dashed #adb5bd; border-radius: 10px; padding: 30px; text-align: center; background-color: #f8f9fa; margin-bottom: 15px; transition: all 0.3s ease;",
                              tags$i(class="fas fa-file-upload", style="font-size: 3rem; color: #6c757d; margin-bottom: 10px;"),
                              tags$h5("ZONA DROP", style="color:#495057; font-weight:bold;"),
                              tags$p(style="color:#6c757d; font-size:0.85rem;", "Arrastra aquí o selecciona archivos"),
                              fileInput("file_upload", NULL, buttonLabel="Explorar...", placeholder="Ningún archivo", multiple = TRUE, accept = c(".pdf", ".zip", ".png", ".jpg", ".doc", ".docx"))
                          ),
                          tags$div(class="mt-3",
                              tags$p(tags$i(class="fas fa-check-circle", style="color:#28a745;"), " Formatos: PDF, DOC, ZIP, PNG", style="color:#6c757d; font-size:0.85rem; margin-bottom: 5px;"),
                              tags$p(tags$i(class="fas fa-weight-hanging", style="color:#ffc107;"), " Peso Máximo: 200MB", style="color:#6c757d; font-size:0.85rem; margin-bottom: 5px;"),
                              tags$p(tags$i(class="fas fa-star", style="color:#0056b3;"), " Recomendado: PDF/A (ISO 19005)", style="color:#6c757d; font-size:0.85rem;")
                          )
                      ),
                      bs4Card(title=tags$span(tags$i(class="fas fa-history"), " Últimos Ingresos"), status="white", solidHeader=FALSE, width=12, collapsible=TRUE, collapsed=TRUE,
                          uiOutput("recent_submissions")
                      )
                  )
               )
            ),
            
            # TAB 2: MONITOR DE EXPEDIENTES (TABLA CRUD)
            tabPanel("Monitor de Expedientes", icon = icon("table"), tags$br(),
               bs4Card(title=tags$span(tags$i(class="fas fa-database"), " Directorio Activo Local"), status="info", solidHeader=FALSE, width=12, collapsible=FALSE,
                   fluidRow(
                       column(4, textInput("admin_search", NULL, placeholder="Buscar en tabla...", width="100%")),
                       column(4, uiOutput("admin_filter_type")),
                       column(4, tags$div(style="text-align:right; padding-top: 5px;",
                           actionButton("btn_export_csv", "Exportar CSV", icon=icon("file-csv"), class="btn btn-outline-secondary btn-sm", style="margin-right:5px;"),
                           actionButton("btn_refresh_table", "Refrescar", icon=icon("sync-alt"), class="btn btn-outline-info btn-sm")
                       ))
                   ),
                   tags$hr(style="margin-top:0;"),
                   uiOutput("admin_control_table"),
                   tags$div(class="d-flex justify-content-between align-items-center mt-3",
                       tags$span(class="text-muted", style="font-size:0.85rem;", uiOutput("admin_table_summary", inline=TRUE)),
                       tags$div(
                           actionButton("admin_prev", tags$i(class="fas fa-chevron-left"), class="btn btn-outline-secondary btn-sm", style="margin-right:5px;"),
                           tags$span(class="text-muted", style="font-size:0.85rem;", textOutput("admin_page_info", inline=TRUE)),
                           actionButton("admin_next", tags$i(class="fas fa-chevron-right"), class="btn btn-outline-secondary btn-sm", style="margin-left:5px;")
                       )
                   )
               )
            ),
            
            # TAB 3: CATEGORÍAS Y TAXONOMÍAS
            tabPanel("Categorías", icon = icon("sitemap"), tags$br(),
               fluidRow(
                  column(width=5,
                     bs4Card(title=tags$span(tags$i(class="fas fa-plus-square"), " Nueva Categoría"), status="warning", solidHeader=FALSE, width=12, collapsible=FALSE,
                         textInput("new_tax_name", "Nombre de la Tipología *", placeholder="Ej: Resolución Rectoral", width="100%"),
                         textAreaInput("new_tax_desc", "Descripción", placeholder="Breve descripción de esta tipología documental...", rows=3, width="100%"),
                         selectInput("new_tax_scope", "Alcance", choices=c("Extensión", "RRHH", "Ambos"), width="100%"),
                         actionButton("add_tax_btn", "Registrar Categoría", icon=icon("check-circle"), class="btn btn-warning w-100 font-weight-bold", style="border-radius:8px;")
                     )
                  ),
                  column(width=7,
                     bs4Card(title=tags$span(tags$i(class="fas fa-tags"), " Taxonomías Activas"), status="secondary", solidHeader=FALSE, width=12, collapsible=FALSE,
                         uiOutput("admin_tax_list")
                     )
                  )
               )
            ),
            
            # TAB 4: USUARIOS Y PERMISOS
            tabPanel("Usuarios", icon = icon("users-cog"), tags$br(),
               bs4Card(title=tags$span(tags$i(class="fas fa-user-shield"), " Control de Acceso"), status="danger", solidHeader=FALSE, width=12, collapsible=FALSE,
                   uiOutput("admin_users_table"),
                   tags$hr(),
                   tags$h6(tags$i(class="fas fa-user-plus"), " Registrar Nuevo Usuario", style="font-weight:bold; margin-bottom:15px;"),
                   fluidRow(
                       column(3, textInput("new_user_name", NULL, placeholder="Usuario", width="100%")),
                       column(3, passwordInput("new_user_pass", NULL, placeholder="Contraseña", width="100%")),
                       column(3, selectInput("new_user_modulo", NULL, choices=c("Extensión", "RRHH"), width="100%")),
                       column(3, selectInput("new_user_rol", NULL, choices=c("Normal", "Admin"), width="100%"))
                   ),
                   actionButton("btn_add_user", "Crear Usuario", icon=icon("user-plus"), class="btn btn-outline-danger", style="border-radius:8px;")
               )
            ),
            
            # TAB 5: ESTADÍSTICAS
            tabPanel("Estadísticas", icon = icon("chart-bar"), tags$br(),
               fluidRow(
                  column(6, bs4Card(title=tags$span(tags$i(class="fas fa-chart-pie"), " Distribución por Tipo"), status="primary", solidHeader=FALSE, width=12, collapsible=FALSE, uiOutput("stats_by_type"))),
                  column(6, bs4Card(title=tags$span(tags$i(class="fas fa-calendar-alt"), " Cronología de Ingresos"), status="success", solidHeader=FALSE, width=12, collapsible=FALSE, uiOutput("stats_timeline")))
               ),
               fluidRow(
                  column(12, bs4Card(title=tags$span(tags$i(class="fas fa-server"), " Estado del Sistema"), status="secondary", solidHeader=FALSE, width=12, collapsible=FALSE, uiOutput("stats_system")))
               )
            )
         )
      )
    }
    
    do.call(bs4TabItems, lista_tabs)
  })

  # ==========================================
  # RENDERIZADORES DEL PANEL ADMIN
  # ==========================================
  
  # --- KPIs ---
  output$kpi_total_docs <- renderUI({
      df <- if (session_state$modulo == "Extensión") db_ext else db_rrhh
      bs4ValueBox(value = nrow(df), subtitle = "Registros Totales", icon = icon("archive"), color = "primary", width = 12)
  })
  output$kpi_total_categorias <- renderUI({
      df <- if (session_state$modulo == "Extensión") db_ext else db_rrhh
      bs4ValueBox(value = length(unique(df$doc_type)), subtitle = "Categorías Activas", icon = icon("tags"), color = "warning", width = 12)
  })
  output$kpi_total_usuarios <- renderUI({
      mod_users <- db_users[db_users$modulo == session_state$modulo, ]
      bs4ValueBox(value = nrow(mod_users), subtitle = "Usuarios del Módulo", icon = icon("users"), color = "success", width = 12)
  })
  output$kpi_ultimo_ingreso <- renderUI({
      df <- if (session_state$modulo == "Extensión") db_ext else db_rrhh
      fecha_col <- if (session_state$modulo == "Extensión") df$fecha else df$fecha_ingreso
      ultima <- max(as.Date(fecha_col, format="%Y-%m-%d"), na.rm = TRUE)
      bs4ValueBox(value = format(ultima, "%d/%m/%Y"), subtitle = "Última Entrada", icon = icon("clock"), color = "info", width = 12)
  })
  
  # --- FORMULARIO DINÁMICO DE SUBMISSION ---
  output$admin_submit_form <- renderUI({
      if (session_state$modulo == "Extensión") {
          tagList(
              fluidRow(
                  column(6, 
                      tags$label(class="font-weight-bold text-muted", "dc.title (Título) *"),
                      textInput("submit_title", NULL, placeholder="Ej: Análisis de Suelos en la Cuenca del Guaire", width="100%")
                  ),
                  column(6,
                      tags$label(class="font-weight-bold text-muted", "dc.contributor.author (Autor) *"),
                      textInput("submit_author", NULL, placeholder="Ej: Dr. Juan Pérez", width="100%")
                  )
              ),
              fluidRow(
                  column(6,
                      tags$label(class="font-weight-bold text-muted mt-2", "dc.type (Tipología) *"),
                      selectInput("submit_type", NULL, choices=c("Proyecto de Investigación","Plano Arquitectónico","Acta","Convenio","Informe"), width="100%")
                  ),
                  column(6,
                      tags$label(class="font-weight-bold text-muted mt-2", "dc.date.issued (Fecha) *"),
                      dateInput("submit_date", NULL, value = Sys.Date(), language="es", width="100%")
                  )
              ),
              tags$label(class="font-weight-bold text-muted mt-2", "dc.description.abstract (Resumen)"),
              textAreaInput("submit_abstract", NULL, rows=3, width="100%", placeholder="Describa brevemente el contenido del documento..."),
              tags$label(class="font-weight-bold text-muted mt-2", "dc.identifier.location (Ubicación Topográfica) *"),
              textInput("submit_location", NULL, placeholder="Ej: Archivo Central - Estante A1, Gaveta 4", width="100%")
          )
      } else {
          tagList(
              fluidRow(
                  column(6,
                      tags$label(class="font-weight-bold text-muted", "Nombre Completo *"),
                      textInput("submit_empleado", NULL, placeholder="Ej: Susana Pérez", width="100%")
                  ),
                  column(6,
                      tags$label(class="font-weight-bold text-muted", "Cédula de Identidad *"),
                      textInput("submit_cedula", NULL, placeholder="Ej: V-12345678", width="100%")
                  )
              ),
              fluidRow(
                  column(4,
                      tags$label(class="font-weight-bold text-muted mt-2", "Clasificación *"),
                      selectInput("submit_type", NULL, choices=c("Hoja de Vida","Contrato","Evaluación Desempeño","Nómina"), width="100%")
                  ),
                  column(4,
                      tags$label(class="font-weight-bold text-muted mt-2", "Departamento *"),
                      textInput("submit_depto", NULL, placeholder="Ej: Biología", width="100%")
                  ),
                  column(4,
                      tags$label(class="font-weight-bold text-muted mt-2", "Estatus *"),
                      selectInput("submit_estatus", NULL, choices=c("Activo","Inactivo","Jubilado"), width="100%")
                  )
              ),
              fluidRow(
                  column(6,
                      tags$label(class="font-weight-bold text-muted mt-2", "Fecha de Ingreso *"),
                      dateInput("submit_fecha_ingreso", NULL, value = Sys.Date(), language="es", width="100%")
                  ),
                  column(6,
                      tags$label(class="font-weight-bold text-muted mt-2", "Retención Física *"),
                      textInput("submit_location", NULL, placeholder="Archivo Pasivo - Caja J-02", width="100%")
                  )
              )
          )
      }
  })
  
  # --- ÚLTIMOS INGRESOS ---
  output$recent_submissions <- renderUI({
      df <- if (session_state$modulo == "Extensión") db_ext else db_rrhh
      n <- min(3, nrow(df))
      items <- lapply(1:n, function(i) {
          f <- df[i, ]
          nombre <- if (session_state$modulo == "Extensión") f$titulo else f$empleado
          tags$div(class="d-flex align-items-center mb-2 p-2", style="background:#f8f9fa; border-radius:8px;",
              tags$i(class="fas fa-file-alt mr-2", style="color:#2b4e72; font-size:1.2rem;"),
              tags$div(
                  tags$strong(nome <- substr(nombre, 1, 35), style="font-size:0.85rem;"),
                  tags$br(),
                  tags$span(class="text-muted", style="font-size:0.75rem;", f$doc_type)
              )
          )
      })
      tagList(items)
  })
  
  # --- TABLA CRUD COMPLETA ---
  p_admin <- reactiveVal(1)
  observeEvent(input$admin_prev, { p_admin(max(1, p_admin() - 1)) })
  observeEvent(input$admin_next, { p_admin(p_admin() + 1) })
  observeEvent(input$btn_refresh_table, { p_admin(1) })
  
  output$admin_filter_type <- renderUI({
      df <- if (session_state$modulo == "Extensión") db_ext else db_rrhh
      tipos <- unique(df$doc_type)
      selectInput("admin_type_filter", NULL, choices=c("Todos los tipos" = "", tipos), width="100%")
  })
  
  output$admin_control_table <- renderUI({
      df <- if (session_state$modulo == "Extensión") db_ext else db_rrhh
      
      # Filtrar por búsqueda
      if (!is.null(input$admin_search) && input$admin_search != "") {
          term <- tolower(input$admin_search)
          if (session_state$modulo == "Extensión") {
              df <- df[grepl(term, tolower(df$titulo)) | grepl(term, tolower(df$autor)), ]
          } else {
              df <- df[grepl(term, tolower(df$empleado)) | grepl(term, tolower(df$cedula)), ]
          }
      }
      # Filtrar por tipo
      if (!is.null(input$admin_type_filter) && input$admin_type_filter != "") {
          df <- df[df$doc_type == input$admin_type_filter, ]
      }
      
      if (nrow(df) == 0) return(tags$div(class="alert alert-secondary text-center", tags$i(class="fas fa-search"), " No se encontraron registros con estos filtros."))
      
      rpp <- 8
      tot_pags <- ceiling(nrow(df) / rpp)
      if (p_admin() > tot_pags) p_admin(tot_pags)
      pag <- p_admin()
      idx_i <- (pag - 1) * rpp + 1
      idx_f <- min(pag * rpp, nrow(df))
      df_view <- df[idx_i:idx_f, ]
      
      filas <- lapply(1:nrow(df_view), function(i) {
          f <- df_view[i, ]
          real_idx <- idx_i + i - 1
          
          if (session_state$modulo == "Extensión") {
              tags$tr(
                  tags$td(style="vertical-align:middle; font-weight:600;", substr(f$titulo, 1, 40)),
                  tags$td(style="vertical-align:middle;", f$autor),
                  tags$td(style="vertical-align:middle; font-size:0.9em;", f$fecha),
                  tags$td(style="vertical-align:middle;", tags$span(class="ds-badge", f$doc_type)),
                  tags$td(style="vertical-align:middle; font-size:0.85em; color:#6c757d;", f$ubicacion),
                  tags$td(style="vertical-align:middle;",
                      actionButton(paste0("view_",real_idx), NULL, icon=icon("eye"), class="btn btn-sm btn-outline-secondary", title="Ver"),
                      actionButton(paste0("edit_",real_idx), NULL, icon=icon("edit"), class="btn btn-sm btn-outline-info ml-1", title="Editar"),
                      actionButton(paste0("del_",real_idx), NULL, icon=icon("trash-alt"), class="btn btn-sm btn-outline-danger ml-1", title="Eliminar")
                  )
              )
          } else {
              color_st <- switch(f$estatus, "Activo"="#28a745", "Jubilado"="#6f42c1", "Inactivo"="#dc3545", "#6c757d")
              tags$tr(
                  tags$td(style="vertical-align:middle; font-weight:600;", f$empleado),
                  tags$td(style="vertical-align:middle;", f$cedula),
                  tags$td(style="vertical-align:middle;", f$departamento),
                  tags$td(style="vertical-align:middle;", tags$span(class="ds-badge", style=sprintf("background-color:%s;", color_st), f$estatus)),
                  tags$td(style="vertical-align:middle;", tags$span(class="ds-badge", style="background-color:#6c757d;", f$doc_type)),
                  tags$td(style="vertical-align:middle;",
                      actionButton(paste0("view_",real_idx), NULL, icon=icon("eye"), class="btn btn-sm btn-outline-secondary", title="Ver"),
                      actionButton(paste0("edit_",real_idx), NULL, icon=icon("edit"), class="btn btn-sm btn-outline-info ml-1", title="Editar"),
                      actionButton(paste0("del_",real_idx), NULL, icon=icon("trash-alt"), class="btn btn-sm btn-outline-danger ml-1", title="Eliminar")
                  )
              )
          }
      })
      
      encabezados <- if (session_state$modulo == "Extensión") {
          tags$tr(tags$th("Título"), tags$th("Autor"), tags$th("Fecha"), tags$th("Tipo"), tags$th("Ubicación"), tags$th("Acciones"))
      } else {
          tags$tr(tags$th("Empleado"), tags$th("C.I."), tags$th("Depto."), tags$th("Estatus"), tags$th("Tipo"), tags$th("Acciones"))
      }
      
      tags$div(class="table-responsive",
          tags$table(class="table table-hover mb-0", style="font-family:'Nunito',sans-serif; font-size:0.88rem;",
              tags$thead(class="thead-light", encabezados),
              tags$tbody(filas)
          )
      )
  })
  
  output$admin_table_summary <- renderUI({
      df <- if (session_state$modulo == "Extensión") db_ext else db_rrhh
      tags$span(paste("Total:", nrow(df), "registros en el módulo", session_state$modulo))
  })
  output$admin_page_info <- renderText({
      df <- if (session_state$modulo == "Extensión") db_ext else db_rrhh
      tot <- ceiling(nrow(df) / 8)
      paste("Pág", p_admin(), "de", tot)
  })
  
  # --- TAXONOMÍAS ---
  output$admin_tax_list <- renderUI({
      df <- if (session_state$modulo == "Extensión") db_ext else db_rrhh
      cats <- unique(df$doc_type)
      conteos <- table(df$doc_type)
      
      elementos <- lapply(cats, function(cat) {
          n <- conteos[[cat]]
          tags$div(class="d-flex justify-content-between align-items-center p-3 mb-2", style="background:#f8f9fa; border-radius:10px; border-left:4px solid #2b4e72;",
              tags$div(
                  tags$strong(cat, style="font-size:0.95rem;"),
                  tags$br(),
                  tags$span(class="text-muted", style="font-size:0.8rem;", paste(n, "documentos asignados"))
              ),
              tags$div(
                  tags$span(class="badge badge-pill", style="background-color:#2b4e72; color:white; padding:6px 12px; font-size:0.85rem;", n),
                  actionButton(paste0("tax_edit_", which(cats == cat)), NULL, icon=icon("edit"), class="btn btn-sm btn-outline-secondary ml-2")
              )
          )
      })
      
      tagList(
          tags$div(class="d-flex justify-content-between align-items-center mb-3",
              tags$span(class="text-muted", style="font-size:0.85rem;", paste(length(cats), "categorías registradas")),
              tags$span(class="badge badge-success", "Todas Activas")
          ),
          elementos
      )
  })
  
  # --- USUARIOS ---
  output$admin_users_table <- renderUI({
      filas <- lapply(1:nrow(db_users), function(i) {
          u <- db_users[i, ]
          color_rol <- if (u$rol == "Admin") "danger" else "info"
          tags$tr(
              tags$td(style="vertical-align:middle;", tags$i(class="fas fa-user-circle mr-2", style="color:#2b4e72;"), tags$strong(u$usuario)),
              tags$td(style="vertical-align:middle;", u$modulo),
              tags$td(style="vertical-align:middle;", tags$span(class=paste0("badge badge-", color_rol), u$rol)),
              tags$td(style="vertical-align:middle;", "••••••••"),
              tags$td(style="vertical-align:middle;",
                  actionButton(paste0("user_reset_", i), NULL, icon=icon("key"), class="btn btn-sm btn-outline-warning", title="Reset Password"),
                  actionButton(paste0("user_del_", i), NULL, icon=icon("user-times"), class="btn btn-sm btn-outline-danger ml-1", title="Revocar")
              )
          )
      })
      tags$table(class="table table-hover mb-0", style="font-family:'Nunito',sans-serif; font-size:0.88rem;",
          tags$thead(class="thead-light", tags$tr(tags$th("Usuario"), tags$th("Módulo"), tags$th("Rol"), tags$th("Pass"), tags$th("Acciones"))),
          tags$tbody(filas)
      )
  })
  
  # --- ESTADÍSTICAS ---
  output$stats_by_type <- renderUI({
      df <- if (session_state$modulo == "Extensión") db_ext else db_rrhh
      conteos <- sort(table(df$doc_type), decreasing=TRUE)
      total <- sum(conteos)
      
      barras <- lapply(names(conteos), function(tipo) {
          n <- conteos[[tipo]]
          pct <- round(n / total * 100)
          colores <- c("#2b4e72", "#0056b3", "#28a745", "#ffc107", "#dc3545", "#6f42c1")
          color <- colores[((which(names(conteos) == tipo) - 1) %% length(colores)) + 1]
          tags$div(class="mb-3",
              tags$div(class="d-flex justify-content-between", style="font-size:0.85rem;",
                  tags$span(tags$strong(tipo)),
                  tags$span(paste0(n, " (", pct, "%)"))
              ),
              tags$div(class="progress", style="height:8px; border-radius:4px;",
                  tags$div(class="progress-bar", role="progressbar", style=sprintf("width:%s%%; background-color:%s; border-radius:4px;", pct, color))
              )
          )
      })
      tagList(barras)
  })
  
  output$stats_timeline <- renderUI({
      df <- if (session_state$modulo == "Extensión") db_ext else db_rrhh
      fecha_col <- if (session_state$modulo == "Extensión") df$fecha else df$fecha_ingreso
      fechas <- as.Date(fecha_col, format="%Y-%m-%d")
      por_anio <- sort(table(format(fechas, "%Y")))
      
      items <- lapply(names(por_anio), function(anio) {
          n <- por_anio[[anio]]
          tags$div(class="d-flex align-items-center mb-2",
              tags$div(style="width:60px; text-align:right; margin-right:15px;",
                  tags$strong(anio, style="font-size:0.95rem; color:#2b4e72;")
              ),
              tags$div(style="flex-grow:1;",
                  tags$div(class="progress", style="height:20px; border-radius:10px;",
                      tags$div(class="progress-bar", role="progressbar", 
                          style=sprintf("width:%s%%; background: linear-gradient(135deg, #2b4e72, #0056b3); border-radius:10px; font-size:0.75rem; line-height:20px;", min(100, n * 20)),
                          paste(n, "docs"))
                  )
              )
          )
      })
      tagList(items)
  })
  
  output$stats_system <- renderUI({
      df <- if (session_state$modulo == "Extensión") db_ext else db_rrhh
      tags$div(class="row",
          tags$div(class="col-md-3 text-center p-3",
              tags$i(class="fas fa-database", style="font-size:2rem; color:#2b4e72; margin-bottom:8px;"),
              tags$h5(style="margin:0; font-weight:bold;", "CSV Local"),
              tags$span(class="text-muted", style="font-size:0.8rem;", "Motor de Datos")
          ),
          tags$div(class="col-md-3 text-center p-3",
              tags$i(class="fas fa-hdd", style="font-size:2rem; color:#28a745; margin-bottom:8px;"),
              tags$h5(style="margin:0; font-weight:bold;", paste(nrow(df), "Registros")),
              tags$span(class="text-muted", style="font-size:0.8rem;", "En Almacenamiento")
          ),
          tags$div(class="col-md-3 text-center p-3",
              tags$i(class="fas fa-shield-alt", style="font-size:2rem; color:#ffc107; margin-bottom:8px;"),
              tags$h5(style="margin:0; font-weight:bold;", "Zero-Trust"),
              tags$span(class="text-muted", style="font-size:0.8rem;", "Modelo de Seguridad")
          ),
          tags$div(class="col-md-3 text-center p-3",
              tags$i(class="fas fa-check-circle", style="font-size:2rem; color:#28a745; margin-bottom:8px;"),
              tags$h5(style="margin:0; font-weight:bold;", "Operativo"),
              tags$span(class="text-muted", style="font-size:0.8rem;", "Estado del Sistema")
          )
      )
  })
}

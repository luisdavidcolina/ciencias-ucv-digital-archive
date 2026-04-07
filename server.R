library(shiny)
library(bs4Dash)

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
        match <- authenticate_user(db_users, input$login_user, input$login_pass)
    
        if (!is.null(match) && nrow(match) == 1) {
      session_state$logged <- TRUE
      session_state$username <- match$usuario[1]
      session_state$modulo <- match$modulo[1]
      session_state$rol <- match$rol[1]

            # Trigger client-side navigation after successful login.
            session$sendCustomMessage("navigateToSearch", list(modulo = session_state$modulo))
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
        if (!isTRUE(session_state$logged)) {
            return(NULL)
        }
        tab_default <- resolve_default_tab(session_state$modulo, session_state$rol)

    lista_menu <- list()
    
    if (session_state$modulo == "Extensión") {
            lista_menu[[length(lista_menu) + 1]] <- bs4SidebarMenuItem(
                "Archivo Extensión",
                tabName = "tab_extension",
                icon = icon("folder-open"),
                selected = identical(tab_default, "tab_extension")
            )
    }
    if (session_state$modulo == "RRHH") {
            lista_menu[[length(lista_menu) + 1]] <- bs4SidebarMenuItem(
                "Expedientes RRHH",
                tabName = "tab_rrhh",
                icon = icon("id-card"),
                selected = identical(tab_default, "tab_rrhh")
            )
    }
    if (session_state$rol == "Admin") {
            lista_menu[[length(lista_menu) + 1]] <- bs4SidebarMenuItem(
                "Panel de Control",
                tabName = "mydspace_tab",
                icon = icon("sliders-h"),
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
    input$btn_s_ext
    input$btn_update_ext
        datos <- filter_extension_data(
            datos = db_ext,
            search_term = input$search_ext,
            doc_types = input$ext_doc_type,
            sort_mode = input$sort_ext
        )

        if (!is.null(input$ext_date_range) && length(input$ext_date_range) == 2) {
            f_ini <- as.Date(input$ext_date_range[1])
            f_fin <- as.Date(input$ext_date_range[2])
            fechas <- suppressWarnings(as.Date(datos$fecha, format = "%Y-%m-%d"))
            keep <- !is.na(fechas) & fechas >= f_ini & fechas <= f_fin
            datos <- datos[keep, , drop = FALSE]
        }

        if (!is.null(input$ext_year_range) && length(input$ext_year_range) == 2) {
            fechas <- suppressWarnings(as.Date(datos$fecha, format = "%Y-%m-%d"))
            anios <- as.integer(format(fechas, "%Y"))
            keep <- !is.na(anios) & anios >= input$ext_year_range[1] & anios <= input$ext_year_range[2]
            datos <- datos[keep, , drop = FALSE]
        }

        datos
  })
  
  observeEvent(input$ext_prev, { p_ext(max(1, p_ext() - 1)) })
  observeEvent(input$ext_next, { 
        rpp <- safe_as_numeric(input$rpp_ext)
        tot_pags <- compute_total_pages(nrow(dat_ext_react()), rpp)
    p_ext(min(tot_pags, p_ext() + 1)) 
  })

    output$ext_page_info <- renderText({
        datos <- dat_ext_react()
        if (nrow(datos) == 0) {
            return("Pág. 0 de 0")
        }

        rpp <- safe_as_numeric(input$rpp_ext)
        tot_pags <- max(1, compute_total_pages(nrow(datos), rpp))
        pag_actual <- clamp_page(p_ext(), tot_pags)
        paste("Pág.", pag_actual, "de", tot_pags)
    })

    output$ext_pagination_controls <- renderUI({
        datos <- dat_ext_react()
        rpp <- safe_as_numeric(input$rpp_ext)
        tot_pags <- compute_total_pages(nrow(datos), rpp)
        pag_actual <- clamp_page(p_ext(), tot_pags)

        prev_disabled <- pag_actual <= 1
        next_disabled <- pag_actual >= tot_pags
        prev_attr <- if (prev_disabled) "disabled" else NULL
        next_attr <- if (next_disabled) "disabled" else NULL

        div(
            class = "d-flex justify-content-between align-items-center mt-3",
            actionButton(
                "ext_prev",
                "Anterior",
                icon = icon("chevron-left"),
                class = "btn btn-outline-secondary btn-sm",
                disabled = prev_attr
            ),
            textOutput("ext_page_info", inline = TRUE),
            actionButton(
                "ext_next",
                "Siguiente",
                icon = icon("chevron-right"),
                class = "btn btn-outline-secondary btn-sm",
                disabled = next_attr
            )
        )
    })
  
  output$list_extension <- renderUI({
    datos <- dat_ext_react(); if (nrow(datos) == 0) return(div(class = "alert alert-secondary", "No se encontraron proyectos."))
    rpp <- as.numeric(input$rpp_ext); tot_pags <- ceiling(nrow(datos) / rpp)
    pag_actual <- p_ext(); idx_inicio <- (pag_actual - 1) * rpp + 1; idx_fin <- min(pag_actual * rpp, nrow(datos))
    datos_view <- datos[idx_inicio:idx_fin, ]
    tarjetas <- lapply(1:nrow(datos_view), function(i) {
      fila <- datos_view[i, ]
      idx_real <- idx_inicio + i - 1
            btn_actions <- if (session_state$rol == "Admin") {
                div(
                    class = "ds-item-actions",
                    tags$button(type = "button", class = "btn btn-sm btn-outline-info ds-action-btn", title = "Visualizar", onclick = sprintf("Shiny.setInputValue('open_doc', {mod: 'ext', idx: %s, nonce: Date.now()}, {priority: 'event'});", idx_real), tags$i(class = "fas fa-eye")),
                    tags$button(type = "button", class = "btn btn-sm btn-outline-warning ds-action-btn", title = "Editar", tags$i(class = "fas fa-pen")),
                    tags$button(type = "button", class = "btn btn-sm btn-outline-primary ds-action-btn", title = "Descargar", tags$i(class = "fas fa-download"))
                )
            } else {
                NULL
            }

            div(class = "ds-item-card", div(class = "ds-item-thumbnail", tags$i(class = "fas fa-file-alt")),
        div(class = "ds-item-metadata", tags$a(class = "ds-item-title", href="#", onclick = sprintf("Shiny.setInputValue('open_doc', {mod: 'ext', idx: %s, nonce: Date.now()}, {priority: 'event'}); return false;", idx_real), fila$titulo),
           div(class = "ds-item-authors", paste("Responsable:", fila$autor)),
           div(class = "ds-item-date", paste("Fecha Emisión:", fila$fecha)),
                     div(class = "ds-item-publisher", paste("Ubicación física:", fila$ubicacion)),
           div(class = "ds-item-abstract", fila$abstract),
                     tags$span(class = "ds-badge", fila$doc_type)),
                btn_actions)
    })
    tagList(tarjetas)
  })

  observeEvent(c(input$btn_s_rrhh, input$btn_update_rrhh), { p_rrhh(1) }, ignoreInit = TRUE)
  dat_rrhh_react <- reactive({
        datos <- filter_rrhh_data(db_rrhh, input$search_rrhh)

        if (!is.null(input$rrhh_doc_type) && nzchar(input$rrhh_doc_type)) {
            datos <- datos[datos$doc_type == input$rrhh_doc_type, , drop = FALSE]
        }

        if (!is.null(input$rrhh_estatus) && nzchar(input$rrhh_estatus) && input$rrhh_estatus != "Todos") {
            datos <- datos[datos$estatus == input$rrhh_estatus, , drop = FALSE]
        }

        if (!is.null(input$rrhh_date_range) && length(input$rrhh_date_range) == 2) {
            f_ini <- as.Date(input$rrhh_date_range[1])
            f_fin <- as.Date(input$rrhh_date_range[2])
            fechas <- suppressWarnings(as.Date(datos$fecha_ingreso, format = "%Y-%m-%d"))
            keep <- !is.na(fechas) & fechas >= f_ini & fechas <= f_fin
            datos <- datos[keep, , drop = FALSE]
        }

        if (!is.null(input$rrhh_year_range) && length(input$rrhh_year_range) == 2) {
            fechas <- suppressWarnings(as.Date(datos$fecha_ingreso, format = "%Y-%m-%d"))
            anios <- as.integer(format(fechas, "%Y"))
            keep <- !is.na(anios) & anios >= input$rrhh_year_range[1] & anios <= input$rrhh_year_range[2]
            datos <- datos[keep, , drop = FALSE]
        }

        datos
  })

    show_doc_modal <- function(doc, mod) {
        is_admin <- identical(session_state$rol, "Admin")

        if (identical(mod, "ext")) {
            titulo <- doc$titulo
            resumen <- if (!is.null(doc$abstract) && nzchar(doc$abstract)) doc$abstract else "Sin resumen disponible."
            thumb_icon <- "fas fa-file-alt"
            thumb_badge <- doc$doc_type
            meta <- tagList(
                div(class = "ds-doc-meta-row", tags$span(class = "k", "Responsable"), tags$span(class = "v", doc$autor)),
                div(class = "ds-doc-meta-row", tags$span(class = "k", "Tipo"), tags$span(class = "v", doc$doc_type)),
                div(class = "ds-doc-meta-row", tags$span(class = "k", "Fecha de emisión"), tags$span(class = "v", doc$fecha)),
                div(class = "ds-doc-meta-row", tags$span(class = "k", "Ubicación física"), tags$span(class = "v", doc$ubicacion))
            )
        } else {
            titulo <- paste("Expediente:", doc$empleado)
            resumen <- paste("Documento de RRHH en estado", doc$estatus, "adscrito a", doc$departamento)
            thumb_icon <- "fas fa-user-lock"
            thumb_badge <- doc$estatus
            meta <- tagList(
                div(class = "ds-doc-meta-row", tags$span(class = "k", "Empleado"), tags$span(class = "v", doc$empleado)),
                div(class = "ds-doc-meta-row", tags$span(class = "k", "Cédula"), tags$span(class = "v", doc$cedula)),
                div(class = "ds-doc-meta-row", tags$span(class = "k", "Tipo"), tags$span(class = "v", doc$doc_type)),
                div(class = "ds-doc-meta-row", tags$span(class = "k", "Ubicación física"), tags$span(class = "v", doc$ubicacion)),
                div(class = "ds-doc-meta-row", tags$span(class = "k", "Fecha de ingreso"), tags$span(class = "v", doc$fecha_ingreso))
            )
        }

        footer_actions <- list(
            modalButton("Cerrar"),
            actionButton("doc_view_btn", NULL, icon = icon("eye"), class = "btn btn-outline-info", title = "Visualizar")
        )

        if (is_admin) {
            footer_actions <- c(
                footer_actions,
                list(
                    actionButton("doc_edit_btn", NULL, icon = icon("pen"), class = "btn btn-outline-warning", title = "Editar"),
                    actionButton("doc_download_btn", NULL, icon = icon("download"), class = "btn btn-outline-primary", title = "Descargar")
                )
            )
        }

        showModal(modalDialog(
            title = NULL,
            size = "l",
            easyClose = TRUE,
            class = "ds-doc-modal",
            footer = do.call(tagList, footer_actions),
            div(class = "ds-doc-modal-head", tags$i(class = "fas fa-file-alt"), tags$h4(titulo)),
            div(class = "ds-doc-modal-grid",
                    div(class = "ds-doc-panel ds-doc-thumb-panel", tags$h5("Miniatura"), div(class = "ds-doc-thumb", tags$i(class = thumb_icon), tags$span(class = "ds-doc-thumb-badge", thumb_badge))),
                    div(class = "ds-doc-panel", tags$h5("Metadata"), meta),
                    div(class = "ds-doc-panel", tags$h5("Descripción"), tags$p(class = "ds-doc-abstract", resumen))
            )
        ))
    }

    observeEvent(input$open_doc, {
        payload <- input$open_doc
        req(!is.null(payload$mod), !is.null(payload$idx))

        idx <- as.integer(payload$idx)
        if (is.na(idx) || idx < 1) return()

        if (identical(payload$mod, "ext")) {
            datos <- dat_ext_react()
            if (idx <= nrow(datos)) show_doc_modal(datos[idx, ], "ext")
        } else if (identical(payload$mod, "rrhh")) {
            datos <- dat_rrhh_react()
            if (idx <= nrow(datos)) show_doc_modal(datos[idx, ], "rrhh")
        }
    }, ignoreInit = TRUE)

    observeEvent(input$doc_edit_btn, {
        showNotification("Modo edición en construcción.", type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$doc_view_btn, {
        showNotification("Vista de documento en construcción.", type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$doc_download_btn, {
        showNotification("Descarga en construcción.", type = "message")
    }, ignoreInit = TRUE)
  
  output$list_rrhh <- renderUI({
    datos <- dat_rrhh_react(); if (nrow(datos) == 0) return(div(class = "alert alert-secondary", "No hay expedientes."))
    tarjetas <- lapply(1:nrow(datos), function(i) {
      fila <- datos[i, ]
      div(class = "ds-item-card", div(class = "ds-item-thumbnail", tags$i(class = "fas fa-user-lock", style="color:#dc3545;")),
                div(class = "ds-item-metadata", tags$a(class = "ds-item-title", href="#", onclick = sprintf("Shiny.setInputValue('open_doc', {mod: 'rrhh', idx: %s, nonce: Date.now()}, {priority: 'event'}); return false;", i), paste("Expediente:", fila$empleado)),
           div(class = "ds-item-authors", tags$strong(paste("C.I.:", fila$cedula))),
           div(class = "ds-item-publisher", paste("Adscripción:", fila$departamento)),
                     div(class = "ds-item-date", paste("Ubicación física:", fila$ubicacion)),
           tags$span(class = "ds-badge", style="background-color: #6c757d;", fila$doc_type)))
    })
    tagList(tarjetas)
  })

  # ==========================================
  # CUERPO PRINCIPAL DEL DASHBOARD
  # ==========================================
  output$main_body <- renderUI({
    req(session_state$logged)
    build_main_body_ui(session_state, db_ext, db_rrhh)
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

  # --- FILTROS DE ESTADÍSTICAS ---
  output$stats_filters_panel <- renderUI({
      req(session_state$logged, session_state$rol == "Admin")

      df <- if (session_state$modulo == "Extensión") db_ext else db_rrhh
      fecha_col <- if (session_state$modulo == "Extensión") "fecha" else "fecha_ingreso"
      fechas <- suppressWarnings(as.Date(df[[fecha_col]], format = "%Y-%m-%d"))
      fechas_ok <- fechas[!is.na(fechas)]

      min_fecha <- if (length(fechas_ok) > 0) min(fechas_ok) else Sys.Date() - 365
      max_fecha <- if (length(fechas_ok) > 0) max(fechas_ok) else Sys.Date()

      third_filter_ui <- if (session_state$modulo == "RRHH") {
          selectInput("stats_status_filter", "Estatus", choices = c("Todos" = "", sort(unique(df$estatus))), width = "100%")
      } else {
          selectInput("stats_author_filter", "Responsable", choices = c("Todos" = "", sort(unique(df$autor))), width = "100%")
      }

      fourth_filter_ui <- if (session_state$modulo == "RRHH") {
          selectInput("stats_dept_filter", "Departamento", choices = c("Todos" = "", sort(unique(df$departamento))), width = "100%")
      } else {
          checkboxInput("stats_only_recent", "Solo últimos 24 meses", value = FALSE)
      }

      tagList(
          fluidRow(
              column(3,
                  dateRangeInput("stats_date_range", "Rango de fechas", start = min_fecha, end = max_fecha, language = "es", width = "100%")
              ),
              column(3,
                  selectInput("stats_type_filter", "Tipología", choices = c("Todas" = "", sort(unique(df$doc_type))), width = "100%")
              ),
              column(3, third_filter_ui),
              column(3, fourth_filter_ui)
          ),
          fluidRow(
              column(12,
                  tags$div(style = "text-align:right; margin-top:6px;",
                      actionButton("stats_reset_filters", "Limpiar filtros", icon = icon("eraser"), class = "btn btn-outline-secondary btn-sm")
                  )
              )
          )
      )
  })

  observeEvent(input$stats_reset_filters, {
      updateSelectInput(session, "stats_type_filter", selected = "")
      updateSelectInput(session, "stats_status_filter", selected = "")
      updateSelectInput(session, "stats_dept_filter", selected = "")
      updateSelectInput(session, "stats_author_filter", selected = "")
      updateCheckboxInput(session, "stats_only_recent", value = FALSE)
  }, ignoreInit = TRUE)

  stats_filtered_df <- reactive({
      req(session_state$logged, session_state$rol == "Admin")

      df <- if (session_state$modulo == "Extensión") db_ext else db_rrhh
      fecha_col <- if (session_state$modulo == "Extensión") "fecha" else "fecha_ingreso"
      fechas <- suppressWarnings(as.Date(df[[fecha_col]], format = "%Y-%m-%d"))

      # Rango de fechas
      if (!is.null(input$stats_date_range) && length(input$stats_date_range) == 2) {
          f_ini <- as.Date(input$stats_date_range[1])
          f_fin <- as.Date(input$stats_date_range[2])
          keep <- !is.na(fechas) & fechas >= f_ini & fechas <= f_fin
          df <- df[keep, , drop = FALSE]
          fechas <- fechas[keep]
      }

      # Tipología
      if (!is.null(input$stats_type_filter) && nzchar(input$stats_type_filter)) {
          keep <- df$doc_type == input$stats_type_filter
          df <- df[keep, , drop = FALSE]
          fechas <- fechas[keep]
      }

      if (session_state$modulo == "RRHH") {
          if (!is.null(input$stats_status_filter) && nzchar(input$stats_status_filter)) {
              keep <- df$estatus == input$stats_status_filter
              df <- df[keep, , drop = FALSE]
              fechas <- fechas[keep]
          }
          if (!is.null(input$stats_dept_filter) && nzchar(input$stats_dept_filter)) {
              keep <- df$departamento == input$stats_dept_filter
              df <- df[keep, , drop = FALSE]
              fechas <- fechas[keep]
          }
      } else {
          if (!is.null(input$stats_author_filter) && nzchar(input$stats_author_filter)) {
              keep <- df$autor == input$stats_author_filter
              df <- df[keep, , drop = FALSE]
              fechas <- fechas[keep]
          }
          if (isTRUE(input$stats_only_recent)) {
              cutoff <- Sys.Date() - 730
              keep <- !is.na(fechas) & fechas >= cutoff
              df <- df[keep, , drop = FALSE]
              fechas <- fechas[keep]
          }
      }

      list(df = df, fechas = fechas)
  })
  
  # --- ESTADÍSTICAS ---
  output$stats_by_type <- renderUI({
      payload <- stats_filtered_df()
      df <- payload$df
      if (nrow(df) == 0) {
          return(tags$div(class="alert alert-secondary", "No hay datos para los filtros seleccionados."))
      }
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
      payload <- stats_filtered_df()
      fechas <- payload$fechas
      fechas <- fechas[!is.na(fechas)]
      if (length(fechas) == 0) {
          return(tags$div(class="alert alert-secondary", "No hay fechas para visualizar con los filtros actuales."))
      }
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
      payload <- stats_filtered_df()
      df <- payload$df
      system_status <- if (nrow(df) > 0) "Operativo" else "Sin Datos"
      tags$div(class="row",
          tags$div(class="col-md-3 text-center p-3",
              tags$i(class="fas fa-database", style="font-size:2rem; color:#2b4e72; margin-bottom:8px;"),
              tags$h5(style="margin:0; font-weight:bold;", "CSV Local"),
              tags$span(class="text-muted", style="font-size:0.8rem;", "Motor de Datos")
          ),
          tags$div(class="col-md-3 text-center p-3",
              tags$i(class="fas fa-hdd", style="font-size:2rem; color:#28a745; margin-bottom:8px;"),
              tags$h5(style="margin:0; font-weight:bold;", paste(nrow(df), "Registros")),
              tags$span(class="text-muted", style="font-size:0.8rem;", "Filtrados")
          ),
          tags$div(class="col-md-3 text-center p-3",
              tags$i(class="fas fa-shield-alt", style="font-size:2rem; color:#ffc107; margin-bottom:8px;"),
              tags$h5(style="margin:0; font-weight:bold;", "Zero-Trust"),
              tags$span(class="text-muted", style="font-size:0.8rem;", "Modelo de Seguridad")
          ),
          tags$div(class="col-md-3 text-center p-3",
              tags$i(class="fas fa-check-circle", style="font-size:2rem; color:#28a745; margin-bottom:8px;"),
              tags$h5(style="margin:0; font-weight:bold;", system_status),
              tags$span(class="text-muted", style="font-size:0.8rem;", "Estado del Sistema")
          )
      )
    })
}

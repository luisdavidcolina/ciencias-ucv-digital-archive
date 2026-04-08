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

        datos <- filter_by_tesauro(datos, input$ext_tesauro)

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
                                         div(class = "ds-item-tags", render_tesauro_badges(fila))),
                btn_actions)
    })
    tagList(tarjetas)
  })

  observeEvent(c(input$btn_s_rrhh, input$btn_update_rrhh), { p_rrhh(1) }, ignoreInit = TRUE)
  dat_rrhh_react <- reactive({
      input$btn_s_rrhh
      input$btn_update_rrhh
        datos <- filter_rrhh_data(db_rrhh, input$search_rrhh)

        datos <- filter_by_doc_types(datos, input$rrhh_doc_type)
      datos <- filter_by_persons(datos, input$rrhh_people)
        datos <- filter_by_tesauro(datos, input$rrhh_tesauro)

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

    register_document_modal_handlers(
        input = input,
        output = output,
        session = session,
        session_state = session_state,
        dat_ext_react = dat_ext_react,
        dat_rrhh_react = dat_rrhh_react
    )
  
  output$list_rrhh <- renderUI({
    datos <- dat_rrhh_react(); if (nrow(datos) == 0) return(div(class = "alert alert-secondary", "No hay expedientes."))
    tarjetas <- lapply(1:nrow(datos), function(i) {
      fila <- datos[i, ]
      div(class = "ds-item-card", div(class = "ds-item-thumbnail", tags$i(class = "fas fa-user-lock", style="color:#dc3545;")),
                div(class = "ds-item-metadata", tags$a(class = "ds-item-title", href="#", onclick = sprintf("Shiny.setInputValue('open_doc', {mod: 'rrhh', idx: %s, nonce: Date.now()}, {priority: 'event'}); return false;", i), paste("Expediente:", fila$empleado)),
           div(class = "ds-item-authors", tags$strong(paste("C.I.:", fila$cedula))),
         div(class = "ds-item-authors", paste("Personas asociadas:", fila$personas_relacionadas)),
           div(class = "ds-item-publisher", paste("Adscripción:", fila$departamento)),
                     div(class = "ds-item-date", paste("Ubicación física:", fila$ubicacion)),
                     div(class = "ds-item-tags", render_tesauro_badges(fila))))
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

  register_admin_panel_outputs(
      input = input,
      output = output,
      session = session,
      session_state = session_state,
      db_ext = db_ext,
      db_rrhh = db_rrhh,
      db_users = db_users
  )

  register_stats_admin_outputs(
      input = input,
      output = output,
      session = session,
      session_state = session_state,
      db_ext = db_ext,
      db_rrhh = db_rrhh
  )
}

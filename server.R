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
  
  p_archivo <- reactiveVal(1)
  p_rrhh <- reactiveVal(1)

  # Límites absolutos de fechas para resetear Archivo
  archivo_fechas_raw <- suppressWarnings(as.Date(db_archivo$fecha, format = "%Y-%m-%d"))
  archivo_fechas_raw <- archivo_fechas_raw[!is.na(archivo_fechas_raw)]
  abs_archivo_min_fecha <- if (length(archivo_fechas_raw) > 0) min(archivo_fechas_raw) else Sys.Date() - 365
  abs_archivo_max_fecha <- if (length(archivo_fechas_raw) > 0) max(archivo_fechas_raw) else Sys.Date()
  abs_archivo_min_year <- as.integer(format(abs_archivo_min_fecha, "%Y"))
  abs_archivo_max_year <- as.integer(format(abs_archivo_max_fecha, "%Y"))

  # Límites absolutos de fechas para resetear RRHH
  rrhh_fechas_raw <- suppressWarnings(as.Date(db_rrhh$fecha_ingreso, format = "%Y-%m-%d"))
  rrhh_fechas_raw <- rrhh_fechas_raw[!is.na(rrhh_fechas_raw)]
  abs_rrhh_min_fecha <- if (length(rrhh_fechas_raw) > 0) min(rrhh_fechas_raw) else Sys.Date() - 365
  abs_rrhh_max_fecha <- if (length(rrhh_fechas_raw) > 0) max(rrhh_fechas_raw) else Sys.Date()
  abs_rrhh_min_year <- as.integer(format(abs_rrhh_min_fecha, "%Y"))
  abs_rrhh_max_year <- as.integer(format(abs_rrhh_max_fecha, "%Y"))

  # Helper para conversión de fecha segura (evita errores charToDate durante digitación del usuario)
  safe_get_year <- function(date_val) {
    if (is.null(date_val) || length(date_val) == 0) return(NA_integer_)
    tryCatch({
      d <- as.Date(date_val)
      if (is.na(d)) return(NA_integer_)
      as.integer(format(d, "%Y"))
    }, error = function(e) {
      NA_integer_
    })
  }

  safe_check_dates_equal <- function(d1, d2) {
    tryCatch({
      as.Date(d1) == as.Date(d2)
    }, error = function(e) {
      FALSE
    })
  }

  # Sincronización Archivo
  observeEvent(input$archivo_date_range, {
    dates <- input$archivo_date_range
    req(length(dates) == 2)
    
    y_start <- safe_get_year(dates[1])
    y_end <- safe_get_year(dates[2])
    req(!is.na(y_start), !is.na(y_end))
    
    # Sincronizar el Slider si los valores no coinciden
    if (!is.null(input$archivo_year_range)) {
      current_slider <- input$archivo_year_range
      if (current_slider[1] != y_start || current_slider[2] != y_end) {
        updateSliderInput(session, "archivo_year_range", value = c(y_start, y_end))
      }
    }
    
    # Sincronizar el Selector de Año Específico si no coincide
    if (!is.null(input$archivo_specific_year)) {
      current_spec <- input$archivo_specific_year
      expected_spec <- if (y_start == y_end) as.character(y_start) else ""
      if (current_spec != expected_spec) {
        updateSelectInput(session, "archivo_specific_year", selected = expected_spec)
      }
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$archivo_year_range, {
    years <- input$archivo_year_range
    req(length(years) == 2, !any(is.na(years)))
    
    # Si el date range actual ya cubre el mismo año o rango de años, NO lo sobreescribimos.
    if (!is.null(input$archivo_date_range)) {
      current_dates <- input$archivo_date_range
      if (length(current_dates) == 2) {
        y_start <- safe_get_year(current_dates[1])
        y_end <- safe_get_year(current_dates[2])
        if (!is.na(y_start) && !is.na(y_end) && y_start == years[1] && y_end == years[2]) {
          return()
        }
      }
    }
    
    expected_start <- as.Date(paste0(years[1], "-01-01"))
    expected_end <- as.Date(paste0(years[2], "-12-31"))
    
    updateDateRangeInput(session, "archivo_date_range", start = expected_start, end = expected_end)
    
    if (!is.null(input$archivo_specific_year)) {
      current_spec <- input$archivo_specific_year
      expected_spec <- if (years[1] == years[2]) as.character(years[1]) else ""
      if (current_spec != expected_spec) {
        updateSelectInput(session, "archivo_specific_year", selected = expected_spec)
      }
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$archivo_specific_year, {
    spec_year <- input$archivo_specific_year
    req(!is.null(spec_year), !is.na(spec_year))
    
    if (nzchar(spec_year)) {
      y <- as.integer(spec_year)
      req(!is.na(y))
      
      # Si las fechas ya están dentro de este año específico, no las machacamos
      if (!is.null(input$archivo_date_range)) {
        current_dates <- input$archivo_date_range
        if (length(current_dates) == 2) {
          y_start <- safe_get_year(current_dates[1])
          y_end <- safe_get_year(current_dates[2])
          if (!is.na(y_start) && !is.na(y_end) && y_start == y && y_end == y) {
            return()
          }
        }
      }
      
      expected_start <- as.Date(paste0(y, "-01-01"))
      expected_end <- as.Date(paste0(y, "-12-31"))
      
      updateDateRangeInput(session, "archivo_date_range", start = expected_start, end = expected_end)
      
      if (!is.null(input$archivo_year_range)) {
        current_slider <- input$archivo_year_range
        if (current_slider[1] != y || current_slider[2] != y) {
          updateSliderInput(session, "archivo_year_range", value = c(y, y))
        }
      }
    } else {
      # Si es "Todos", solo restablecemos si el rango anterior estaba acotado a un único año
      if (!is.null(input$archivo_date_range)) {
        current_dates <- input$archivo_date_range
        if (length(current_dates) == 2) {
          y_start <- safe_get_year(current_dates[1])
          y_end <- safe_get_year(current_dates[2])
          
          if (!is.na(y_start) && !is.na(y_end) && y_start == y_end) {
            if (!safe_check_dates_equal(current_dates[1], abs_archivo_min_fecha) || 
                !safe_check_dates_equal(current_dates[2], abs_archivo_max_fecha)) {
              updateDateRangeInput(session, "archivo_date_range", start = abs_archivo_min_fecha, end = abs_archivo_max_fecha)
            }
            
            if (!is.null(input$archivo_year_range)) {
              current_slider <- input$archivo_year_range
              if (current_slider[1] != abs_archivo_min_year || current_slider[2] != abs_archivo_max_year) {
                updateSliderInput(session, "archivo_year_range", value = c(abs_archivo_min_year, abs_archivo_max_year))
              }
            }
          }
        }
      }
    }
  }, ignoreInit = TRUE)

  # Sincronización RRHH
  observeEvent(input$rrhh_date_range, {
    dates <- input$rrhh_date_range
    req(length(dates) == 2)
    
    y_start <- safe_get_year(dates[1])
    y_end <- safe_get_year(dates[2])
    req(!is.na(y_start), !is.na(y_end))
    
    # Sincronizar el Slider si los valores no coinciden
    if (!is.null(input$rrhh_year_range)) {
      current_slider <- input$rrhh_year_range
      if (current_slider[1] != y_start || current_slider[2] != y_end) {
        updateSliderInput(session, "rrhh_year_range", value = c(y_start, y_end))
      }
    }
    
    # Sincronizar el Selector de Año Específico si no coincide
    if (!is.null(input$rrhh_specific_year)) {
      current_spec <- input$rrhh_specific_year
      expected_spec <- if (y_start == y_end) as.character(y_start) else ""
      if (current_spec != expected_spec) {
        updateSelectInput(session, "rrhh_specific_year", selected = expected_spec)
      }
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$rrhh_year_range, {
    years <- input$rrhh_year_range
    req(length(years) == 2, !any(is.na(years)))
    
    # Si el date range actual ya cubre el mismo año o rango de años, NO lo sobreescribimos.
    if (!is.null(input$rrhh_date_range)) {
      current_dates <- input$rrhh_date_range
      if (length(current_dates) == 2) {
        y_start <- safe_get_year(current_dates[1])
        y_end <- safe_get_year(current_dates[2])
        if (!is.na(y_start) && !is.na(y_end) && y_start == years[1] && y_end == years[2]) {
          return()
        }
      }
    }
    
    expected_start <- as.Date(paste0(years[1], "-01-01"))
    expected_end <- as.Date(paste0(years[2], "-12-31"))
    
    updateDateRangeInput(session, "rrhh_date_range", start = expected_start, end = expected_end)
    
    if (!is.null(input$rrhh_specific_year)) {
      current_spec <- input$rrhh_specific_year
      expected_spec <- if (years[1] == years[2]) as.character(years[1]) else ""
      if (current_spec != expected_spec) {
        updateSelectInput(session, "rrhh_specific_year", selected = expected_spec)
      }
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$rrhh_specific_year, {
    spec_year <- input$rrhh_specific_year
    req(!is.null(spec_year), !is.na(spec_year))
    
    if (nzchar(spec_year)) {
      y <- as.integer(spec_year)
      req(!is.na(y))
      
      # Si las fechas ya están dentro de este año específico, no las machacamos
      if (!is.null(input$rrhh_date_range)) {
        current_dates <- input$rrhh_date_range
        if (length(current_dates) == 2) {
          y_start <- safe_get_year(current_dates[1])
          y_end <- safe_get_year(current_dates[2])
          if (!is.na(y_start) && !is.na(y_end) && y_start == y && y_end == y) {
            return()
          }
        }
      }
      
      expected_start <- as.Date(paste0(y, "-01-01"))
      expected_end <- as.Date(paste0(y, "-12-31"))
      
      updateDateRangeInput(session, "rrhh_date_range", start = expected_start, end = expected_end)
      
      if (!is.null(input$rrhh_year_range)) {
        current_slider <- input$rrhh_year_range
        if (current_slider[1] != y || current_slider[2] != y) {
          updateSliderInput(session, "rrhh_year_range", value = c(y, y))
        }
      }
    } else {
      # Si es "Todos", solo restablecemos si el rango anterior estaba acotado a un único año
      if (!is.null(input$rrhh_date_range)) {
        current_dates <- input$rrhh_date_range
        if (length(current_dates) == 2) {
          y_start <- safe_get_year(current_dates[1])
          y_end <- safe_get_year(current_dates[2])
          
          if (!is.na(y_start) && !is.na(y_end) && y_start == y_end) {
            if (!safe_check_dates_equal(current_dates[1], abs_rrhh_min_fecha) || 
                !safe_check_dates_equal(current_dates[2], abs_rrhh_max_fecha)) {
              updateDateRangeInput(session, "rrhh_date_range", start = abs_rrhh_min_fecha, end = abs_rrhh_max_fecha)
            }
            
            if (!is.null(input$rrhh_year_range)) {
              current_slider <- input$rrhh_year_range
              if (current_slider[1] != abs_rrhh_min_year || current_slider[2] != abs_rrhh_max_year) {
                updateSliderInput(session, "rrhh_year_range", value = c(abs_rrhh_min_year, abs_rrhh_max_year))
              }
            }
          }
        }
      }
    }
  }, ignoreInit = TRUE)

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
      log_event(match$usuario[1], "Login Success", match$modulo[1], paste("Rol:", match$rol[1]))
      session_state$logged <- TRUE
      session_state$username <- match$usuario[1]
      session_state$modulo <- match$modulo[1]
      session_state$rol <- match$rol[1]

            # Persistir sesión en el cliente
            session$sendCustomMessage("persistSession", list(
              username = match$usuario[1],
              modulo = match$modulo[1],
              rol = match$rol[1]
            ))

            # Trigger client-side navigation after successful login.
            session$sendCustomMessage("navigateToSearch", list(modulo = session_state$modulo))
    } else {
      log_event(input$login_user, "Login Failure", "Auth", "Credenciales incorrectas", "Failure")
      showNotification("Credenciales incorrectas", type = "error")
    }
  })

  observeEvent(session_state$logged, {
    if (!isTRUE(session_state$logged)) return()

    modulo_actual <- session_state$modulo
    rol_actual <- session_state$rol

    target_tab <- resolve_default_tab(modulo_actual, rol_actual)
    if (is.null(target_tab) || !nzchar(target_tab)) {
      target_tab <- if (identical(modulo_actual, "RRHH")) "tab_rrhh" else "tab_archivo"
    }

    session$onFlushed(function() {
      # Try server-side tab update first, then keep JS fallback for compatibility.
      try(updateTabItems(session, "sidebar_tabs", selected = target_tab), silent = TRUE)
      session$sendCustomMessage("navigateToSearch", list(modulo = modulo_actual))
    }, once = TRUE)
  }, ignoreInit = TRUE)
  
  observeEvent(input$logout_btn, {
    session_state$logged <- FALSE
    session_state$username <- NULL
    session_state$modulo <- NULL
    session_state$rol <- NULL
    # Limpiar sesión persistida en el cliente
    session$sendCustomMessage("clearPersistedSession", list())
  })

  observeEvent(input$restore_session, {
    saved <- input$restore_session
    req(saved, saved$username)
    
    match <- restore_user_session(db_users, saved$username)
    if (!is.null(match) && nrow(match) == 1) {
      log_event(match$usuario[1], "Session Restored", match$modulo[1], paste("Rol:", match$rol[1]))
      session_state$logged <- TRUE
      session_state$username <- match$usuario[1]
      session_state$modulo <- match$modulo[1]
      session_state$rol <- match$rol[1]
      
      session$sendCustomMessage("navigateToSearch", list(modulo = session_state$modulo))
    }
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
    
    if (session_state$modulo == "Archivo") {
            lista_menu[[length(lista_menu) + 1]] <- bs4SidebarMenuItem(
          "Archivo Institucional",
                tabName = "tab_archivo",
                icon = icon("folder-open"),
                selected = identical(tab_default, "tab_archivo")
            )
    }
    if (session_state$modulo == "RRHH") {
            lista_menu[[length(lista_menu) + 1]] <- bs4SidebarMenuItem(
          "Archivos RRHH",
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
  observeEvent(c(input$btn_s_archivo, input$btn_update_archivo), { p_archivo(1) }, ignoreInit = TRUE)
  observeEvent(input$btn_clear_archivo, {
    archivo_fechas <- suppressWarnings(as.Date(db_archivo$fecha, format = "%Y-%m-%d"))
    archivo_fechas <- archivo_fechas[!is.na(archivo_fechas)]
    archivo_min_fecha <- if (length(archivo_fechas) > 0) min(archivo_fechas) else Sys.Date() - 365
    archivo_max_fecha <- if (length(archivo_fechas) > 0) max(archivo_fechas) else Sys.Date()
    archivo_min_year <- as.integer(format(archivo_min_fecha, "%Y"))
    archivo_max_year <- as.integer(format(archivo_max_fecha, "%Y"))

    updateTextInput(session, "search_archivo", value = "")
    updateSelectizeInput(session, "archivo_doc_type", selected = character(0))
    updateSelectizeInput(session, "archivo_tesauro", selected = character(0))
    updateDateRangeInput(session, "archivo_date_range", start = archivo_min_fecha, end = archivo_max_fecha)
    updateSliderInput(session, "archivo_year_range", value = c(archivo_min_year, archivo_max_year))
    p_archivo(1)
  }, ignoreInit = TRUE)

  dat_archivo_react <- reactive({
    input$btn_s_archivo
    input$btn_update_archivo
        datos <- filter_archivo_data(
            datos = db_archivo,
            search_term = input$search_archivo,
            doc_types = input$archivo_doc_type,
            sort_mode = input$sort_archivo
        )

        datos <- filter_by_tesauro(datos, input$archivo_tesauro)

        if (!is.null(input$archivo_date_range) && length(input$archivo_date_range) == 2) {
            f_ini <- as.Date(input$archivo_date_range[1])
            f_fin <- as.Date(input$archivo_date_range[2])
            fechas <- suppressWarnings(as.Date(datos$fecha, format = "%Y-%m-%d"))
            keep <- !is.na(fechas) & fechas >= f_ini & fechas <= f_fin
            datos <- datos[keep, , drop = FALSE]
        }

        if (!is.null(input$archivo_year_range) && length(input$archivo_year_range) == 2) {
            fechas <- suppressWarnings(as.Date(datos$fecha, format = "%Y-%m-%d"))
            anios <- as.integer(format(fechas, "%Y"))
            keep <- !is.na(anios) & anios >= input$archivo_year_range[1] & anios <= input$archivo_year_range[2]
            datos <- datos[keep, , drop = FALSE]
        }

        if (!is.null(input$archivo_specific_year) && nzchar(input$archivo_specific_year)) {
            fechas <- suppressWarnings(as.Date(datos$fecha, format = "%Y-%m-%d"))
            anios <- as.integer(format(fechas, "%Y"))
            target_year <- as.integer(input$archivo_specific_year)
            keep <- !is.na(anios) & anios == target_year
            datos <- datos[keep, , drop = FALSE]
        }

        datos
  })

  ext_pagination <- reactive({
  paginate_rows(dat_archivo_react(), p_archivo(), input$rpp_archivo)
  })
  
  observeEvent(input$archivo_prev, { p_archivo(max(1, p_archivo() - 1)) })
  observeEvent(input$archivo_next, { 
    pag <- ext_pagination()
  p_archivo(min(pag$total_pages, p_archivo() + 1)) 
  })

    output$archivo_page_info <- renderText({
    pag <- ext_pagination()
    if (nrow(pag$data) == 0) {
            return("Pág. 0 de 0")
        }

    paste("Pág.", pag$page, "de", pag$total_pages)
    })

    output$ext_pagination_controls <- renderUI({
    pag <- ext_pagination()
    build_pagination_controls("archivo_prev", "archivo_next", "archivo_page_info", pag$page, pag$total_pages)
    })
  
  output$list_archivo <- renderUI({
  pag <- ext_pagination()
  if (nrow(pag$data) == 0) return(tags$div(class = "text-center my-5 py-4", tags$i(class = "fas fa-folder-open fa-4x text-muted mb-3 opacity-50"), tags$h5(class = "text-secondary font-weight-bold", "El Archivo está vacío"), tags$p(class = "text-muted", "No hay folios institucionales que coincidan con estos filtros.")))
  datos_view <- pag$data
  idx_inicio <- pag$start
  tarjetas <- lapply(seq_len(nrow(datos_view)), function(i) {
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
           div(class = "ds-item-authors", paste("Autor:", fila$autor)),
           div(class = "ds-item-date", paste("Fecha Emisión:", fila$fecha)),
                     div(class = "ds-item-publisher", paste("Ubicación física:", fila$ubicacion)),
                     div(class = "ds-item-abstract", fila$abstract),
                                         div(class = "ds-item-tags", render_tesauro_badges(fila))),
                btn_actions)
    })
    tagList(tarjetas)
  })

  output$download_archivo_xls <- downloadHandler(
    filename = function() {
      paste0("archivo_institucional_archivo_", format(Sys.Date(), "%Y%m%d"), ".xls")
    },
    content = function(file) {
      log_event(session_state$username, "Export XLS", session_state$modulo, "Descarga de reporte filtrado")
      export_df <- dat_archivo_react()
      write_excel_compatible_xls(
        path = file,
        title = "Exportación de resultados de Archivo Institucional - Archivo",
        sections = list(
          list(
            title = "Resultados filtrados",
            note = paste0("Registros exportados: ", nrow(export_df)),
            df = export_df
          )
        )
      )
    }
  )

  observeEvent(c(input$btn_s_rrhh, input$btn_update_rrhh), { p_rrhh(1) }, ignoreInit = TRUE)
  observeEvent(input$btn_clear_rrhh, {
    rrhh_fechas <- suppressWarnings(as.Date(db_rrhh$fecha_ingreso, format = "%Y-%m-%d"))
    rrhh_fechas <- rrhh_fechas[!is.na(rrhh_fechas)]
    rrhh_min_fecha <- if (length(rrhh_fechas) > 0) min(rrhh_fechas) else Sys.Date() - 365
    rrhh_max_fecha <- if (length(rrhh_fechas) > 0) max(rrhh_fechas) else Sys.Date()
    rrhh_min_year <- as.integer(format(rrhh_min_fecha, "%Y"))
    rrhh_max_year <- as.integer(format(rrhh_max_fecha, "%Y"))

    updateTextInput(session, "search_rrhh", value = "")
    updateSelectizeInput(session, "rrhh_doc_type", selected = character(0))
    updateCheckboxGroupInput(session, "rrhh_estado", selected = character(0))
    updateSelectizeInput(session, "rrhh_people", selected = character(0))
    updateDateRangeInput(session, "rrhh_date_range", start = rrhh_min_fecha, end = rrhh_max_fecha)
    updateSliderInput(session, "rrhh_year_range", value = c(rrhh_min_year, rrhh_max_year))
    p_rrhh(1)
  }, ignoreInit = TRUE)

  dat_rrhh_react <- reactive({
      input$btn_s_rrhh
      input$btn_update_rrhh
        datos <- filter_rrhh_data(db_rrhh, NULL, input$sort_rrhh)

        datos <- filter_by_doc_types(datos, input$rrhh_doc_type)
      datos <- filter_by_persons(datos, input$rrhh_people)

        if (!is.null(input$rrhh_estado) && length(input$rrhh_estado) > 0) {
          estado_buscado <- input$rrhh_estado
          if ("Retirado" %in% estado_buscado) {
            estado_buscado <- unique(c(estado_buscado, "Jubilado"))
          }
          datos <- datos[datos$estado %in% estado_buscado, , drop = FALSE]
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

        if (!is.null(input$rrhh_specific_year) && nzchar(input$rrhh_specific_year)) {
            fechas <- suppressWarnings(as.Date(datos$fecha_ingreso, format = "%Y-%m-%d"))
            anios <- as.integer(format(fechas, "%Y"))
            target_year <- as.integer(input$rrhh_specific_year)
            keep <- !is.na(anios) & anios == target_year
            datos <- datos[keep, , drop = FALSE]
        }

        datos
  })

  rrhh_person_index <- reactive({
    index_df <- build_rrhh_person_index(dat_rrhh_react())
    filter_rrhh_person_index(index_df, input$search_rrhh, sort_mode = input$sort_rrhh)
  })

  rrhh_pagination <- reactive({
    paginate_rows(rrhh_person_index(), p_rrhh(), input$rpp_rrhh)
  })

  observeEvent(input$rrhh_prev, { p_rrhh(max(1, p_rrhh() - 1)) })
  observeEvent(input$rrhh_next, {
        pag <- rrhh_pagination()
    p_rrhh(min(pag$total_pages, p_rrhh() + 1))
  })

  output$rrhh_page_info <- renderText({
    pag <- rrhh_pagination()
    if (nrow(pag$data) == 0) {
      return("Pág. 0 de 0")
    }

    paste("Pág.", pag$page, "de", pag$total_pages)
  })

  output$rrhh_pagination_controls <- renderUI({
    pag <- rrhh_pagination()
    build_pagination_controls("rrhh_prev", "rrhh_next", "rrhh_page_info", pag$page, pag$total_pages)
  })

    register_document_modal_handlers(
        input = input,
        output = output,
        session = session,
        session_state = session_state,
        dat_archivo_react = dat_archivo_react,
        dat_rrhh_react = dat_rrhh_react
    )
  
  output$list_rrhh <- renderUI({
    pag <- rrhh_pagination()
    if (nrow(pag$data) == 0) return(tags$div(class = "text-center my-5 py-5", tags$i(class = "fas fa-id-badge fa-4x text-muted mb-3 opacity-50"), tags$h5(class = "text-secondary font-weight-bold", "Sin Perfiles Activos"), tags$p(class = "text-muted", "No existen trabajadores o expedientes asociados a tu búsqueda.")))

    datos_view <- pag$data
    idx_inicio <- pag$start

    tarjetas <- lapply(seq_len(nrow(datos_view)), function(i) {
      fila <- datos_view[i, ]
      persona_js <- jsonlite::toJSON(fila$persona_raw, auto_unbox = TRUE)
      card_badges <- tags$div(
        class = "ds-item-tags",
        tags$span(class = "ds-badge ds-badge-status", if (nzchar(fila$estatuses)) fila$estatuses else "Sin estado"),
        tags$span(class = "ds-badge", if (nzchar(fila$tipos)) fila$tipos else "Sin tipologías")
      )

      btn_actions <- if (session_state$rol == "Admin") {
        div(
          class = "ds-item-actions",
          tags$button(
            type = "button",
            class = "btn btn-sm btn-outline-info ds-action-btn",
            title = "Visualizar",
            onclick = sprintf("Shiny.setInputValue('open_doc', {mod: 'rrhh_person', person: %s, nonce: Date.now()}, {priority: 'event'});", persona_js),
            tags$i(class = "fas fa-eye")
          ),
          tags$button(type = "button", class = "btn btn-sm btn-outline-warning ds-action-btn", title = "Editar", tags$i(class = "fas fa-pen")),
          tags$button(type = "button", class = "btn btn-sm btn-outline-primary ds-action-btn", title = "Descargar", tags$i(class = "fas fa-download"))
        )
      } else {
        NULL
      }

      div(
        class = "ds-item-card",
        div(class = "ds-item-thumbnail", tags$i(class = "fas fa-user-circle", style = "color:#2b4e72;")),
        div(
          class = "ds-item-metadata",
          tags$a(
            class = "ds-item-title",
            href = "#",
            onclick = sprintf(
              "Shiny.setInputValue('open_doc', {mod: 'rrhh_person', person: %s, nonce: Date.now()}, {priority: 'event'}); return false;",
              persona_js
            ),
            fila$persona
          ),
          div(class = "ds-item-authors", paste("Cargo:", if (nzchar(fila$cargos)) fila$cargos else "Sin cargo asignado")),
          div(class = "ds-item-date", paste("Ingreso:", if (nzchar(fila$fecha_ingreso)) fila$fecha_ingreso else "Sin fecha", "| C.I.:", if (nzchar(fila$cedulas)) fila$cedulas else "Sin cédula")),
          div(class = "ds-item-publisher", paste("Adscripción:", if (nzchar(fila$departamentos)) fila$departamentos else "Sin dependencia o AP")),
          card_badges
        ),
        btn_actions
      )
    })
    tagList(tarjetas)
  })

  output$download_rrhh_xls <- downloadHandler(
    filename = function() {
      paste0("archivo_institucional_rrhh_", format(Sys.Date(), "%Y%m%d"), ".xls")
    },
    content = function(file) {
      person_index <- rrhh_person_index()
      rrhh_data <- dat_rrhh_react()

      summary_df <- if (nrow(person_index) > 0) {
        person_index[, c("persona", "doc_count", "cedulas", "departamentos", "estatuses", "tipos"), drop = FALSE]
      } else {
        data.frame(
          persona = character(0),
          doc_count = integer(0),
          cedulas = character(0),
          departamentos = character(0),
          estatuses = character(0),
          tipos = character(0),
          stringsAsFactors = FALSE
        )
      }

      sections <- list(
        list(
          title = "Resumen de personas",
          note = paste0("Personas visibles: ", nrow(person_index)),
          df = summary_df
        )
      )

      if (nrow(person_index) > 0) {
        person_sections <- lapply(seq_len(nrow(person_index)), function(i) {
          person_row <- person_index[i, ]
          indices <- unlist(person_row$row_indices, use.names = FALSE)
          file_rows <- if (length(indices) > 0) rrhh_data[indices, , drop = FALSE] else rrhh_data[0, , drop = FALSE]

          list(
            title = paste0("Expediente: ", person_row$persona),
            note = paste0("Expedientes asociados: ", person_row$doc_count),
            df = file_rows
          )
        })

        sections <- c(sections, person_sections)
      }

      write_excel_compatible_xls(
        path = file,
        title = "Exportación de expedientes de Archivo Institucional - RRHH",
        sections = sections
      )
    }
  )

  # ==========================================
  # CUERPO PRINCIPAL DEL DASHBOARD
  # ==========================================
  output$main_body <- renderUI({
    req(session_state$logged)
    build_main_body_ui(session_state, db_archivo, db_rrhh)
  })

  register_admin_panel_outputs(
      input = input,
      output = output,
      session = session,
      session_state = session_state,
      db_archivo = db_archivo,
      db_rrhh = db_rrhh,
      db_users = db_users
  )

  register_stats_admin_outputs(
      input = input,
      output = output,
      session = session,
      session_state = session_state,
      db_archivo = db_archivo,
      db_rrhh = db_rrhh
  )
}

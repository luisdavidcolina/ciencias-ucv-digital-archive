register_stats_admin_outputs <- function(input, output, session, session_state, db_ext, db_rrhh) {
  # --- FILTROS DE ESTADISTICAS ---
  output$stats_filters_panel <- renderUI({
    req(session_state$logged, session_state$rol == "Admin")

    df <- if (session_state$modulo == "Extensión") db_ext else db_rrhh
    type_choices <- extract_doc_type_set(df$doc_type)
    fecha_col <- if (session_state$modulo == "Extensión") "fecha" else "fecha_ingreso"
    fechas <- suppressWarnings(as.Date(df[[fecha_col]], format = "%Y-%m-%d"))
    fechas_ok <- fechas[!is.na(fechas)]

    min_fecha <- if (length(fechas_ok) > 0) min(fechas_ok) else Sys.Date() - 365
    max_fecha <- if (length(fechas_ok) > 0) max(fechas_ok) else Sys.Date()

    third_filter_ui <- if (session_state$modulo == "RRHH") {
      selectInput("stats_status_filter", "Estado", choices = c("Todos" = "", sort(unique(df$estado))), width = "100%")
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
          selectizeInput("stats_type_filter", "Tipología", choices = type_choices, multiple = TRUE, width = "100%")
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

    if (!is.null(input$stats_date_range) && length(input$stats_date_range) == 2) {
      f_ini <- as.Date(input$stats_date_range[1])
      f_fin <- as.Date(input$stats_date_range[2])
      keep <- !is.na(fechas) & fechas >= f_ini & fechas <= f_fin
      df <- df[keep, , drop = FALSE]
      fechas <- fechas[keep]
    }

    if (!is.null(input$stats_type_filter) && length(input$stats_type_filter) > 0) {
      keep <- vapply(df$doc_type, row_has_any_doc_type, logical(1), selected_types = input$stats_type_filter)
      df <- df[keep, , drop = FALSE]
      fechas <- fechas[keep]
    }

    if (session_state$modulo == "RRHH") {
      if (!is.null(input$stats_status_filter) && nzchar(input$stats_status_filter)) {
        keep <- df$estado == input$stats_status_filter
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

  # --- ESTADISTICAS ---
  output$stats_by_type <- renderUI({
    payload <- stats_filtered_df()
    df <- payload$df
    if (nrow(df) == 0) {
      return(tags$div(class = "alert alert-secondary", "No hay datos para los filtros seleccionados."))
    }
    conteos <- sort(table(df$doc_type), decreasing = TRUE)
    total <- sum(conteos)

    barras <- lapply(names(conteos), function(tipo) {
      n <- conteos[[tipo]]
      pct <- round(n / total * 100)
      colores <- c("#2b4e72", "#0056b3", "#28a745", "#ffc107", "#dc3545", "#6f42c1")
      color <- colores[((which(names(conteos) == tipo) - 1) %% length(colores)) + 1]
      tags$div(class = "mb-3",
        tags$div(class = "d-flex justify-content-between", style = "font-size:0.85rem;",
          tags$span(tags$strong(tipo)),
          tags$span(paste0(n, " (", pct, "%)"))
        ),
        tags$div(class = "progress", style = "height:8px; border-radius:4px;",
          tags$div(class = "progress-bar", role = "progressbar", style = sprintf("width:%s%%; background-color:%s; border-radius:4px;", pct, color))
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
      return(tags$div(class = "alert alert-secondary", "No hay fechas para visualizar con los filtros actuales."))
    }
    por_anio <- sort(table(format(fechas, "%Y")))

    items <- lapply(names(por_anio), function(anio) {
      n <- por_anio[[anio]]
      tags$div(class = "d-flex align-items-center mb-2",
        tags$div(style = "width:60px; text-align:right; margin-right:15px;",
          tags$strong(anio, style = "font-size:0.95rem; color:#2b4e72;")
        ),
        tags$div(style = "flex-grow:1;",
          tags$div(class = "progress", style = "height:20px; border-radius:10px;",
            tags$div(class = "progress-bar", role = "progressbar",
              style = sprintf("width:%s%%; background: linear-gradient(135deg, #2b4e72, #0056b3); border-radius:10px; font-size:0.75rem; line-height:20px;", min(100, n * 20)),
              paste(n, "docs")
            )
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
    tags$div(class = "row",
      tags$div(class = "col-md-3 text-center p-3",
        tags$i(class = "fas fa-database", style = "font-size:2rem; color:#2b4e72; margin-bottom:8px;"),
        tags$h5(style = "margin:0; font-weight:bold;", "CSV Local"),
        tags$span(class = "text-muted", style = "font-size:0.8rem;", "Motor de Datos")
      ),
      tags$div(class = "col-md-3 text-center p-3",
        tags$i(class = "fas fa-hdd", style = "font-size:2rem; color:#28a745; margin-bottom:8px;"),
        tags$h5(style = "margin:0; font-weight:bold;", paste(nrow(df), "Registros")),
        tags$span(class = "text-muted", style = "font-size:0.8rem;", "Filtrados")
      ),
      tags$div(class = "col-md-3 text-center p-3",
        tags$i(class = "fas fa-shield-alt", style = "font-size:2rem; color:#ffc107; margin-bottom:8px;"),
        tags$h5(style = "margin:0; font-weight:bold;", "Zero-Trust"),
        tags$span(class = "text-muted", style = "font-size:0.8rem;", "Modelo de Seguridad")
      ),
      tags$div(class = "col-md-3 text-center p-3",
        tags$i(class = "fas fa-check-circle", style = "font-size:2rem; color:#28a745; margin-bottom:8px;"),
        tags$h5(style = "margin:0; font-weight:bold;", system_status),
        tags$span(class = "text-muted", style = "font-size:0.8rem;", "Estado del Sistema")
      )
    )
  })
}

register_admin_panel_outputs <- function(input, output, session, session_state, db_ext, db_rrhh, db_users) {
  rrhh_admin_data <- reactiveVal(db_rrhh)
  rrhh_people_store <- reactiveVal(extract_people_choices(db_rrhh))

  get_mod_df <- function() {
    if (identical(session_state$modulo, "Extensión")) {
      db_ext
    } else {
      rrhh_admin_data()
    }
  }

  # --- KPIs ---
  output$kpi_total_docs <- renderUI({
    df <- get_mod_df()
    bs4ValueBox(value = nrow(df), subtitle = "Registros Totales", icon = icon("archive"), color = "primary", width = 12)
  })

  output$kpi_total_categorias <- renderUI({
    df <- get_mod_df()
    bs4ValueBox(value = length(unique(df$doc_type)), subtitle = "Categorías Activas", icon = icon("tags"), color = "warning", width = 12)
  })

  output$kpi_total_usuarios <- renderUI({
    mod_users <- db_users[db_users$modulo == session_state$modulo, ]
    bs4ValueBox(value = nrow(mod_users), subtitle = "Usuarios del Módulo", icon = icon("users"), color = "success", width = 12)
  })

  output$kpi_ultimo_ingreso <- renderUI({
    df <- get_mod_df()
    fecha_col <- if (session_state$modulo == "Extensión") df$fecha else df$fecha_ingreso
    ultima <- max(as.Date(fecha_col, format = "%Y-%m-%d"), na.rm = TRUE)
    bs4ValueBox(value = format(ultima, "%d/%m/%Y"), subtitle = "Última Entrada", icon = icon("clock"), color = "info", width = 12)
  })

  # --- FORMULARIO DINÁMICO DE SUBMISSION ---
  output$admin_submit_form <- renderUI({
    if (session_state$modulo == "Extensión") {
      tagList(
        fluidRow(
          column(6,
            tags$label(class = "font-weight-bold text-muted", "dc.title (Título) *"),
            textInput("submit_title", NULL, placeholder = "Ej: Análisis de Suelos en la Cuenca del Guaire", width = "100%")
          ),
          column(6,
            tags$label(class = "font-weight-bold text-muted", "dc.contributor.author (Autor) *"),
            textInput("submit_author", NULL, placeholder = "Ej: Dr. Juan Pérez", width = "100%")
          )
        ),
        fluidRow(
          column(6,
            tags$label(class = "font-weight-bold text-muted mt-2", "dc.type (Tipología) *"),
            selectInput("submit_type", NULL, choices = c("Proyecto de Investigación", "Plano Arquitectónico", "Acta", "Convenio", "Informe"), width = "100%")
          ),
          column(6,
            tags$label(class = "font-weight-bold text-muted mt-2", "dc.date.issued (Fecha) *"),
            dateInput("submit_date", NULL, value = Sys.Date(), language = "es", width = "100%")
          )
        ),
        tags$label(class = "font-weight-bold text-muted mt-2", "dc.description.abstract (Resumen)"),
        textAreaInput("submit_abstract", NULL, rows = 3, width = "100%", placeholder = "Describa brevemente el contenido del documento..."),
        tags$label(class = "font-weight-bold text-muted mt-2", "dc.identifier.location (Ubicación Topográfica) *"),
        textInput("submit_location", NULL, placeholder = "Ej: Archivo Central - Estante A1, Gaveta 4", width = "100%")
      )
    } else {
      tagList(
        fluidRow(
          column(6,
            tags$label(class = "font-weight-bold text-muted", "Nombre Completo *"),
            textInput("submit_empleado", NULL, placeholder = "Ej: Susana Pérez", width = "100%")
          ),
          column(6,
            tags$label(class = "font-weight-bold text-muted", "Cédula de Identidad *"),
            textInput("submit_cedula", NULL, placeholder = "Ej: V-12345678", width = "100%")
          )
        ),
        tags$label(class = "font-weight-bold text-muted mt-2", "Personas Relacionadas"),
        textInput("submit_personas", NULL, placeholder = "Ej: Susana Pérez; Dirección RRHH; Asesoría Legal", width = "100%"),
        fluidRow(
          column(4,
            tags$label(class = "font-weight-bold text-muted mt-2", "Clasificación *"),
            selectInput("submit_type", NULL, choices = c("Hoja de Vida", "Contrato", "Evaluación Desempeño", "Nómina"), width = "100%")
          ),
          column(4,
            tags$label(class = "font-weight-bold text-muted mt-2", "Departamento *"),
            textInput("submit_depto", NULL, placeholder = "Ej: Biología", width = "100%")
          ),
          column(4,
            tags$label(class = "font-weight-bold text-muted mt-2", "Estado *"),
            selectInput("submit_estado", NULL, choices = c("Activo", "Inactivo", "Retirado", "Pensionado"), width = "100%")
          )
        ),
        fluidRow(
          column(6,
            tags$label(class = "font-weight-bold text-muted mt-2", "Fecha de Ingreso *"),
            dateInput("submit_fecha_ingreso", NULL, value = Sys.Date(), language = "es", width = "100%")
          ),
          column(6,
            tags$label(class = "font-weight-bold text-muted mt-2", "Retención Física *"),
            textInput("submit_location", NULL, placeholder = "Archivo Pasivo - Caja J-02", width = "100%")
          )
        )
      )
    }
  })

  # --- ÚLTIMOS INGRESOS ---
  output$recent_submissions <- renderUI({
    df <- get_mod_df()
    n <- min(3, nrow(df))
    items <- lapply(1:n, function(i) {
      f <- df[i, ]
      nombre <- if (session_state$modulo == "Extensión") f$titulo else f$empleado
      tags$div(class = "d-flex align-items-center mb-2 p-2", style = "background:#f8f9fa; border-radius:8px;",
        tags$i(class = "fas fa-file-alt mr-2", style = "color:#2b4e72; font-size:1.2rem;"),
        tags$div(
          tags$strong(nome <- substr(nombre, 1, 35), style = "font-size:0.85rem;"),
          tags$br(),
          tags$span(class = "text-muted", style = "font-size:0.75rem;", f$doc_type)
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
    df <- get_mod_df()
    tipos <- extract_doc_type_set(df$doc_type)
    selectizeInput("admin_type_filter", NULL, choices = tipos, multiple = TRUE, width = "100%")
  })

  output$admin_filter_person <- renderUI({
    if (!identical(session_state$modulo, "RRHH")) return(NULL)

    people <- rrhh_people_store()
    selectizeInput("admin_person_filter", NULL, choices = people, multiple = TRUE, width = "100%", options = list(placeholder = "Filtrar por persona"))
  })

  output$admin_control_table <- renderUI({
    df <- get_mod_df()

    if (!is.null(input$admin_search) && input$admin_search != "") {
      term <- tolower(input$admin_search)
      if (session_state$modulo == "Extensión") {
        df <- df[grepl(term, tolower(df$titulo)) | grepl(term, tolower(df$autor)), ]
      } else {
        df <- df[grepl(term, tolower(df$empleado)) | grepl(term, tolower(df$cedula)), ]
      }
    }

    df <- filter_by_doc_types(df, input$admin_type_filter)
    if (identical(session_state$modulo, "RRHH")) {
      df <- filter_by_persons(df, input$admin_person_filter)
    }

    if (nrow(df) == 0) {
      return(tags$div(class = "alert alert-secondary text-center", tags$i(class = "fas fa-search"), " No se encontraron registros con estos filtros."))
    }

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
          tags$td(style = "vertical-align:middle; font-weight:600;", substr(f$titulo, 1, 40)),
          tags$td(style = "vertical-align:middle;", f$autor),
          tags$td(style = "vertical-align:middle; font-size:0.9em;", f$fecha),
          tags$td(style = "vertical-align:middle;", tags$span(class = "ds-badge", f$doc_type)),
          tags$td(style = "vertical-align:middle; font-size:0.85em; color:#6c757d;", f$ubicacion),
          tags$td(style = "vertical-align:middle;",
            actionButton(paste0("view_", real_idx), NULL, icon = icon("eye"), class = "btn btn-sm btn-outline-secondary", title = "Ver"),
            actionButton(paste0("edit_", real_idx), NULL, icon = icon("edit"), class = "btn btn-sm btn-outline-info ml-1", title = "Editar"),
            actionButton(paste0("del_", real_idx), NULL, icon = icon("trash-alt"), class = "btn btn-sm btn-outline-danger ml-1", title = "Eliminar")
          )
        )
      } else {
        color_st <- switch(f$estado, "Activo" = "#28a745", "Inactivo" = "#dc3545", "Retirado" = "#6f42c1", "Pensionado" = "#0056b3", "#6c757d")
        tags$tr(
          tags$td(style = "vertical-align:middle; font-weight:600;", f$empleado),
          tags$td(style = "vertical-align:middle;", f$cedula),
          tags$td(style = "vertical-align:middle; font-size:0.85em;", f$personas_relacionadas),
          tags$td(style = "vertical-align:middle;", f$departamento),
          tags$td(style = "vertical-align:middle;", tags$span(class = "ds-badge", style = sprintf("background-color:%s;", color_st), f$estado)),
          tags$td(style = "vertical-align:middle;", tags$span(class = "ds-badge", style = "background-color:#6c757d;", f$doc_type)),
          tags$td(style = "vertical-align:middle;",
            actionButton(paste0("view_", real_idx), NULL, icon = icon("eye"), class = "btn btn-sm btn-outline-secondary", title = "Ver"),
            actionButton(paste0("edit_", real_idx), NULL, icon = icon("edit"), class = "btn btn-sm btn-outline-info ml-1", title = "Editar"),
            actionButton(paste0("del_", real_idx), NULL, icon = icon("trash-alt"), class = "btn btn-sm btn-outline-danger ml-1", title = "Eliminar")
          )
        )
      }
    })

    encabezados <- if (session_state$modulo == "Extensión") {
      tags$tr(tags$th("Título"), tags$th("Autor"), tags$th("Fecha"), tags$th("Tipo"), tags$th("Ubicación"), tags$th("Acciones"))
    } else {
      tags$tr(tags$th("Persona titular"), tags$th("C.I."), tags$th("Personas vinculadas"), tags$th("Depto."), tags$th("Estado"), tags$th("Tipo de archivo"), tags$th("Acciones"))
    }

    tags$div(class = "table-responsive",
      tags$table(class = "table table-hover mb-0", style = "font-family:'Nunito',sans-serif; font-size:0.88rem;",
        tags$thead(class = "thead-light", encabezados),
        tags$tbody(filas)
      )
    )
  })

  output$admin_table_summary <- renderUI({
    df <- get_mod_df()
    tags$span(paste("Total:", nrow(df), "registros en el módulo", session_state$modulo))
  })

  output$admin_page_info <- renderText({
    df <- get_mod_df()
    tot <- ceiling(nrow(df) / 8)
    paste("Pág", p_admin(), "de", tot)
  })

  output$admin_people_tab <- renderUI({
    if (!identical(session_state$modulo, "RRHH")) return(NULL)

    people <- rrhh_people_store()
    rrhh_df <- rrhh_admin_data()
    selected_person <- if (length(people) > 0) people[1] else NULL

    total_files <- nrow(rrhh_df)
    total_people <- length(people)
    avg_files_per_person <- if (total_people > 0) {
      round(mean(vapply(people, function(p) {
        sum(vapply(seq_len(nrow(rrhh_df)), function(r) row_has_any_person(rrhh_df$empleado[r], rrhh_df$personas_relacionadas[r], p), logical(1)))
      }, numeric(1))), 2)
    } else {
      0
    }

    rows <- lapply(people, function(p) {
      rel_count <- sum(vapply(seq_len(nrow(rrhh_df)), function(r) row_has_any_person(rrhh_df$empleado[r], rrhh_df$personas_relacionadas[r], p), logical(1)))
      tags$tr(
        tags$td(tags$strong(p)),
        tags$td(rel_count)
      )
    })

    bs4Card(
      title = tags$span(tags$i(class = "fas fa-users"), " Personas y Archivos de Personal (RRHH)"),
      status = "secondary",
      solidHeader = FALSE,
      width = 12,
      collapsible = TRUE,
      collapsed = FALSE,
      class = "mt-3",
      fluidRow(
        column(4, bs4ValueBox(value = total_people, subtitle = "Personas registradas", icon = icon("users"), color = "primary", width = 12)),
        column(4, bs4ValueBox(value = total_files, subtitle = "Archivos RRHH", icon = icon("folder-open"), color = "info", width = 12)),
        column(4, bs4ValueBox(value = avg_files_per_person, subtitle = "Promedio archivos/persona", icon = icon("chart-line"), color = "success", width = 12))
      ),
      tags$hr(),
      fluidRow(
        column(6,
          tags$label(class = "font-weight-bold", "Registrar Persona"),
          textInput("person_new_name", NULL, placeholder = "Ej: Dirección de Personal"),
          actionButton("person_add_btn", "Agregar persona", icon = icon("user-plus"), class = "btn btn-sm btn-outline-primary")
        ),
        column(6,
          tags$label(class = "font-weight-bold", "Renombrar / Eliminar"),
          selectInput("person_edit_target", NULL, choices = people, selected = selected_person),
          textInput("person_edit_name", NULL, placeholder = "Nuevo nombre"),
          tags$div(
            actionButton("person_rename_btn", "Renombrar", icon = icon("edit"), class = "btn btn-sm btn-outline-secondary"),
            actionButton("person_delete_btn", "Eliminar", icon = icon("trash"), class = "btn btn-sm btn-outline-danger ml-1")
          )
        )
      ),
      tags$hr(),
      fluidRow(
        column(6,
          tags$label(class = "font-weight-bold", "Vincular personas a archivo"),
          selectInput("person_link_record", "Archivo (persona titular)", choices = rrhh_df$empleado),
          selectizeInput("person_link_people", "Personas vinculadas", choices = people, multiple = TRUE, options = list(plugins = list("remove_button"))),
          actionButton("person_link_save_btn", "Guardar vinculación", icon = icon("link"), class = "btn btn-sm btn-outline-success")
        ),
        column(6,
          tags$label(class = "font-weight-bold", "Perfil de persona"),
          selectInput("person_profile_target", "Persona", choices = people, selected = selected_person),
          uiOutput("admin_person_profile")
        )
      ),
      tags$hr(),
      fluidRow(
        column(6,
          tags$label(class = "font-weight-bold", "Ver archivos de la persona"),
          selectInput("person_files_target", "Persona", choices = people),
          uiOutput("admin_person_linked_files")
        ),
        column(6, tags$div())
      ),
      tags$hr(),
      fluidRow(
        column(12,
          tags$table(class = "table table-sm table-hover mb-0",
            tags$thead(class = "thead-light", tags$tr(tags$th("Persona"), tags$th("Archivos asociados"))),
            tags$tbody(rows)
          )
        )
      )
    )
  })

  output$admin_person_profile <- renderUI({
    req(identical(session_state$modulo, "RRHH"))
    rrhh_df <- rrhh_admin_data()
    person <- input$person_profile_target

    if (is.null(person) || !nzchar(person)) {
      return(tags$div(class = "alert alert-secondary", "Selecciona una persona para ver información."))
    }

    keep <- vapply(seq_len(nrow(rrhh_df)), function(i) row_has_any_person(rrhh_df$empleado[i], rrhh_df$personas_relacionadas[i], person), logical(1))
    rows <- rrhh_df[keep, , drop = FALSE]

    if (nrow(rows) == 0) {
      return(tags$div(class = "alert alert-secondary", "Esta persona no tiene archivos vinculados actualmente."))
    }

    depto_top <- names(sort(table(rows$departamento), decreasing = TRUE))[1]
    estado_top <- names(sort(table(rows$estado), decreasing = TRUE))[1]
    tipos <- paste(sort(unique(rows$doc_type)), collapse = ", ")

    tags$div(class = "p-2", style = "background:#f8f9fa; border:1px solid #e3e7ec; border-radius:8px;",
      tags$p(tags$strong("Persona:"), " ", person, style = "margin-bottom:0.35rem;"),
      tags$p(tags$strong("Total de archivos:"), " ", nrow(rows), style = "margin-bottom:0.35rem;"),
      tags$p(tags$strong("Departamento predominante:"), " ", depto_top, style = "margin-bottom:0.35rem;"),
      tags$p(tags$strong("Estado predominante:"), " ", estado_top, style = "margin-bottom:0.35rem;"),
      tags$p(tags$strong("Tipos de archivo:"), " ", tipos, style = "margin-bottom:0;")
    )
  })

  output$admin_person_linked_files <- renderUI({
    req(identical(session_state$modulo, "RRHH"))

    rrhh_df <- rrhh_admin_data()
    person <- input$person_files_target
    req(!is.null(person), nzchar(person))

    keep <- vapply(seq_len(nrow(rrhh_df)), function(i) row_has_any_person(rrhh_df$empleado[i], rrhh_df$personas_relacionadas[i], person), logical(1))
    rows <- rrhh_df[keep, , drop = FALSE]

    if (nrow(rows) == 0) {
      return(tags$div(class = "alert alert-secondary", "No hay archivos asociados a esta persona."))
    }

    body <- lapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      tags$tr(
        tags$td(r$empleado),
        tags$td(r$cedula),
        tags$td(r$doc_type),
        tags$td(r$departamento),
        tags$td(r$estado)
      )
    })

    tags$table(class = "table table-sm table-striped mb-0",
      tags$thead(class = "thead-light", tags$tr(tags$th("Archivo"), tags$th("C.I."), tags$th("Tipo"), tags$th("Depto."), tags$th("Estado"))),
      tags$tbody(body)
    )
  })

  observeEvent(input$person_add_btn, {
    req(identical(session_state$modulo, "RRHH"))
    new_name <- trimws(input$person_new_name)
    if (!nzchar(new_name)) return()

    people <- sort(unique(c(rrhh_people_store(), new_name)))
    rrhh_people_store(people)
    updateSelectInput(session, "person_edit_target", choices = people)
    updateSelectizeInput(session, "person_link_people", choices = people, selected = split_person_terms(input$person_link_people))
    showNotification("Persona agregada al catálogo de RRHH.", type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$person_rename_btn, {
    req(identical(session_state$modulo, "RRHH"))
    old_name <- input$person_edit_target
    new_name <- trimws(input$person_edit_name)
    if (!nzchar(old_name) || !nzchar(new_name)) return()

    rrhh_df <- rrhh_admin_data()
    rrhh_df$personas_relacionadas <- vapply(rrhh_df$personas_relacionadas, function(v) {
      vals <- split_person_terms(v)
      vals[vals == old_name] <- new_name
      paste(unique(vals), collapse = ";")
    }, character(1))
    rrhh_admin_data(rrhh_df)

    people <- extract_people_choices(rrhh_df)
    rrhh_people_store(people)
    updateSelectInput(session, "person_edit_target", choices = people, selected = new_name)
    updateSelectizeInput(session, "person_link_people", choices = people)
    showNotification("Persona renombrada y vínculos actualizados.", type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$person_delete_btn, {
    req(identical(session_state$modulo, "RRHH"))
    target <- input$person_edit_target
    if (!nzchar(target)) return()

    rrhh_df <- rrhh_admin_data()
    rrhh_df$personas_relacionadas <- vapply(rrhh_df$personas_relacionadas, function(v) {
      vals <- split_person_terms(v)
      vals <- vals[vals != target]
      paste(unique(vals), collapse = ";")
    }, character(1))
    rrhh_admin_data(rrhh_df)

    people <- extract_people_choices(rrhh_df)
    rrhh_people_store(people)
    updateSelectInput(session, "person_edit_target", choices = people)
    updateSelectizeInput(session, "person_link_people", choices = people)
    showNotification("Persona eliminada del catálogo y desvinculada de archivos.", type = "warning")
  }, ignoreInit = TRUE)

  observeEvent(input$person_link_record, {
    req(identical(session_state$modulo, "RRHH"))
    rrhh_df <- rrhh_admin_data()
    rec <- input$person_link_record
    if (!nzchar(rec)) return()

    idx <- match(rec, rrhh_df$empleado)
    if (is.na(idx)) return()
    current_people <- split_person_terms(rrhh_df$personas_relacionadas[idx])
    updateSelectizeInput(session, "person_link_people", selected = current_people)
  }, ignoreInit = TRUE)

  observeEvent(input$person_link_save_btn, {
    req(identical(session_state$modulo, "RRHH"))
    rrhh_df <- rrhh_admin_data()
    rec <- input$person_link_record
    if (!nzchar(rec)) return()

    idx <- match(rec, rrhh_df$empleado)
    if (is.na(idx)) return()

    selected <- split_person_terms(input$person_link_people)
    rrhh_df$personas_relacionadas[idx] <- paste(selected, collapse = ";")
    rrhh_admin_data(rrhh_df)

    rrhh_people_store(extract_people_choices(rrhh_df))
    showNotification("Personas vinculadas al archivo RRHH.", type = "message")
  }, ignoreInit = TRUE)

  # --- TAXONOMÍAS ---
  output$admin_tax_list <- renderUI({
    df <- get_mod_df()
    cats <- unique(df$doc_type)
    conteos <- table(df$doc_type)

    elementos <- lapply(cats, function(cat) {
      n <- conteos[[cat]]
      tags$div(class = "d-flex justify-content-between align-items-center p-3 mb-2", style = "background:#f8f9fa; border-radius:10px; border-left:4px solid #2b4e72;",
        tags$div(
          tags$strong(cat, style = "font-size:0.95rem;"),
          tags$br(),
          tags$span(class = "text-muted", style = "font-size:0.8rem;", paste(n, "documentos asignados"))
        ),
        tags$div(
          tags$span(class = "badge badge-pill", style = "background-color:#2b4e72; color:white; padding:6px 12px; font-size:0.85rem;", n),
          actionButton(paste0("tax_edit_", which(cats == cat)), NULL, icon = icon("edit"), class = "btn btn-sm btn-outline-secondary ml-2")
        )
      )
    })

    tagList(
      tags$div(class = "d-flex justify-content-between align-items-center mb-3",
        tags$span(class = "text-muted", style = "font-size:0.85rem;", paste(length(cats), "categorías registradas")),
        tags$span(class = "badge badge-success", "Todas Activas")
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
        tags$td(style = "vertical-align:middle;", tags$i(class = "fas fa-user-circle mr-2", style = "color:#2b4e72;"), tags$strong(u$usuario)),
        tags$td(style = "vertical-align:middle;", u$modulo),
        tags$td(style = "vertical-align:middle;", tags$span(class = paste0("badge badge-", color_rol), u$rol)),
        tags$td(style = "vertical-align:middle;", "••••••••"),
        tags$td(style = "vertical-align:middle;",
          actionButton(paste0("user_reset_", i), NULL, icon = icon("key"), class = "btn btn-sm btn-outline-warning", title = "Reset Password"),
          actionButton(paste0("user_del_", i), NULL, icon = icon("user-times"), class = "btn btn-sm btn-outline-danger ml-1", title = "Revocar")
        )
      )
    })

    tags$table(class = "table table-hover mb-0", style = "font-family:'Nunito',sans-serif; font-size:0.88rem;",
      tags$thead(class = "thead-light", tags$tr(tags$th("Usuario"), tags$th("Módulo"), tags$th("Rol"), tags$th("Pass"), tags$th("Acciones"))),
      tags$tbody(filas)
    )
  })
}

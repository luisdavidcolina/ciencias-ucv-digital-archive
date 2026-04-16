register_document_modal_handlers <- function(input, output, session, session_state, dat_ext_react, dat_rrhh_react) {
  rrhh_modal_context <- reactiveVal(NULL)

  person_initials <- function(person_name) {
    parts <- strsplit(trimws(as.character(person_name)), "\\s+")[[1]]
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0) {
      return("RR")
    }

    paste0(toupper(substr(parts, 1, 1)[seq_len(min(length(parts), 2))]), collapse = "")
  }

  build_photo_panel <- function(profile) {
    if (!is.null(profile$foto_url) && nzchar(profile$foto_url)) {
      return(tags$div(
        class = "rrhh-person-photo-card",
        tags$img(src = profile$foto_url, class = "rrhh-person-photo", alt = profile$persona)
      ))
    }

    tags$div(
      class = "rrhh-person-photo-card rrhh-person-photo-fallback",
      tags$span(class = "rrhh-person-photo-initials", person_initials(profile$persona)),
      tags$i(class = "fas fa-user rrhh-person-photo-icon")
    )
  }

  build_file_card <- function(file_row) {
    file_idx <- if (".__idx" %in% names(file_row)) file_row$.__idx[1] else NA_integer_
    file_status <- if ("estatus" %in% names(file_row)) file_row$estatus else if ("estado" %in% names(file_row)) file_row$estado else "Sin estatus"

    tags$div(
      class = "rrhh-person-file-item",
      tags$div(
        class = "rrhh-person-file-head",
        tags$div(
          class = "rrhh-person-file-main",
          tags$strong(file_row$doc_type),
          tags$span(class = "rrhh-person-file-sub", paste("Fecha de ingreso:", if ("fecha_ingreso" %in% names(file_row)) file_row$fecha_ingreso else "Sin fecha"))
        ),
        tags$a(
          href = "#",
          class = "btn btn-sm btn-outline-info",
          onclick = sprintf(
            "Shiny.setInputValue('open_doc', {mod: 'rrhh_file', idx: %s, nonce: Date.now()}, {priority: 'event'}); return false;",
            file_idx
          ),
          "Abrir archivo"
        )
      ),
      tags$div(
        class = "rrhh-person-file-meta",
        tags$span(paste("Dependencia o AP:", if ("departamento" %in% names(file_row)) file_row$departamento else "Sin dependencia o AP")),
        tags$span(paste("Estatus:", file_status)),
        tags$span(paste("Ubicación:", if ("ubicacion" %in% names(file_row)) file_row$ubicacion else "Sin ubicación"))
      ),
      tags$div(class = "ds-item-tags", render_tesauro_badges(file_row))
    )
  }

  build_category_panel <- function(category_name, category_rows) {
    if (nrow(category_rows) == 0) {
      return(tags$div(class = "alert alert-secondary", "No hay archivos en esta categoría con los filtros actuales."))
    }

    tagList(lapply(seq_len(nrow(category_rows)), function(i) build_file_card(category_rows[i, , drop = FALSE])))
  }

  show_doc_modal <- function(doc, mod) {
    is_admin <- identical(session_state$rol, "Admin")

    if (identical(mod, "ext")) {
      titulo <- doc$titulo
      resumen <- if (!is.null(doc$abstract) && nzchar(doc$abstract)) doc$abstract else "Sin resumen disponible."
      thumb_icon <- "fas fa-file-alt"
      thumb_badge <- get_doc_primary_term(doc)
      tesauro <- paste(get_doc_tesauro_terms(doc), collapse = "; ")
      meta <- tagList(
        div(class = "ds-doc-meta-row", tags$span(class = "k", "Responsable"), tags$span(class = "v", doc$autor)),
        div(class = "ds-doc-meta-row", tags$span(class = "k", "Tipo"), tags$span(class = "v", doc$doc_type)),
        div(class = "ds-doc-meta-row", tags$span(class = "k", "Tesauro"), tags$span(class = "v", if (nzchar(tesauro)) tesauro else "Sin tesauro")),
        div(class = "ds-doc-meta-row", tags$span(class = "k", "Fecha de emisión"), tags$span(class = "v", doc$fecha)),
        div(class = "ds-doc-meta-row", tags$span(class = "k", "Ubicación física"), tags$span(class = "v", doc$ubicacion))
      )
    } else {
      titulo <- paste("Archivo de:", doc$empleado)
      estado_value <- if ("estatus" %in% names(doc)) doc$estatus else if ("estado" %in% names(doc)) doc$estado else "Sin estatus"
      resumen <- paste("Documento de RRHH en estado", estado_value, "en", doc$departamento)
      thumb_icon <- "fas fa-user-lock"
      thumb_badge <- get_doc_primary_term(doc)
      tesauro <- paste(get_doc_tesauro_terms(doc), collapse = "; ")
      meta <- tagList(
        div(class = "ds-doc-meta-row", tags$span(class = "k", "Persona titular"), tags$span(class = "v", doc$empleado)),
        div(class = "ds-doc-meta-row", tags$span(class = "k", "Cédula"), tags$span(class = "v", doc$cedula)),
        div(class = "ds-doc-meta-row", tags$span(class = "k", "Personas vinculadas"), tags$span(class = "v", doc$personas_relacionadas)),
        div(class = "ds-doc-meta-row", tags$span(class = "k", "Tipo"), tags$span(class = "v", doc$doc_type)),
        div(class = "ds-doc-meta-row", tags$span(class = "k", "Tesauro"), tags$span(class = "v", if (nzchar(tesauro)) tesauro else "Sin tesauro")),
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

  output$rrhh_person_modal_body <- renderUI({
    context <- rrhh_modal_context()
    req(!is.null(context))

    filtered_rows <- filter_rrhh_person_files(
      context$rows,
      input$rrhh_modal_search,
      input$rrhh_modal_doc_type,
      input$rrhh_modal_sort
    )

    doc_type_choices <- sort(unique(context$rows$doc_type))
    total_files <- nrow(context$rows)
    visible_files <- nrow(filtered_rows)
    profile <- context

    ingresos_text <- if (nzchar(profile$fecha_ingreso)) profile$fecha_ingreso else "No registrada"
    jubilacion_text <- if (grepl("Jubilado", profile$statuses, fixed = TRUE)) {
      if (nzchar(profile$fecha_jubilacion)) profile$fecha_jubilacion else "No registrada"
    } else {
      "No aplica"
    }
    pension_text <- if (grepl("Pensionado", profile$statuses, fixed = TRUE)) {
      if (nzchar(profile$fecha_pension)) profile$fecha_pension else "No registrada"
    } else {
      "No aplica"
    }

    if (visible_files == 0) {
      files_body <- tags$div(class = "alert alert-secondary", "No se encontraron archivos con estos filtros.")
    } else {
      categories <- sort(unique(filtered_rows$doc_type))
      categories <- categories[nzchar(categories)]

      tab_panels <- lapply(categories, function(category_name) {
        category_rows <- filtered_rows[filtered_rows$doc_type == category_name, , drop = FALSE]
        tabPanel(
          category_name,
          tags$div(class = "rrhh-person-category-panel", build_category_panel(category_name, category_rows))
        )
      })

      files_body <- if (length(tab_panels) == 0) {
        tags$div(class = "alert alert-secondary", "No hay categorías disponibles con estos filtros.")
      } else {
        do.call(tabsetPanel, c(list(id = "rrhh_person_category_tabs", type = "pills", selected = categories[1]), tab_panels))
      }
    }

    tags$div(
      class = "rrhh-person-modal-wrap",
      style = "max-height: 75vh; overflow-y: auto;",
      div(class = "ds-doc-modal-head", tags$i(class = "fas fa-folder-open"), tags$h4(paste("Expediente de:", profile$persona))),
      fluidRow(
        column(
          4,
          bs4Card(
            title = "Persona", status = "primary", solidHeader = FALSE, width = 12,
            build_photo_panel(profile),
            tags$div(class = "rrhh-person-summary", style = "margin-top:0.9rem;",
              tags$p(tags$strong("Expedientes:"), " ", total_files, style = "margin-bottom:0.35rem;"),
              tags$p(tags$strong("Cédula:"), " ", if (nzchar(profile$cedulas)) profile$cedulas else "Sin cédula", style = "margin-bottom:0.35rem;"),
              tags$p(tags$strong("Fecha ingreso:"), " ", ingresos_text, style = "margin-bottom:0.35rem;"),
              tags$p(tags$strong("Fecha jubilación:"), " ", jubilacion_text, style = "margin-bottom:0.35rem;"),
              tags$p(tags$strong("Fecha pensión:"), " ", pension_text, style = "margin-bottom:0.35rem;"),
              tags$p(tags$strong("Dependencia o AP:"), " ", if (nzchar(profile$departamentos)) profile$departamentos else "Sin dependencia o AP", style = "margin-bottom:0.35rem;"),
              tags$p(tags$strong("Estatus(es):"), " ", if (nzchar(profile$statuses)) profile$statuses else "Sin estatus", style = "margin-bottom:0.35rem;"),
              tags$p(tags$strong("Tipología(s):"), " ", if (nzchar(profile$tipos)) profile$tipos else "Sin tipología", style = "margin-bottom:0;")
            )
          )
        ),
        column(
          8,
          bs4Card(
            title = "Filtros del expediente", status = "secondary", solidHeader = FALSE, width = 12,
            fluidRow(
              column(
                7,
                textInput(
                  "rrhh_modal_search",
                  "Buscar dentro del expediente",
                  placeholder = "Tipo, dependencia o AP, ubicación o persona relacionada",
                  width = "100%"
                )
              ),
              column(
                5,
                selectizeInput(
                  "rrhh_modal_doc_type",
                  "Tipos",
                  choices = doc_type_choices,
                  multiple = TRUE,
                  width = "100%",
                  options = list(plugins = list("remove_button"), dropdownParent = "body")
                )
              )
            ),
            selectInput(
              "rrhh_modal_sort",
              "Orden",
              choices = c("Lo más relevante", "Título A-Z"),
              width = "100%"
            ),
            tags$small(class = "text-muted", paste("Navega entre categorías disponibles.", visible_files, "archivo(s) visibles con los filtros actuales."))
          )
        )
      ),
      tags$hr(),
      tags$div(class = "rrhh-person-files", files_body)
    )
  })

  observeEvent(input$open_doc, {
    payload <- input$open_doc
    req(!is.null(payload$mod))

    if (identical(payload$mod, "ext")) {
      req(!is.null(payload$idx))
      idx <- as.integer(payload$idx)
      if (is.na(idx) || idx < 1) return()
      datos <- dat_ext_react()
      if (idx <= nrow(datos)) show_doc_modal(datos[idx, ], "ext")
    } else if (identical(payload$mod, "rrhh_file") || identical(payload$mod, "rrhh")) {
      req(!is.null(payload$idx))
      idx <- as.integer(payload$idx)
      if (is.na(idx) || idx < 1) return()
      datos <- dat_rrhh_react()
      if (idx <= nrow(datos)) {
        removeModal()
        show_doc_modal(datos[idx, ], "rrhh")
      }
    } else if (identical(payload$mod, "rrhh_person")) {
      req(!is.null(payload$person))
      persona <- as.character(payload$person)
      if (!nzchar(persona)) return()

      context <- build_rrhh_person_profile(dat_rrhh_react(), persona)
      req(!is.null(context))
      rrhh_modal_context(context)

      updateTextInput(session, "rrhh_modal_search", value = "")
      updateSelectizeInput(session, "rrhh_modal_doc_type", selected = character(0))
      updateSelectInput(session, "rrhh_modal_sort", selected = "Lo más relevante")

      removeModal()
      showModal(modalDialog(
        title = NULL,
        size = "l",
        easyClose = TRUE,
        class = "ds-doc-modal rrhh-person-modal",
        footer = modalButton("Cerrar"),
        uiOutput("rrhh_person_modal_body")
      ))
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
}

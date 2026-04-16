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
    jubilacion_text <- if (grepl("Retirado", profile$statuses, fixed = TRUE)) {
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
      files_body <- tags$div(class = "alert alert-secondary mt-3", "No se encontraron archivos con estos filtros en el expediente.")
    } else {
      categories <- sort(unique(filtered_rows$doc_type))
      categories <- categories[nzchar(categories)]

      category_blocks <- lapply(categories, function(category_name) {
        category_rows <- filtered_rows[filtered_rows$doc_type == category_name, , drop = FALSE]
        tags$div(
          class = "rrhh-person-category-section mb-4",
          tags$h5(class = "border-bottom pb-2 mb-3 ds-category-title", tags$i(class = "fas fa-folder-open mr-2"), category_name),
          build_category_panel(category_name, category_rows)
        )
      })

      files_body <- if (length(category_blocks) == 0) {
        tags$div(class = "alert alert-secondary mt-3", "No hay categorías disponibles con estos filtros.")
      } else {
        tags$div(class = "rrhh-category-list mt-2", category_blocks)
      }
    }

    tags$div(
      class = "rrhh-person-modal-wrap",
      style = "max-height: 75vh; overflow-y: auto; overflow-x: hidden;",
      
      tags$div(
        class = "ds-person-profile-header mb-4 p-3 bg-white rounded shadow-sm border",
        fluidRow(
          column(12, class="d-flex flex-column flex-md-row align-items-center align-items-md-start",
            div(class = "ds-person-avatar-wrap mb-3 mb-md-0 mr-md-4", style="width: 150px; min-width: 150px;", build_photo_panel(profile)),
            div(class = "ds-person-info flex-grow-1 w-100",
              tags$div(class = "d-flex justify-content-between align-items-center border-bottom pb-2 mb-3",
                 tags$h3(class = "ds-person-name m-0 text-primary font-weight-bold", profile$persona),
                 tags$span(class = "badge badge-info text-uppercase px-3 py-2", if(nzchar(profile$statuses)) profile$statuses else "Sin estado")
              ),
              tags$h5(class = "ds-person-cargo text-secondary mb-3 font-weight-bold", tags$i(class = "fas fa-user-tie mr-2"), if(nzchar(profile$cargos)) profile$cargos else "Cargo no especificado"),
              fluidRow(
                column(6, class="mb-2", tags$strong("C.I.:"), " ", if(nzchar(profile$cedulas)) profile$cedulas else "N/A", 
                  if(nzchar(profile$cedulas)) tags$a(href="#", class="btn btn-xs btn-outline-primary ml-2 py-0 px-2", onclick="alert('Abriendo visor Cédula de Identidad...');return false;", tags$i(class="fas fa-id-card"), " Ver") else NULL),
                column(6, class="mb-2", tags$strong("RIF:"), " ", if(nzchar(profile$rifs)) profile$rifs else "N/A"),
                column(6, class="mb-2", tags$strong("Adscripción:"), " ", if(nzchar(profile$departamentos)) profile$departamentos else "N/A"),
                column(6, class="mb-2", tags$strong("Ingreso:"), " ", ingresos_text),
                if (grepl("Retirado", profile$statuses, fixed = TRUE)) column(6, class="mb-2", tags$strong("Jubilación:"), " ", jubilacion_text) else NULL,
                if (grepl("Pensionado", profile$statuses, fixed = TRUE)) column(6, class="mb-2", tags$strong("Pensión:"), " ", pension_text) else NULL
              )
            )
          )
        )
      ),

      tags$div(
        class = "ds-modal-filters-wrap bg-light p-3 rounded border mb-4",
        fluidRow(
          column(12, tags$h6(class="font-weight-bold text-secondary text-uppercase shadow-sm-text mb-3", tags$i(class="fas fa-sliders-h mr-2"), "Explorar Documentos")),
          column(12, md = 5, class = "mb-2 mb-md-0",
            textInput("rrhh_modal_search", NULL, placeholder="🔍 Buscar palabras, ubicaciones, fechas...", width="100%")
          ),
          column(12, md = 4, class = "mb-2 mb-md-0",
            selectizeInput("rrhh_modal_doc_type", NULL, choices = doc_type_choices, multiple = TRUE, width = "100%", options = list(placeholder="📂 Elegir categorías...", plugins = list("remove_button"), dropdownParent = "body"))
          ),
          column(12, md = 3, class = "mb-2 mb-md-0",
            selectInput("rrhh_modal_sort", NULL, choices = c("Alfabético (A-Z)", "Alfabético (Z-A)", "Más recientes primero", "Más antiguos primero"), selected = "Alfabético (A-Z)", width = "100%")
          )
        ),
        fluidRow(
          column(12, tags$div(class="text-right mt-1", tags$span(class="badge badge-pill badge-primary px-3 py-1", paste(visible_files, "folios visibles"))))
        )
      ),
      
      tags$div(class = "rrhh-person-files px-1", files_body)
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
      updateSelectInput(session, "rrhh_modal_sort", selected = "Alfabético (A-Z)")

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

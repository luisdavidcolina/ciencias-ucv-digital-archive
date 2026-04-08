register_document_modal_handlers <- function(input, output, session, session_state, dat_ext_react, dat_rrhh_react) {
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
      titulo <- paste("Expediente:", doc$empleado)
      resumen <- paste("Documento de RRHH en estado", doc$estatus, "adscrito a", doc$departamento)
      thumb_icon <- "fas fa-user-lock"
      thumb_badge <- get_doc_primary_term(doc)
      tesauro <- paste(get_doc_tesauro_terms(doc), collapse = "; ")
      meta <- tagList(
        div(class = "ds-doc-meta-row", tags$span(class = "k", "Empleado"), tags$span(class = "v", doc$empleado)),
        div(class = "ds-doc-meta-row", tags$span(class = "k", "Cédula"), tags$span(class = "v", doc$cedula)),
        div(class = "ds-doc-meta-row", tags$span(class = "k", "Personas asociadas"), tags$span(class = "v", doc$personas_relacionadas)),
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
}

split_doc_types <- function(x) {
  if (length(x) == 0) return(character(0))

  vals <- trimws(unlist(strsplit(as.character(x), ";", fixed = TRUE), use.names = FALSE))
  vals <- vals[nzchar(vals)]
  unique(vals)
}

extract_doc_type_set <- function(x) {
  sort(split_doc_types(x))
}

row_has_any_doc_type <- function(row_value, selected_types) {
  if (is.null(selected_types) || length(selected_types) == 0) return(TRUE)

  row_types <- split_doc_types(row_value)
  if (length(row_types) == 0) return(FALSE)

  any(row_types %in% selected_types)
}

filter_by_doc_types <- function(datos, selected_types) {
  if (is.null(selected_types) || length(selected_types) == 0) {
    return(datos)
  }

  selected_types <- selected_types[nzchar(selected_types)]
  if (length(selected_types) == 0) {
    return(datos)
  }

  keep <- vapply(datos$doc_type, row_has_any_doc_type, logical(1), selected_types = selected_types)
  datos[keep, , drop = FALSE]
}

filter_extension_data <- function(datos, search_term, doc_types, sort_mode) {
  out <- datos

  if (!is.null(search_term) && nzchar(search_term)) {
    term <- tolower(search_term)
    out <- out[grepl(term, tolower(out$titulo)) | grepl(term, tolower(out$autor)), , drop = FALSE]
  }

  out <- filter_by_doc_types(out, doc_types)

  if (!is.null(sort_mode)) {
    if (sort_mode %in% c("Titulo A-Z", "Título A-Z")) {
      out <- out[order(out$titulo), , drop = FALSE]
    } else if (sort_mode %in% c("Fecha de Emision (Asc)", "Fecha de Emisión (Asc)")) {
      out <- out[order(out$fecha), , drop = FALSE]
    } else if (sort_mode %in% c("Fecha de Emision (Desc)", "Fecha de Emisión (Desc)")) {
      out <- out[order(out$fecha, decreasing = TRUE), , drop = FALSE]
    }
  }

  out
}

filter_rrhh_data <- function(datos, search_term, sort_mode = NULL) {
  out <- datos

  if (!is.null(search_term) && nzchar(search_term)) {
    query <- tolower(trimws(search_term))
    tokens <- strsplit(query, "\\s+")[[1]]
    tokens <- tokens[nzchar(tokens)]

    search_cols <- c(
      "empleado", "cedula", "departamento", "doc_type", "estado",
      "personas_relacionadas", "tesauro_primario", "tesauro_secundario", "descriptores_libres"
    )
    search_cols <- search_cols[search_cols %in% names(out)]

    haystack <- apply(out[, search_cols, drop = FALSE], 1, function(row) {
      tolower(paste(row, collapse = " "))
    })

    keep <- vapply(haystack, function(text) {
      all(vapply(tokens, function(tok) grepl(tok, text, fixed = TRUE), logical(1)))
    }, logical(1))

    out <- out[keep, , drop = FALSE]
  }

  if (!is.null(sort_mode) && sort_mode %in% c("Título A-Z", "Lo más relevante")) {
    if (identical(sort_mode, "Título A-Z")) {
      if ("empleado" %in% names(out)) {
        out <- out[order(out$empleado), , drop = FALSE]
      }
    }
  }

  out
}

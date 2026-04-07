filter_extension_data <- function(datos, search_term, doc_types, sort_mode) {
  out <- datos

  if (!is.null(search_term) && nzchar(search_term)) {
    term <- tolower(search_term)
    out <- out[grepl(term, tolower(out$titulo)) | grepl(term, tolower(out$autor)), , drop = FALSE]
  }

  if (!is.null(doc_types) && length(doc_types) > 0) {
    out <- out[out$doc_type %in% doc_types, , drop = FALSE]
  }

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

filter_rrhh_data <- function(datos, search_term) {
  out <- datos

  if (!is.null(search_term) && nzchar(search_term)) {
    term <- tolower(search_term)
    out <- out[grepl(term, tolower(out$empleado)) | grepl(term, tolower(out$cedula)), , drop = FALSE]
  }

  out
}

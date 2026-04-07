split_tesauro_terms <- function(x) {
  if (length(x) == 0) return(character(0))

  vals <- trimws(unlist(strsplit(as.character(x), ";", fixed = TRUE), use.names = FALSE))
  vals <- vals[nzchar(vals)]
  unique(vals)
}

ensure_tesauro_columns <- function(df) {
  required <- c("tesauro_primario", "tesauro_secundario", "descriptores_libres")

  for (col in required) {
    if (!col %in% names(df)) {
      df[[col]] <- ""
    }
  }

  # Backfill minimo para no dejar registros vacios en la primera migracion.
  if ("doc_type" %in% names(df)) {
    empty_primary <- !nzchar(trimws(as.character(df$tesauro_primario)))
    df$tesauro_primario[empty_primary] <- as.character(df$doc_type[empty_primary])
  }

  df
}

extract_tesauro_choices <- function(df) {
  cols <- c("doc_type", "tesauro_primario", "tesauro_secundario", "descriptores_libres")
  cols <- cols[cols %in% names(df)]
  if (length(cols) == 0) return(character(0))

  terms <- unlist(lapply(cols, function(col) split_tesauro_terms(df[[col]])), use.names = FALSE)
  sort(unique(terms[nzchar(terms)]))
}

row_has_any_tesauro <- function(row, selected_terms) {
  if (is.null(selected_terms) || length(selected_terms) == 0) return(TRUE)

  selected_terms <- selected_terms[nzchar(selected_terms)]
  if (length(selected_terms) == 0) return(TRUE)

  vals <- c(row$doc_type, row$tesauro_primario, row$tesauro_secundario, row$descriptores_libres)
  row_terms <- split_tesauro_terms(vals)
  if (length(row_terms) == 0) return(FALSE)

  any(row_terms %in% selected_terms)
}

filter_by_tesauro <- function(datos, selected_terms) {
  if (is.null(selected_terms) || length(selected_terms) == 0) {
    return(datos)
  }

  selected_terms <- selected_terms[nzchar(selected_terms)]
  if (length(selected_terms) == 0) {
    return(datos)
  }

  keep <- vapply(seq_len(nrow(datos)), function(i) row_has_any_tesauro(datos[i, , drop = FALSE], selected_terms), logical(1))
  datos[keep, , drop = FALSE]
}

get_doc_tesauro_terms <- function(doc) {
  vals <- c(doc$doc_type, doc$tesauro_primario, doc$tesauro_secundario, doc$descriptores_libres)
  split_tesauro_terms(vals)
}

get_doc_primary_term <- function(doc) {
  if (!is.null(doc$tesauro_primario) && nzchar(trimws(as.character(doc$tesauro_primario)))) {
    return(as.character(doc$tesauro_primario))
  }

  terms <- get_doc_tesauro_terms(doc)
  if (length(terms) > 0) terms[1] else "Sin tesauro"
}

render_tesauro_badges <- function(doc, max_badges = 4) {
  terms <- get_doc_tesauro_terms(doc)
  if (length(terms) == 0) {
    return(tags$span(class = "ds-badge", "Sin tesauro"))
  }

  shown <- terms[seq_len(min(length(terms), max_badges))]
  badges <- lapply(shown, function(term) tags$span(class = "ds-badge", term))

  if (length(terms) > max_badges) {
    badges[[length(badges) + 1]] <- tags$span(class = "ds-badge", paste0("+", length(terms) - max_badges))
  }

  do.call(tagList, badges)
}

split_person_terms <- function(x) {
  if (length(x) == 0) return(character(0))

  vals <- trimws(unlist(strsplit(as.character(x), ";", fixed = TRUE), use.names = FALSE))
  vals <- vals[nzchar(vals)]
  unique(vals)
}

ensure_person_columns <- function(df) {
  if (!"personas_relacionadas" %in% names(df)) {
    df$personas_relacionadas <- ""
  }

  optional_columns <- c("foto_url", "fecha_jubilacion", "fecha_pension")
  for (col in optional_columns) {
    if (!col %in% names(df)) {
      df[[col]] <- ""
    }
  }

  if ("empleado" %in% names(df)) {
    empty_rel <- !nzchar(trimws(as.character(df$personas_relacionadas)))
    df$personas_relacionadas[empty_rel] <- as.character(df$empleado[empty_rel])
  }

  df
}

extract_people_choices <- function(df) {
  terms <- character(0)

  if ("empleado" %in% names(df)) {
    terms <- c(terms, as.character(df$empleado))
  }

  if ("personas_relacionadas" %in% names(df)) {
    terms <- c(terms, unlist(lapply(df$personas_relacionadas, split_person_terms), use.names = FALSE))
  }

  terms <- trimws(terms)
  sort(unique(terms[nzchar(terms)]))
}

row_has_any_person <- function(primary_person, related_people, selected_people) {
  if (is.null(selected_people) || length(selected_people) == 0) return(TRUE)

  selected_people <- selected_people[nzchar(selected_people)]
  if (length(selected_people) == 0) return(TRUE)

  row_people <- unique(c(as.character(primary_person), split_person_terms(related_people)))
  row_people <- row_people[nzchar(trimws(row_people))]
  if (length(row_people) == 0) return(FALSE)

  any(row_people %in% selected_people)
}

filter_by_persons <- function(datos, selected_people) {
  if (is.null(selected_people) || length(selected_people) == 0) {
    return(datos)
  }

  selected_people <- selected_people[nzchar(selected_people)]
  if (length(selected_people) == 0) {
    return(datos)
  }

  keep <- vapply(
    seq_len(nrow(datos)),
    function(i) row_has_any_person(datos$empleado[i], datos$personas_relacionadas[i], selected_people),
    logical(1)
  )

  datos[keep, , drop = FALSE]
}

collapse_unique_terms <- function(values) {
  values <- trimws(as.character(values))
  values <- values[nzchar(values)]
  if (length(values) == 0) {
    return("")
  }

  paste(sort(unique(values)), collapse = "; ")
}

get_rrhh_status_column <- function(datos) {
  if ("estatus" %in% names(datos)) {
    return("estatus")
  }

  if ("estado" %in% names(datos)) {
    return("estado")
  }

  NULL
}

build_rrhh_person_index <- function(datos) {
  if (is.null(datos) || nrow(datos) == 0) {
    return(data.frame(
      persona = character(0),
      doc_count = integer(0),
      primary_count = integer(0),
      cedulas = character(0),
      departamentos = character(0),
      estatuses = character(0),
      tipos = character(0),
      row_indices = I(list()),
      stringsAsFactors = FALSE
    ))
  }

  employees <- trimws(as.character(datos$empleado))
  related_people <- unlist(lapply(datos$personas_relacionadas, split_person_terms), use.names = FALSE)
  persons <- sort(unique(c(employees[nzchar(employees)], related_people[nzchar(related_people)])))

  status_column <- get_rrhh_status_column(datos)
  row_indices <- lapply(persons, function(person_name) {
    which(vapply(
      seq_len(nrow(datos)),
      function(row_index) row_has_any_person(datos$empleado[row_index], datos$personas_relacionadas[row_index], person_name),
      logical(1)
    ))
  })

  index_df <- data.frame(
    persona = persons,
    doc_count = vapply(row_indices, length, integer(1)),
    primary_count = vapply(persons, function(person_name) sum(employees == person_name, na.rm = TRUE), integer(1)),
    cedulas = vapply(persons, function(person_name) {
      primary_rows <- employees == person_name
      collapse_unique_terms(if ("cedula" %in% names(datos)) datos$cedula[primary_rows] else character(0))
    }, character(1)),
    departamentos = vapply(row_indices, function(indices) {
      if ("departamento" %in% names(datos)) collapse_unique_terms(datos$departamento[indices]) else ""
    }, character(1)),
    estatuses = vapply(row_indices, function(indices) {
      if (is.null(status_column)) return("")
      collapse_unique_terms(datos[[status_column]][indices])
    }, character(1)),
    tipos = vapply(row_indices, function(indices) {
      if ("doc_type" %in% names(datos)) collapse_unique_terms(datos$doc_type[indices]) else ""
    }, character(1)),
    row_indices = I(row_indices),
    stringsAsFactors = FALSE
  )

  if (nrow(index_df) == 0) {
    return(index_df)
  }

  index_df <- index_df[order(-index_df$doc_count, index_df$persona), , drop = FALSE]
  rownames(index_df) <- NULL
  index_df
}

first_non_empty_value <- function(values) {
  values <- trimws(as.character(values))
  values <- values[nzchar(values)]
  if (length(values) == 0) {
    return("")
  }

  values[[1]]
}

build_rrhh_person_profile <- function(datos, persona) {
  if (is.null(datos) || nrow(datos) == 0 || is.null(persona) || !nzchar(persona)) {
    return(NULL)
  }

  keep <- vapply(
    seq_len(nrow(datos)),
    function(row_index) row_has_any_person(datos$empleado[row_index], datos$personas_relacionadas[row_index], persona),
    logical(1)
  )

  rows <- datos[keep, , drop = FALSE]
  if (nrow(rows) == 0) {
    return(NULL)
  }

  status_column <- get_rrhh_status_column(rows)
  primary_rows <- trimws(as.character(rows$empleado)) == persona

  list(
    persona = persona,
    row_indices = which(keep),
    rows = rows,
    foto_url = if ("foto_url" %in% names(rows)) first_non_empty_value(rows$foto_url) else "",
    cedulas = if ("cedula" %in% names(rows)) collapse_unique_terms(rows$cedula[primary_rows]) else "",
    departamentos = if ("departamento" %in% names(rows)) collapse_unique_terms(rows$departamento) else "",
    statuses = if (!is.null(status_column)) collapse_unique_terms(rows[[status_column]]) else "",
    fecha_ingreso = if ("fecha_ingreso" %in% names(rows)) collapse_unique_terms(rows$fecha_ingreso) else "",
    fecha_jubilacion = if ("fecha_jubilacion" %in% names(rows)) collapse_unique_terms(rows$fecha_jubilacion) else "",
    fecha_pension = if ("fecha_pension" %in% names(rows)) collapse_unique_terms(rows$fecha_pension) else "",
    tipos = if ("doc_type" %in% names(rows)) collapse_unique_terms(rows$doc_type) else "",
    categories = if ("doc_type" %in% names(rows)) sort(unique(trimws(as.character(rows$doc_type)))) else character(0)
  )
}

filter_rrhh_person_index <- function(index_df, search_term) {
  if (is.null(search_term) || !nzchar(search_term)) {
    return(index_df)
  }

  query <- tolower(trimws(search_term))
  tokens <- strsplit(query, "\\s+")[[1]]
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) == 0) {
    return(index_df)
  }

  search_cols <- c("persona", "cedulas", "departamentos", "estatuses", "tipos")
  search_cols <- search_cols[search_cols %in% names(index_df)]
  if (length(search_cols) == 0) {
    return(index_df)
  }

  haystack <- apply(index_df[, search_cols, drop = FALSE], 1, function(row) {
    tolower(paste(row, collapse = " "))
  })

  keep <- vapply(haystack, function(text) {
    all(vapply(tokens, function(token) grepl(token, text, fixed = TRUE), logical(1)))
  }, logical(1))

  index_df[keep, , drop = FALSE]
}

filter_rrhh_person_files <- function(datos, search_term = NULL, doc_types = NULL, sort_mode = NULL) {
  out <- datos

  if (!is.null(search_term) && nzchar(search_term)) {
    query <- tolower(trimws(search_term))
    tokens <- strsplit(query, "\\s+")[[1]]
    tokens <- tokens[nzchar(tokens)]

    search_cols <- c(
      "empleado", "cedula", "departamento", "doc_type", "estatus",
      "fecha_ingreso", "ubicacion", "personas_relacionadas",
      "tesauro_primario", "tesauro_secundario", "descriptores_libres"
    )
    search_cols <- search_cols[search_cols %in% names(out)]

    if (length(search_cols) > 0) {
      haystack <- apply(out[, search_cols, drop = FALSE], 1, function(row) {
        tolower(paste(row, collapse = " "))
      })

      keep <- vapply(haystack, function(text) {
        all(vapply(tokens, function(token) grepl(token, text, fixed = TRUE), logical(1)))
      }, logical(1))

      out <- out[keep, , drop = FALSE]
    }
  }

  out <- filter_by_doc_types(out, doc_types)

  if (!is.null(sort_mode)) {
    if (identical(sort_mode, "Título A-Z")) {
      if ("doc_type" %in% names(out)) {
        out <- out[order(out$doc_type, out$empleado), , drop = FALSE]
      }
    } else if (identical(sort_mode, "Lo más relevante")) {
      if ("fecha_ingreso" %in% names(out)) {
        fechas <- suppressWarnings(as.Date(out$fecha_ingreso, format = "%Y-%m-%d"))
        out <- out[order(fechas, out$empleado, decreasing = TRUE), , drop = FALSE]
      }
    }
  }

  out
}

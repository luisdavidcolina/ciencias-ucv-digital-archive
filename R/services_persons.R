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

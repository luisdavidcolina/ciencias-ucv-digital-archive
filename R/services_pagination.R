safe_as_numeric <- function(x, fallback = 5) {
  n <- suppressWarnings(as.numeric(x))
  if (is.na(n) || n <= 0) return(fallback)
  n
}

compute_total_pages <- function(total_rows, rows_per_page) {
  if (is.null(total_rows) || total_rows <= 0) return(0)
  ceiling(total_rows / rows_per_page)
}

clamp_page <- function(page, total_pages) {
  if (total_pages <= 0) return(0)
  max(1, min(page, total_pages))
}

paginate_rows <- function(datos, page, rows_per_page) {
  rows_per_page <- safe_as_numeric(rows_per_page)
  total_pages <- compute_total_pages(nrow(datos), rows_per_page)
  current_page <- clamp_page(page, total_pages)

  if (nrow(datos) == 0 || total_pages == 0) {
    return(list(
      data = datos[0, , drop = FALSE],
      page = current_page,
      total_pages = total_pages,
      start = 0,
      end = 0,
      rows_per_page = rows_per_page
    ))
  }

  start <- (current_page - 1) * rows_per_page + 1
  end <- min(current_page * rows_per_page, nrow(datos))

  list(
    data = datos[start:end, , drop = FALSE],
    page = current_page,
    total_pages = total_pages,
    start = start,
    end = end,
    rows_per_page = rows_per_page
  )
}

build_pagination_controls <- function(prev_id, next_id, page_info_id, current_page, total_pages) {
  prev_disabled <- current_page <= 1
  next_disabled <- current_page >= total_pages
  prev_attr <- if (prev_disabled) "disabled" else NULL
  next_attr <- if (next_disabled) "disabled" else NULL

  div(
    class = "d-flex justify-content-between align-items-center mt-3",
    actionButton(
      prev_id,
      "Anterior",
      icon = icon("chevron-left"),
      class = "btn btn-outline-secondary btn-sm",
      disabled = prev_attr
    ),
    textOutput(page_info_id, inline = TRUE),
    actionButton(
      next_id,
      "Siguiente",
      icon = icon("chevron-right"),
      class = "btn btn-outline-secondary btn-sm",
      disabled = next_attr
    )
  )
}

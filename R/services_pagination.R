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

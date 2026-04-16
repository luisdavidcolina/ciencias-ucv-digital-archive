escape_html <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

format_export_value <- function(x) {
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }

  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

dataframe_to_html_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return('<p class="empty-note">Sin registros para exportar.</p>')
  }

  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- escape_html(names(df))

  header_html <- paste0("<tr>", paste(sprintf("<th>%s</th>", names(df)), collapse = ""), "</tr>")

  body_html <- apply(df, 1, function(row) {
    values <- escape_html(format_export_value(row))
    paste0("<tr>", paste(sprintf("<td>%s</td>", values), collapse = ""), "</tr>")
  })

  paste0(
    '<table class="export-table">',
    '<thead>', header_html, '</thead>',
    '<tbody>', paste(body_html, collapse = ""), '</tbody>',
    '</table>'
  )
}

build_excel_html_document <- function(title, sections) {
  sections_html <- vapply(sections, function(section) {
    section_title <- if (!is.null(section$title)) section$title else "Sección"
    section_note <- if (!is.null(section$note) && nzchar(section$note)) {
      paste0('<p class="section-note">', escape_html(section$note), '</p>')
    } else {
      ""
    }
    section_body <- if (!is.null(section$html)) {
      section$html
    } else {
      dataframe_to_html_table(section$df)
    }

    paste0(
      '<section class="export-section">',
      sprintf('<h2>%s</h2>', escape_html(section_title)),
      section_note,
      section_body,
      '</section>'
    )
  }, character(1))

  paste0(
    '<!DOCTYPE html><html><head><meta charset="UTF-8">',
    sprintf('<title>%s</title>', escape_html(title)),
    '<style>',
    'body{font-family:Arial,sans-serif;color:#1f2937;margin:24px;} ',
    'h1{font-size:22px;margin:0 0 8px 0;color:#1f3f63;} ',
    '.meta{color:#6b7280;margin-bottom:18px;font-size:13px;} ',
    '.export-section{margin-bottom:28px;} ',
    '.export-section h2{font-size:16px;margin:0 0 10px 0;color:#2b4e72;} ',
    '.section-note{margin:0 0 10px 0;color:#6b7280;font-size:12px;} ',
    '.export-table{border-collapse:collapse;width:100%;margin-top:8px;} ',
    '.export-table th,.export-table td{border:1px solid #d1d5db;padding:6px 8px;font-size:12px;vertical-align:top;} ',
    '.export-table th{background:#f3f6fb;font-weight:700;text-align:left;} ',
    '.empty-note{color:#6b7280;font-style:italic;} ',
    '</style></head><body>',
    sprintf('<h1>%s</h1>', escape_html(title)),
    sprintf('<div class="meta">Generado el %s</div>', escape_html(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))),
    paste(sections_html, collapse = ""),
    '</body></html>'
  )
}

write_excel_compatible_xls <- function(path, title, sections) {
  html <- build_excel_html_document(title, sections)
  writeLines(html, con = path, useBytes = TRUE)
}
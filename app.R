# Explicit app bootstrap with actionable startup errors.
required_files <- c("global.R", "ui.R", "server.R")
missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0) {
  stop(
    paste0("Missing required file(s): ", paste(missing_files, collapse = ", ")),
    call. = FALSE
  )
}

safe_source <- function(path) {
  tryCatch(
    {
      source(path, local = TRUE, encoding = "UTF-8")
    },
    error = function(e) {
      stop(
        paste0("Failed while sourcing ", normalizePath(path), ": ", conditionMessage(e)),
        call. = FALSE
      )
    }
  )
}

safe_source("global.R")
safe_source("ui.R")
safe_source("server.R")

if (!exists("ui", inherits = FALSE)) {
  stop("Object 'ui' was not created by ui.R", call. = FALSE)
}

if (!exists("server", inherits = FALSE)) {
  stop("Object 'server' was not created by server.R", call. = FALSE)
}

shiny::shinyApp(ui = ui, server = server)

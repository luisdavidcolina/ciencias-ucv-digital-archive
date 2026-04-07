library(shiny)
library(bs4Dash)

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")
  ),
  # Absolutamente toda la Interfaz vendrá validada y enviada desde el backend seguro
  uiOutput("page_content")
)

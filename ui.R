library(shiny)
library(bs4Dash)
library(DT)

ui <- bs4DashPage(
  title = "Ciencias UCV Digital Archive",
  header = bs4DashNavbar(
    status = "primary",
    title = bs4DashBrand(
      title = "DSpace UCV",
      color = "primary",
      href = "#"
    )
  ),
  sidebar = bs4DashSidebar(
    status = "primary",
    elevation = 3,
    bs4SidebarMenu(
      id = "menu_lateral",
      bs4SidebarMenuItem("Archivos de Extensión", tabName = "Extensión", icon = icon("book")),
      bs4SidebarMenuItem("Expedientes RRHH", tabName = "RRHH", icon = icon("users-lock"))
    )
  ),
  body = bs4DashBody(
    # Inyectar el CSS de DSpace
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$script(src = "https://kit.fontawesome.com/a076d05399.js", crossorigin="anonymous") # Asegurar carga de iconos
    ),
    
    fluidRow(
      box(
        title = textOutput("titulo_caja"),
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        maximizable = TRUE,
        DTOutput("tabla_dspace")
      )
    )
  )
)

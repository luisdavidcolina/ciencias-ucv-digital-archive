library(shiny)
library(bs4Dash)

ui <- bs4DashPage(
  title = "Ciencias UCV Digital Archive",
  dark = NULL,
  
  # Cabecera simulando DSpace 7 TopNav
  header = bs4DashNavbar(
    status = "dark",  # Forzamos barra oscura
    skin = "dark",
    
    title = bs4DashBrand(
      title = "DSpace UCV",
      color = "primary",  
      href = "#"
    ),
    
    # Controles derechos (Search / Login)
    rightUi = tags$ul(
      class = "navbar-nav",
      tags$li(class = "nav-item", tags$a(href="#", class="nav-link", icon("search"))),
      tags$li(class = "nav-item", tags$a(href="#", class="nav-link", icon("sign-in-alt"), " Log in"))
    )
  ),
  
  # Deshabilitamos la barra lateral de dashboard para usar un layout fluido clásico
  sidebar = bs4DashSidebar(disable = TRUE), 
  
  body = bs4DashBody(
    # Inyectar estilos
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css") 
    ),
    
    fluidPage(
      # Breadcrumb pseudo
      fluidRow(
        column(12, 
               tags$p(style="color: #6c757d; margin-top: 5px; font-size: 13px;", 
                      "DSpace Home / Resultados de Búsqueda")
        )
      ),
      
      fluidRow(
        # ========================================
        # COLUMNA IZQUIERDA: FACETAS / FILTROS (25%)
        # ========================================
        column(width = 3,
               div(class = "filter-card",
                   div(class = "filter-header", "Discover"),
                   div(class = "filter-body",
                       
                       radioButtons("modulo_filter", "Colección Institucional:", 
                                    choices = c("Todos", "Extensión", "RRHH"), 
                                    selected = "Todos"),
                       hr(),
                       checkboxGroupInput("doc_type_filter", "Tipo de Documento:",
                                          choices = c("Convenio", "Hoja de Vida", "Tesis", "Informe", "Contrato"),
                                          selected = character(0)),
                       div(
                         style="margin-top:20px;",
                         actionButton("btn_update", "UPDATE FILTERS", class="btn btn-sm btn-secondary w-100")
                       )
                   )
               )
        ),
        
        # ========================================
        # COLUMNA DERECHA: RESULTADOS (75%)
        # ========================================
        column(width = 9,
               # Barra de búsqueda
               div(class = "search-header-box",
                   fluidRow(
                     column(10,
                            textInput("search_text", label = NULL, placeholder = "Search DSpace...", width = "100%")
                     ),
                     column(2,
                            actionButton("btn_search", "Go", icon = icon("search"), class="btn btn-primary w-100")
                     )
                   )
               ),
               
               # Motor de tarjetas inyectadas desde el Server
               uiOutput("dspace_item_list")
        )
      )
    )
  )
)

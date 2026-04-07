library(shiny)
library(bs4Dash)

ui <- bs4DashPage(
  title = "Ciencias UCV Digital Archive",
  dark = NULL,
  
  # Cabecera estructurada genérica de bs4Dash
  header = bs4DashNavbar(
    status = "white",
    skin = "light",
    title = bs4DashBrand(
      title = "DSpace UCV",
      color = "white",
      href = "#"
    ),
    
    # Controles derechos (Search / Login) compatibilizados
    rightUi = tagList(
      tags$li(class = "nav-item dropdown", tags$a(href="#", class="nav-link", icon("search"))),
      tags$li(class = "nav-item dropdown", tags$a(href="#", class="nav-link", icon("sign-in-alt"), " Log in"))
    )
  ),
  
  # Sidebar deshabilitado estéticamente pero el engine está activo
  sidebar = bs4DashSidebar(disable = TRUE),
  
  body = bs4DashBody(
    # CSS Overrides inyectables
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")
    ),
    
    # Falsa Breadcrumb de navegación
    fluidRow(
      column(12, 
        div(class = "ds-breadcrumb-wrapper",
            div(class = "ds-breadcrumb", "Home  /  Search")
        )
      )
    ),
    
    # Grid Principal Shiny Nativo
    fluidRow(
      # Columna Izquierda: Componentes NATIVOS bs4Dash de Shiny
      column(width = 3,
          # Uso oficial del framework para la Card. CSS le quitara la pintura AdminLTE.
          bs4Card(
            title = "Discover",
            status = "secondary",
            solidHeader = FALSE,
            collapsible = FALSE,
            width = 12,
            
            # Entradas de R Nativas y Seguras
            radioButtons("modulo_filter", "Collection:", 
                         choices = c("Todos", "Extensión", "RRHH"), 
                         selected = "Todos"),
            tags$hr(),
            checkboxGroupInput("doc_type_filter", "Item Type:",
                               choices = c("Convenio", "Hoja de Vida", "Tesis", "Informe", "Contrato"),
                               selected = character(0)),
            
            div(style="margin-top:15px;",
                actionButton("btn_update", "Update", class="btn ds-btn-primary w-100")
            )
          )
      ),
      
      # Columna Derecha: Búsqueda y Resultados Dinámicos
      column(width = 9,
             # Barra de búsqueda unida orgánicamente a Shiny
             div(class = "ds-search-bar",
                 div(class = "input-group",
                     tags$input(id="search_text", type="text", class="form-control ds-search-input", placeholder="Search DSpace..."),
                     div(class="input-group-append",
                         # actionButton de R nativo con clase custom
                         actionButton("btn_search", label = NULL, icon = icon("search"), class="btn ds-btn-primary")
                     )
                 )
             ),
             
             # Salida Reactiva Rura
             uiOutput("dspace_item_list")
      )
    )
  )
)

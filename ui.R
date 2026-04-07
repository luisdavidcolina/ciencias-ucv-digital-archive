library(shiny)
library(bslib)

ui <- fluidPage(
  title = "Ciencias UCV Digital Archive",
  theme = bs_theme(version = 4), # Usamos raw Bootstrap 4 como DSpace
  padding = 0,
  
  # Inyectar estilos CSS copiados del DSpace Angular CLI
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")
  ),
  
  # ---------------------------------------------
  # DSPACE NAVBAR OFICIAL (Sin envoltorios raros)
  # ---------------------------------------------
  tags$header(
    tags$nav(class = "ds-header-nav",
      tags$a(class = "ds-header-logo", href="#", "DSpace UCV"),
      tags$ul(class = "navbar-nav",
        tags$li(class = "nav-item", tags$a(href="#", class="nav-link", icon("search"))),
        tags$li(class = "nav-item", tags$a(href="#", class="nav-link", icon("sign-in-alt"), " Log in"))
      )
    )
  ),
  
  # ---------------------------------------------
  # MAIN CONTAINER WEB (Idéntico a DSpace Grid)
  # ---------------------------------------------
  div(class = "container", style = "margin-top: 2rem; max-width: 1140px; padding-bottom: 50px;",
    
    # Falsa Breadcrumb de navegación
    div(class = "ds-breadcrumb-wrapper",
        div(class = "ds-breadcrumb", "Home  /  Search")
    ),
    
    fluidRow(
      # Columna Izquierda: Discover Facets (25%)
      column(width = 3,
             div(class = "ds-facet-accordion",
                 div(class = "ds-facet-header", "Discover"),
                 div(class = "ds-facet-body",
                     radioButtons("modulo_filter", "Collection:", 
                                  choices = c("Todos", "Extensión", "RRHH"), 
                                  selected = "Todos"),
                     tags$hr(),
                     checkboxGroupInput("doc_type_filter", "Item Type:",
                                        choices = c("Convenio", "Hoja de Vida", "Tesis", "Informe", "Contrato"),
                                        selected = character(0)),
                     div(style="margin-top:20px;",
                         actionButton("btn_update", "Update", class="btn ds-btn-primary w-100")
                     )
                 )
             )
      ),
      
      # Columna Derecha: Búsqueda y Resultados (75%)
      column(width = 9,
             
             # Barra de búsqueda con Input Group real
             div(class = "ds-search-bar",
                 div(class = "input-group",
                     tags$input(id="search_text", type="text", class="form-control ds-search-input", placeholder="Search DSpace..."),
                     div(class="input-group-append",
                         tags$button(id="btn_search", type="button", class="btn ds-btn-primary action-button", icon("search"))
                     )
                 )
             ),
             
             # Div de inyección dinámica para las tarjetas
             uiOutput("dspace_item_list")
      )
    )
  )
)

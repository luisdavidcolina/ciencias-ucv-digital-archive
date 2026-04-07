library(shiny)
library(bs4Dash)

ui <- bs4DashPage(
  title = "Ciencias UCV Digital Archive",
  dark = NULL,
  
  header = bs4DashNavbar(
    status = "white",
    skin = "light",
    title = bs4DashBrand(
      title = "DSpace UCV",
      color = "white",
      href = "#"
    ),
    
    rightUi = tagList(
      tags$li(class = "nav-item dropdown", tags$a(href="#", class="nav-link", icon("search"))),
      tags$li(class = "nav-item dropdown", tags$a(href="#", class="nav-link", icon("sign-in-alt"), " Log in"))
    )
  ),
  
  # ========================================
  # SIDEBAR ADMINISTRATIVO (Resurección)
  # ========================================
  sidebar = bs4DashSidebar(
    skin = "dark",  # Sidebar oscuro
    status = "primary",
    elevation = 3,
    bs4SidebarMenu(
      id = "sidebarmenu",
      bs4SidebarHeader("DSpace Main"),
      bs4SidebarMenuItem("Buscar (Público)", tabName = "search_tab", icon = icon("search")),
      bs4SidebarHeader("Backoffice"),
      bs4SidebarMenuItem("Mi DSpace", tabName = "mydspace_tab", icon = icon("inbox"), badgeLabel = "3", badgeColor = "warning"),
      bs4SidebarMenuItem("Nuevo Ítem", tabName = "new_tab", icon = icon("plus")),
      bs4SidebarMenuItem("Control de Acceso", tabName = "access_tab", icon = icon("users-cog"))
    )
  ),
  
  body = bs4DashBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")
    ),
    
    # Enrutador
    bs4TabItems(
      
      # ========================================
      # TAB 1: VISTA PÚBLICA (La Inalterada)
      # ========================================
      bs4TabItem(
        tabName = "search_tab",
        fluidRow(
          column(12, 
            div(class = "ds-breadcrumb-wrapper",
                div(class = "ds-breadcrumb", "Home  /  Search")
            )
          )
        ),
        fluidRow(
          column(width = 3,
              bs4Card(
                title = "Discover", status = "secondary", solidHeader = FALSE, collapsible = FALSE, width = 12,
                radioButtons("modulo_filter", "Collection:", choices = c("Todos", "Extensión", "RRHH"), selected = "Todos"),
                tags$hr(),
                checkboxGroupInput("doc_type_filter", "Item Type:", choices = c("Convenio", "Hoja de Vida", "Tesis", "Informe", "Contrato"), selected = character(0)),
                div(style="margin-top:15px;", actionButton("btn_update", "Update", class="btn ds-btn-primary w-100"))
              )
          ),
          column(width = 9,
                 div(class = "ds-search-bar",
                     div(class = "input-group",
                         tags$input(id="search_text", type="text", class="form-control ds-search-input", placeholder="Search DSpace..."),
                         div(class="input-group-append", actionButton("btn_search", label = NULL, icon = icon("search"), class="btn ds-btn-primary"))
                     )
                 ),
                 uiOutput("dspace_item_list")
          )
        )
      ),
      
      # ========================================
      # TAB 2: MI DSPACE (Backend Aprobaciones)
      # ========================================
      bs4TabItem(
        tabName = "mydspace_tab",
        # Titulo Admin
        tags$div(style="margin-bottom: 20px;", 
            tags$h3(style="font-family: 'Nunito', sans-serif; color: #212529;", "Mi DSpace")
        ),
        
        # Cajas Estadísticas tipo Dashboard Institucional
        fluidRow(
          bs4ValueBox(
            value = "3", subtitle = "Flujo de trabajo pendiente", icon = icon("clipboard-check"),
            color = "warning", width = 4
          ),
          bs4ValueBox(
            value = "12", subtitle = "Envíos archivados", icon = icon("archive"),
            color = "success", width = 4
          ),
          bs4ValueBox(
            value = "0", subtitle = "Elementos rechazados", icon = icon("times-circle"),
            color = "danger", width = 4
          )
        ),
        
        # Bandeja de Entrada Simulacro
        fluidRow(
          column(width = 12,
                 bs4Card(
                   title = "Tareas Pendientes de Aprobación",
                   status = "primary",
                   solidHeader = FALSE,
                   width = 12,
                   collapsible = FALSE,
                   # Usamos el motor genérico que será llenado por renderUI para mostrar tarjetas accionables
                   uiOutput("admin_workflow_list")
                 )
          )
        )
      ),
      
      # Tabs Placholders
      bs4TabItem(tabName = "new_tab", tags$h3("Formulario de Carga (En proceso...)")),
      bs4TabItem(tabName = "access_tab", tags$h3("Gestión de Usuarios y Roles E-People"))
    )
  )
)

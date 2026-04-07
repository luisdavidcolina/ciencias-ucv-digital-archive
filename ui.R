library(shiny)
library(bs4Dash)

ui <- bs4DashPage(
  title = "Ciencias UCV Digital Archive",
  dark = NULL,
  
  header = bs4DashNavbar(
    status = "white", skin = "light",
    title = bs4DashBrand(title = "DSpace UCV", color = "white", href = "#"),
    rightUi = tagList(
      tags$li(class = "nav-item dropdown", tags$a(href="#", class="nav-link", icon("search"))),
      tags$li(class = "nav-item dropdown", tags$a(href="#", class="nav-link", icon("sign-in-alt"), " Log in"))
    )
  ),
  
  sidebar = bs4DashSidebar(
    skin = "dark", status = "primary", elevation = 3,
    bs4SidebarMenu(
      id = "sidebarmenu",
      bs4SidebarHeader("Comunidades Principales"),
      bs4SidebarMenuItem("Archivo Extensión", tabName = "tab_extension", icon = icon("university")),
      bs4SidebarMenuItem("Expedientes RRHH", tabName = "tab_rrhh", icon = icon("lock")),
      bs4SidebarHeader("Backoffice"),
      bs4SidebarMenuItem("Mi DSpace (Admin)", tabName = "mydspace_tab", icon = icon("inbox"), badgeLabel = "2", badgeColor = "warning")
    )
  ),
  
  body = bs4DashBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")
    ),
    
    bs4TabItems(
      
      # ========================================
      # TAB 1: EXTENSIÓN UCV
      # ========================================
      bs4TabItem(tabName = "tab_extension",
        fluidRow(column(12, div(class = "ds-breadcrumb-wrapper", div(class = "ds-breadcrumb", "Comunidades / Extensión / Búsqueda")))),
        fluidRow(
          column(width = 3,
              bs4Card(title = "Filtros Académicos", status = "secondary", solidHeader = F, collapsible = F, width = 12,
                checkboxGroupInput("ext_doc_type", "Tipología Documental:", choices = c("Proyecto de Investigación", "Plano Arquitectónico", "Acta", "Convenio"), selected = character(0)),
                div(style="margin-top:15px;", actionButton("btn_update_ext", "Aplicar", class="btn ds-btn-primary w-100"))
              )
          ),
          column(width = 9,
                 div(class = "ds-search-bar",
                     div(class = "input-group",
                         tags$input(id="search_ext", type="text", class="form-control ds-search-input", placeholder="Buscar proyectos, actas..."),
                         div(class="input-group-append", actionButton("btn_s_ext", label = NULL, icon = icon("search"), class="btn ds-btn-primary"))
                     )
                 ),
                 uiOutput("list_extension")
          )
        )
      ),
      
      # ========================================
      # TAB 2: RECURSOS HUMANOS (RRHH)
      # ========================================
      bs4TabItem(tabName = "tab_rrhh",
        fluidRow(column(12, div(class = "ds-breadcrumb-wrapper", div(class = "ds-breadcrumb", "Comunidades / RRHH / Búsqueda Reservada")))),
        fluidRow(
          column(width = 3,
              bs4Card(title = "Filtros Laborales", status = "secondary", solidHeader = F, collapsible = F, width = 12, class = "ds-facet-accordion",
                radioButtons("rrhh_estatus", "Condición:", choices = c("Todos", "Activo", "Jubilado"), selected = "Todos"),
                tags$hr(),
                checkboxGroupInput("rrhh_doc_type", "Clase Documento:", choices = c("Hoja de Vida", "Contrato", "Evaluación Desempeño", "Nómina"), selected = character(0)),
                div(style="margin-top:15px;", actionButton("btn_update_rrhh", "Aplicar", class="btn ds-btn-primary w-100"))
              )
          ),
          column(width = 9,
                 div(class = "ds-search-bar",
                     div(class = "input-group",
                         tags$input(id="search_rrhh", type="text", class="form-control ds-search-input", placeholder="Buscar por Cédula o Empleado..."),
                         div(class="input-group-append", actionButton("btn_s_rrhh", label = NULL, icon = icon("search"), class="btn ds-btn-primary"))
                     )
                 ),
                 uiOutput("list_rrhh")
          )
        )
      ),
      
      # ========================================
      # TAB 3: ADMIN BACKOFFICE (MY DSPACE)
      # ========================================
      bs4TabItem(tabName = "mydspace_tab",
        tags$div(style="margin-bottom: 20px;", tags$h3(style="font-family: 'Nunito', sans-serif; color: #212529;", "Mi DSpace")),
        fluidRow(
          bs4ValueBox(value = "2", subtitle = "Flujo de Trabajo", icon = icon("clipboard-check"), color = "warning", width = 4),
          bs4ValueBox(value = "18", subtitle = "Envíos archivados", icon = icon("archive"), color = "success", width = 4),
          bs4ValueBox(value = "1", subtitle = "Rechazados", icon = icon("times-circle"), color = "danger", width = 4)
        ),
        fluidRow(
          column(width = 12,
                 bs4Card(title = "Tareas Pendientes de Revisión", status = "primary", solidHeader = F, width = 12, collapsible = F, 
                         uiOutput("admin_workflow_list"))
          )
        )
      )
    )
  )
)

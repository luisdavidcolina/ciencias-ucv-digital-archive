build_main_body_ui <- function(session_state, db_ext, db_rrhh) {
  lista_tabs <- list()

  if (session_state$modulo == "Extensión") {
    ext_fechas <- suppressWarnings(as.Date(db_ext$fecha, format = "%Y-%m-%d"))
    ext_fechas <- ext_fechas[!is.na(ext_fechas)]
    ext_min_fecha <- if (length(ext_fechas) > 0) min(ext_fechas) else Sys.Date() - 365
    ext_max_fecha <- if (length(ext_fechas) > 0) max(ext_fechas) else Sys.Date()
    ext_min_year <- as.integer(format(ext_min_fecha, "%Y"))
    ext_max_year <- as.integer(format(ext_max_fecha, "%Y"))

    lista_tabs[[length(lista_tabs) + 1]] <- bs4TabItem(tabName = "tab_extension",
      fluidRow(column(12, div(class = "ds-breadcrumb-wrapper", div(class = "ds-breadcrumb", "Comunidades / Extensión / Búsqueda")))),
      fluidRow(
        column(width = 3,
          bs4Card(
            title = "Filtros Académicos", status = "secondary", width = 12,
            bs4Accordion(
              id = "ext_filters_acc",
              bs4AccordionItem(
                title = "Tipología",
                status = "lightblue",
                solidHeader = FALSE,
                collapsed = FALSE,
                checkboxGroupInput("ext_doc_type", "Selecciona una o más", choices = c("Proyecto de Investigación", "Plano Arquitectónico", "Acta", "Convenio"))
              ),
              bs4AccordionItem(
                title = "Fecha",
                status = "teal",
                solidHeader = FALSE,
                collapsed = TRUE,
                tagList(
                  dateRangeInput("ext_date_range", "Rango de fecha", start = ext_min_fecha, end = ext_max_fecha, language = "es", width = "100%"),
                  sliderInput("ext_year_range", "Rango de años", min = ext_min_year, max = ext_max_year, value = c(ext_min_year, ext_max_year), sep = "")
                )
              )
            ),
            actionButton("btn_update_ext", "Aplicar", class = "btn ds-btn-primary w-100 mt-2")
          ),
          bs4Card(
            title = "Vista", status = "secondary", width = 12, class = "mt-3",
            selectInput("sort_ext", "Recientes", choices = c("Lo más relevante", "Título A-Z")),
            selectInput("rpp_ext", "Pág:", choices = c("5", "10"), selected = "5")
          )
        ),
        column(width = 9,
          div(class = "ds-search-bar", div(class = "input-group", tags$input(id = "search_ext", type = "text", class = "form-control ds-search-input", placeholder = "Buscar..."), div(class = "input-group-append", actionButton("btn_s_ext", label = NULL, icon = icon("search"), class = "btn ds-btn-primary")))),
          uiOutput("list_extension"),
          uiOutput("ext_pagination_controls")
        )
      )
    )
  }

  if (session_state$modulo == "RRHH") {
    rrhh_fechas <- suppressWarnings(as.Date(db_rrhh$fecha_ingreso, format = "%Y-%m-%d"))
    rrhh_fechas <- rrhh_fechas[!is.na(rrhh_fechas)]
    rrhh_min_fecha <- if (length(rrhh_fechas) > 0) min(rrhh_fechas) else Sys.Date() - 365
    rrhh_max_fecha <- if (length(rrhh_fechas) > 0) max(rrhh_fechas) else Sys.Date()
    rrhh_min_year <- as.integer(format(rrhh_min_fecha, "%Y"))
    rrhh_max_year <- as.integer(format(rrhh_max_fecha, "%Y"))

    lista_tabs[[length(lista_tabs) + 1]] <- bs4TabItem(tabName = "tab_rrhh",
      fluidRow(column(12, div(class = "ds-breadcrumb-wrapper", div(class = "ds-breadcrumb", "Comunidades / RRHH / Privado")))),
      fluidRow(
        column(width = 3,
          bs4Card(
            title = "Filtros", status = "secondary", width = 12,
            bs4Accordion(
              id = "rrhh_filters_acc",
              bs4AccordionItem(
                title = "Tipología",
                status = "lightblue",
                solidHeader = FALSE,
                collapsed = FALSE,
                selectInput("rrhh_doc_type", "Tipo de expediente", choices = c("Todas" = "", sort(unique(db_rrhh$doc_type))), width = "100%")
              ),
              bs4AccordionItem(
                title = "Fecha",
                status = "teal",
                solidHeader = FALSE,
                collapsed = TRUE,
                tagList(
                  dateRangeInput("rrhh_date_range", "Fecha de ingreso", start = rrhh_min_fecha, end = rrhh_max_fecha, language = "es", width = "100%"),
                  sliderInput("rrhh_year_range", "Rango de años", min = rrhh_min_year, max = rrhh_max_year, value = c(rrhh_min_year, rrhh_max_year), sep = "")
                )
              ),
              bs4AccordionItem(
                title = "Estatus",
                status = "secondary",
                solidHeader = FALSE,
                collapsed = TRUE,
                radioButtons("rrhh_estatus", "Condición", choices = c("Todos", "Activo", "Jubilado", "Inactivo"))
              )
            ),
            actionButton("btn_update_rrhh", "Aplicar", class = "btn ds-btn-primary w-100 mt-2")
          )
        ),
        column(width = 9, uiOutput("list_rrhh"))
      )
    )
  }

  if (session_state$rol == "Admin") {
    lista_tabs[[length(lista_tabs) + 1]] <- bs4TabItem(tabName = "mydspace_tab",
      fluidRow(column(12, div(class = "ds-breadcrumb-wrapper", div(class = "ds-breadcrumb", tags$i(class = "fas fa-shield-alt"), " Panel de Control / Administración")))),
      fluidRow(
        column(3, uiOutput("kpi_total_docs")),
        column(3, uiOutput("kpi_total_categorias")),
        column(3, uiOutput("kpi_total_usuarios")),
        column(3, uiOutput("kpi_ultimo_ingreso"))
      ),
      tabsetPanel(id = "admin_workspace_tabs", type = "pills", selected = "Estadísticas",
        tabPanel("Nuevo Ingreso", icon = icon("plus-circle"), tags$br(),
          fluidRow(
            column(width = 8,
              bs4Card(title = tags$span(tags$i(class = "fas fa-pen-fancy"), " Formulario de Metadatos Dublin Core"), status = "primary", solidHeader = FALSE, width = 12, collapsible = FALSE,
                uiOutput("admin_submit_form"),
                tags$hr(),
                tags$div(class = "d-flex justify-content-between align-items-center",
                  tags$span(class = "text-muted", style = "font-size:0.85rem;", tags$i(class = "fas fa-info-circle"), " Los campos marcados con * son obligatorios"),
                  actionButton("btn_submit_workspace", "Registrar en Archivo", class = "btn btn-success", icon = icon("cloud-upload-alt"), style = "border-radius: 8px; font-weight: bold; padding: 10px 30px; box-shadow: 0 4px 6px rgba(40, 167, 69, 0.3);")
                )
              )
            ),
            column(width = 4,
              bs4Card(title = tags$span(tags$i(class = "fas fa-cloud-upload-alt"), " Zona de Carga"), status = "secondary", solidHeader = FALSE, width = 12, collapsible = FALSE,
                tags$div(style = "border: 2px dashed #adb5bd; border-radius: 10px; padding: 30px; text-align: center; background-color: #f8f9fa; margin-bottom: 15px; transition: all 0.3s ease;",
                  tags$i(class = "fas fa-file-upload", style = "font-size: 3rem; color: #6c757d; margin-bottom: 10px;"),
                  tags$h5("ZONA DROP", style = "color:#495057; font-weight:bold;"),
                  tags$p(style = "color:#6c757d; font-size:0.85rem;", "Arrastra aquí o selecciona archivos"),
                  fileInput("file_upload", NULL, buttonLabel = "Explorar...", placeholder = "Ningún archivo", multiple = TRUE, accept = c(".pdf", ".zip", ".png", ".jpg", ".doc", ".docx"))
                ),
                tags$div(class = "mt-3",
                  tags$p(tags$i(class = "fas fa-check-circle", style = "color:#28a745;"), " Formatos: PDF, DOC, ZIP, PNG", style = "color:#6c757d; font-size:0.85rem; margin-bottom: 5px;"),
                  tags$p(tags$i(class = "fas fa-weight-hanging", style = "color:#ffc107;"), " Peso Máximo: 200MB", style = "color:#6c757d; font-size:0.85rem; margin-bottom: 5px;"),
                  tags$p(tags$i(class = "fas fa-star", style = "color:#0056b3;"), " Recomendado: PDF/A (ISO 19005)", style = "color:#6c757d; font-size:0.85rem;")
                )
              ),
              bs4Card(title = tags$span(tags$i(class = "fas fa-history"), " Últimos Ingresos"), status = "white", solidHeader = FALSE, width = 12, collapsible = TRUE, collapsed = TRUE,
                uiOutput("recent_submissions")
              )
            )
          )
        ),
        tabPanel("Monitor de Expedientes", icon = icon("table"), tags$br(),
          bs4Card(title = tags$span(tags$i(class = "fas fa-database"), " Directorio Activo Local"), status = "info", solidHeader = FALSE, width = 12, collapsible = FALSE,
            fluidRow(
              column(4, textInput("admin_search", NULL, placeholder = "Buscar en tabla...", width = "100%")),
              column(4, uiOutput("admin_filter_type")),
              column(4, tags$div(style = "text-align:right; padding-top: 5px;",
                actionButton("btn_export_csv", "Exportar CSV", icon = icon("file-csv"), class = "btn btn-outline-secondary btn-sm", style = "margin-right:5px;"),
                actionButton("btn_refresh_table", "Refrescar", icon = icon("sync-alt"), class = "btn btn-outline-info btn-sm")
              ))
            ),
            tags$hr(style = "margin-top:0;"),
            uiOutput("admin_control_table"),
            tags$div(class = "d-flex justify-content-between align-items-center mt-3",
              tags$span(class = "text-muted", style = "font-size:0.85rem;", uiOutput("admin_table_summary", inline = TRUE)),
              tags$div(
                actionButton("admin_prev", tags$i(class = "fas fa-chevron-left"), class = "btn btn-outline-secondary btn-sm", style = "margin-right:5px;"),
                tags$span(class = "text-muted", style = "font-size:0.85rem;", textOutput("admin_page_info", inline = TRUE)),
                actionButton("admin_next", tags$i(class = "fas fa-chevron-right"), class = "btn btn-outline-secondary btn-sm", style = "margin-left:5px;")
              )
            )
          )
        ),
        tabPanel("Categorías", icon = icon("sitemap"), tags$br(),
          fluidRow(
            column(width = 5,
              bs4Card(title = tags$span(tags$i(class = "fas fa-plus-square"), " Nueva Categoría"), status = "warning", solidHeader = FALSE, width = 12, collapsible = FALSE,
                textInput("new_tax_name", "Nombre de la Tipología *", placeholder = "Ej: Resolución Rectoral", width = "100%"),
                textAreaInput("new_tax_desc", "Descripción", placeholder = "Breve descripción de esta tipología documental...", rows = 3, width = "100%"),
                selectInput("new_tax_scope", "Alcance", choices = c("Extensión", "RRHH", "Ambos"), width = "100%"),
                actionButton("add_tax_btn", "Registrar Categoría", icon = icon("check-circle"), class = "btn btn-warning w-100 font-weight-bold", style = "border-radius:8px;")
              )
            ),
            column(width = 7,
              bs4Card(title = tags$span(tags$i(class = "fas fa-tags"), " Taxonomías Activas"), status = "secondary", solidHeader = FALSE, width = 12, collapsible = FALSE,
                uiOutput("admin_tax_list")
              )
            )
          )
        ),
        tabPanel("Usuarios", icon = icon("users-cog"), tags$br(),
          bs4Card(title = tags$span(tags$i(class = "fas fa-user-shield"), " Control de Acceso"), status = "danger", solidHeader = FALSE, width = 12, collapsible = FALSE,
            uiOutput("admin_users_table"),
            tags$hr(),
            tags$h6(tags$i(class = "fas fa-user-plus"), " Registrar Nuevo Usuario", style = "font-weight:bold; margin-bottom:15px;"),
            fluidRow(
              column(3, textInput("new_user_name", NULL, placeholder = "Usuario", width = "100%")),
              column(3, passwordInput("new_user_pass", NULL, placeholder = "Contraseña", width = "100%")),
              column(3, selectInput("new_user_modulo", NULL, choices = c("Extensión", "RRHH"), width = "100%")),
              column(3, selectInput("new_user_rol", NULL, choices = c("Normal", "Admin"), width = "100%"))
            ),
            actionButton("btn_add_user", "Crear Usuario", icon = icon("user-plus"), class = "btn btn-outline-danger", style = "border-radius:8px;")
          )
        ),
        tabPanel("Estadísticas", icon = icon("chart-bar"), tags$br(),
          bs4Card(title = tags$span(tags$i(class = "fas fa-filter"), " Filtros Analíticos"), status = "info", solidHeader = FALSE, width = 12, collapsible = FALSE,
            uiOutput("stats_filters_panel")
          ),
          fluidRow(
            column(6, bs4Card(title = tags$span(tags$i(class = "fas fa-chart-pie"), " Distribución por Tipo"), status = "primary", solidHeader = FALSE, width = 12, collapsible = FALSE, uiOutput("stats_by_type"))),
            column(6, bs4Card(title = tags$span(tags$i(class = "fas fa-calendar-alt"), " Cronología de Ingresos"), status = "success", solidHeader = FALSE, width = 12, collapsible = FALSE, uiOutput("stats_timeline")))
          ),
          fluidRow(
            column(12, bs4Card(title = tags$span(tags$i(class = "fas fa-server"), " Estado del Sistema"), status = "secondary", solidHeader = FALSE, width = 12, collapsible = FALSE, uiOutput("stats_system")))
          )
        )
      )
    )
  }

  do.call(bs4TabItems, lista_tabs)
}

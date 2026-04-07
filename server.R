library(shiny)
library(DT)

# --- FASE DE PRUEBA: Carga desde archivo local ---
datos <- read.csv("datos_test.csv", stringsAsFactors = FALSE)

# --- FASE REAL: (Mañana solo activas esto) ---
# datos <- dbGetQuery(con, "SELECT * FROM documentos")

server <- function(input, output, session) {
  
  # Dinamizar el título según el menú
  output$titulo_caja <- renderText({
    if(is.null(input$menu_lateral)) return("Repositorio Digital")
    paste("Repositorio Digital -", input$menu_lateral)
  })
  
  output$tabla_dspace <- renderDT({
    # Aquí filtramos los datos según el menú lateral que elija Susana
    req(input$menu_lateral)
    datos_filtrados <- datos[datos$modulo == input$menu_lateral, ]
    
    # Agregar icono de PDF al título para mimetizar DSpace
    datos_filtrados$titulo <- paste0('<i class="fas fa-file-pdf icon-file"></i> <span class="title-column">', datos_filtrados$titulo, '</span>')
    
    # Remover columna modulo para la vista
    datos_filtrados$modulo <- NULL
    
    datatable(datos_filtrados, 
              escape = FALSE, # Permitir HTML en las celdas (para el icono)
              rownames = FALSE,
              options = list(
                pageLength = 10,
                dom = 'Bfrtip',
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
              ),
              colnames = c("Título", "Tipo de Documento", "Autor", "Fecha")
    )
  })
}

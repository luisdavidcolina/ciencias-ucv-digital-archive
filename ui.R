library(shiny)
library(bs4Dash)

ui <- bs4DashPage(
  title = "",
  dark = NULL,
  header = bs4DashNavbar(
    title = NULL,
    status = "white", skin = "light",
    rightUi = tagList(
      # Estos se mostrarán solo via conditionalPanel más adelante
      tags$li(class="nav-item dropdown", 
          conditionalPanel("output.is_logged", 
              tags$span(style="padding-top:10px; display:inline-block; margin-right:15px; font-weight:bold; color:#dc3545;", tags$i(class="fas fa-user-circle"), textOutput("nav_username", inline=TRUE))
          )
      ),
      tags$li(class="nav-item dropdown", 
          conditionalPanel("output.is_logged", 
              actionButton("logout_btn", "Cerrar", icon=icon("sign-out-alt"), class="btn btn-outline-secondary btn-sm", style="margin-top: 5px; margin-right: 10px;")
          )
      )
    )
  ),
  sidebar = bs4DashSidebar(
    collapsed = FALSE,
    minified = FALSE,
    skin = "dark", status = "primary", elevation = 3,
    bs4SidebarMenu(
      id = "sidebar_tabs",
      uiOutput("sidebar_items")
    )
  ),
  body = bs4DashBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
      tags$script(HTML("\n        $(document).on('keydown', '#login_user, #login_pass', function(e) {\n          if (e.key === 'Enter') {\n            e.preventDefault();\n            $('#login_btn').trigger('click');\n          }\n        });\n\n        $(document).on('click', '#toggle_login_pass', function(e) {\n          e.preventDefault();\n          var passInput = $('#login_pass');\n          if (!passInput.length) return;\n\n          var hidden = passInput.attr('type') === 'password';\n          passInput.attr('type', hidden ? 'text' : 'password');\n\n          var icon = $(this).find('i');\n          icon.toggleClass('fa-eye fa-eye-slash');\n          $(this).attr('aria-label', hidden ? 'Ocultar contraseña' : 'Mostrar contraseña');\n        });\n      "))
    ),
    
    # PANTALLA DE LOGIN (Visible solo si no está logueado)
    conditionalPanel("!output.is_logged",
       div(class="ds-login-backdrop",
          div(class="card p-5 ds-login-card",
              div(style="text-align: center; width: 100%; margin-bottom: 20px;",
                 tags$i(class="fas fa-fingerprint", style="font-size: 4.5rem; color: #2b4e72; margin-bottom: 15px;"),
                 tags$h3(style="font-family: 'Nunito', sans-serif; color: #2b4e72; font-weight: 800; margin-bottom: 5px;", "Departamento de Extensión"),
                 tags$p(class="text-muted", "Intranet Cifrada de Acceso Restringido", style="font-size:0.95rem;")
              ),
              div(style="text-align: left; width: 100%; margin-bottom: 20px; position: relative;",
                  tags$label("Credencial Institucional", style="font-weight: bold; color: #495057; font-size:0.95rem; margin-bottom: 5px; display: block;"),
                  tags$i(class="fas fa-user auth-icon"),
                  textInput("login_user", label = NULL, placeholder="ID de Usuario", width="100%")
              ),
              div(style="text-align: left; width: 100%; margin-bottom: 30px; position: relative;",
                  tags$label("Contraseña", style="font-weight: bold; color: #495057; font-size:0.95rem; margin-bottom: 5px; display: block;"),
                  tags$i(class="fas fa-key auth-icon"),
                  div(class = "ds-password-wrap",
                    passwordInput("login_pass", label = NULL, placeholder="••••••••", width="100%"),
                    tags$button(
                      id = "toggle_login_pass",
                      type = "button",
                      class = "ds-pass-toggle",
                      `aria-label` = "Mostrar contraseña",
                      tags$i(class = "fas fa-eye")
                    )
                  ),
                  div(class = "ds-login-hint", tags$i(class = "fas fa-keyboard"), " También puedes presionar Enter para ingresar")
              ),
              div(style="width: 100%;",
                 actionButton("login_btn", "Autorizar Ingreso", icon=icon("shield-alt"), class="btn w-100", style="background-color: #2b4e72; color: white; border: none; border-radius: 25px; padding: 12px; font-weight: bold; font-size:1.15rem; box-shadow: 0 4px 15px rgba(43,78,114,0.4);")
              )
          )
       )
    ),
    
    # CONTENIDO DEL REPOSITORIO (Dashboard)
    conditionalPanel("output.is_logged",
       div(class = "ds-page-header", tags$i(class = "fas fa-landmark"), " Departamento de Extension"),
       uiOutput("main_body")
    )
  ),
  footer = bs4DashFooter(
      left = tags$a(href="#", "Universidad Central de Venezuela"),
      right = "© 2024 Archivo Ciencias"
  )
)

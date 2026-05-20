library(shiny)
library(bs4Dash)

ui <- bs4DashPage(
  title = "",
  dark = NULL,
  header = bs4DashNavbar(
    title = NULL,
    status = "white", skin = "light",
    controlbarIcon = NULL,
    leftUi = tagList(
      tags$li(
        class = "nav-item dropdown",
        tags$span(
          class = "ds-navbar-logo nav-link",
          tags$i(class = "fas fa-landmark"),
          " Archivo Institucional"
        )
      )
    ),
    rightUi = tagList(
      # Estos se mostrarán solo via conditionalPanel más adelante
      tags$li(class="nav-item dropdown ds-nav-user", 
          conditionalPanel("output.is_logged", 
              tags$span(style="padding-top:10px; display:inline-block; margin-right:15px; font-weight:bold; color:#dc3545;", tags$i(class="fas fa-user-circle"), textOutput("nav_username", inline=TRUE))
          )
      ),
      tags$li(class="nav-item dropdown ds-nav-logout", 
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
      tags$script(HTML("
        $(document).on('keydown', function(e) {
          if (e.key === 'Enter' && $('#login_btn').length > 0 && $('#login_btn').is(':visible')) {
            e.preventDefault();
            $('#login_btn')[0].click();
          }
        });

        $(document).on('click', '#toggle_login_pass', function(e) {
          e.preventDefault();
          var passInput = $('#login_pass');
          if (!passInput.length) return;

          var hidden = passInput.attr('type') === 'password';
          passInput.attr('type', hidden ? 'text' : 'password');

          var icon = $(this).find('i');
          icon.toggleClass('fa-eye fa-eye-slash');
          $(this).attr('aria-label', hidden ? 'Ocultar contraseña' : 'Mostrar contraseña');
        });

        Shiny.addCustomMessageHandler('persistSession', function(msg) {
          if (!msg || !msg.username) return;
          var payload = {
            username: msg.username,
            modulo: msg.modulo || null,
            rol: msg.rol || null,
            ts: Date.now()
          };
          localStorage.setItem('archive_session', JSON.stringify(payload));
        });

        Shiny.addCustomMessageHandler('clearPersistedSession', function(msg) {
          localStorage.removeItem('archive_session');
        });

        $(document).on('shiny:connected', function() {
          var raw = localStorage.getItem('archive_session');
          if (!raw) return;

          try {
            var saved = JSON.parse(raw);
            var ttlMs = 12 * 60 * 60 * 1000;

            if (!saved || !saved.username || !saved.ts || (Date.now() - saved.ts) > ttlMs) {
              localStorage.removeItem('archive_session');
              return;
            }

            // Agregamos un ligero retraso para asegurar que Shiny esté listo para recibir inputs
            setTimeout(function() {
              Shiny.setInputValue('restore_session', saved, { priority: 'event' });
            }, 250);
          } catch (e) {
            localStorage.removeItem('archive_session');
          }
        });

        Shiny.addCustomMessageHandler('navigateToSearch', function(msg) {
          var tab = (msg && msg.modulo === 'RRHH') ? 'tab_rrhh' : 'tab_archivo';
          var attempts = 0;
          var maxAttempts = 20;

          var timer = setInterval(function() {
            attempts += 1;

            var selectors = [
              '.main-sidebar a[data-value=\"' + tab + '\"]',
              '.main-sidebar a[href=\"#shiny-tab-' + tab + '\"]',
              '.main-sidebar a[href=\"#' + tab + '\"]',
              '.main-sidebar a[href*=\"' + tab + '\"]'
            ];

            for (var i = 0; i < selectors.length; i++) {
              var el = document.querySelector(selectors[i]);
              if (el) {
                el.click();
                clearInterval(timer);
                return;
              }
            }

            if (attempts >= maxAttempts) {
              clearInterval(timer);
            }
          }, 120);
        });
      "))
    ),
    
    # PANTALLA DE LOGIN (Visible solo si no está logueado)
    conditionalPanel("!output.is_logged",
       div(class="ds-login-backdrop", style="display: flex !important; flex-direction: column !important; justify-content: center !important; align-items: center !important; gap: 15px;",
        div(style="text-align: center; width: 100%; margin-bottom: 5px;",
          tags$img(src = "logoblanco.png", height = "64px", style = "display:block; margin: 0 auto 0;")
        ),
        div(class="card p-5 ds-login-card",
            div(style="text-align: center; width: 100%; margin-bottom: 10px;",
              tags$p(class = "text-muted", style = "font-size:0.95rem; margin:0 0 8px;",
                   tags$i(class = "fas fa-lock", style = "color: #2b4e72; margin-right:8px;"),
                   "Intranet Cifrada de Acceso Restringido"
              )
            ),
              div(style="text-align: left; width: 100%; margin-bottom: 0px; position: relative;",
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
                  )
              ),
              div(style="width: 100%;",
                 actionButton("login_btn", "Autorizar Ingreso", icon=icon("shield-alt"), class="btn w-100", style="background-color: #2b4e72; color: white; border: none; border-radius: 25px; padding: 12px; font-weight: bold; font-size:1.15rem; box-shadow: 0 4px 15px rgba(43,78,114,0.4);")
              )
          )
       )
    ),
    
    # CONTENIDO DEL REPOSITORIO (Dashboard)
    conditionalPanel("output.is_logged",
       uiOutput("main_body")
    )
  ),
  footer = bs4DashFooter(
      left = tags$a(href="#", "Universidad Central de Venezuela"),
      right = "© 2024 Archivo Institucional"
  )
)

library(shiny)
library(bs4Dash)

# Load internal app services/helpers.
source("R/services_auth.R")
source("R/services_filters.R")
source("R/services_navigation.R")
source("R/services_pagination.R")
source("R/services_export.R")
source("R/services_thesaurus.R")
source("R/services_persons.R")
source("R/server/ui_main_body.R")
source("R/server/admin_panel.R")
source("R/server/stats_admin.R")
source("R/server/document_modal.R")

# Data sources.
db_ext <- read.csv("datos_extension.csv", stringsAsFactors = FALSE)
db_rrhh <- read.csv("datos_rrhh.csv", stringsAsFactors = FALSE)
db_users <- read.csv("usuarios.csv", stringsAsFactors = FALSE)

db_ext <- ensure_tesauro_columns(db_ext)
db_rrhh <- ensure_tesauro_columns(db_rrhh)
db_rrhh <- ensure_person_columns(db_rrhh)

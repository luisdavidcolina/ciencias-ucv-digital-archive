library(shiny)
library(bs4Dash)

# Load internal app services/helpers.
source("R/services_auth.R")
source("R/services_filters.R")
source("R/services_navigation.R")
source("R/services_pagination.R")
source("R/server/ui_main_body.R")

# Data sources.
db_ext <- read.csv("datos_extension.csv", stringsAsFactors = FALSE)
db_rrhh <- read.csv("datos_rrhh.csv", stringsAsFactors = FALSE)
db_users <- read.csv("usuarios.csv", stringsAsFactors = FALSE)

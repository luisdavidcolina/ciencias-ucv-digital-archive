library(testthat)
library(shiny)

# Carga el entorno global antes de correr los tests
source("../global.R")

test_check("ciencias_ucv_archive")

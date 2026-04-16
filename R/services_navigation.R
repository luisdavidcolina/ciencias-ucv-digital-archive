resolve_default_tab <- function(modulo, rol) {
  if (identical(modulo, "Extension") || identical(modulo, "Archivo")) {
    return("tab_archivo")
  }

  if (identical(modulo, "RRHH")) {
    return("tab_rrhh")
  }

  if (identical(rol, "Admin")) {
    return("mydspace_tab")
  }

  NULL
}

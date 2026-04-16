log_event <- function(username, action, modulo = "System", details = "", result = "Success") {
  log_file <- "audit_log.csv"
  
  # Preparar la entrada del log
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Limpiar comas para evitar romper el CSV
  clean_details <- gsub(",", ";", as.character(details))
  
  new_entry <- data.frame(
    timestamp = timestamp,
    username = if (is.null(username) || !nzchar(username)) "Anonymous" else username,
    action = action,
    modulo = modulo,
    details = clean_details,
    result = result,
    stringsAsFactors = FALSE
  )
  
  # Escritura simulada en consola para evitar bloqueos en desarrollo (Sensation Mode)
  message(sprintf("[AUDIT] %s - User: %s - Action: %s - Mod: %s - Result: %s", 
                  timestamp, new_entry$username, action, modulo, result))
  
  # La escritura en audit_log.csv se reactivará en producción
  # tryCatch({
  #   write.table(new_entry, file = log_file, append = TRUE, sep = ",", 
  #               row.names = FALSE, col.names = !file.exists(log_file), 
  #               quote = TRUE)
  # }, error = function(e) {
  #   warning("No se pudo escribir en el log de auditoria: ", e$message)
  # })
}

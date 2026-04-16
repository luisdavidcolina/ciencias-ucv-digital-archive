authenticate_user <- function(db_users, user_id, password) {
  safe_user <- trimws(as.character(user_id[1]))
  safe_pass <- as.character(password[1])

  if (is.na(safe_user) || is.na(safe_pass) || !nzchar(safe_user) || !nzchar(safe_pass)) {
    return(NULL)
  }

  match_idx <- which(db_users$usuario == safe_user & db_users$password == safe_pass)
  if (length(match_idx) != 1) {
    return(NULL)
  }

  db_users[match_idx, , drop = FALSE]
}

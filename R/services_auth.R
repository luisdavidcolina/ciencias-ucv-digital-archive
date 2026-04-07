authenticate_user <- function(db_users, user_id, password) {
  if (is.null(user_id) || is.null(password) || !nzchar(user_id) || !nzchar(password)) {
    return(NULL)
  }

  match <- db_users[db_users$usuario == user_id & db_users$password == password, , drop = FALSE]
  if (nrow(match) != 1) {
    return(NULL)
  }

  match[1, , drop = FALSE]
}

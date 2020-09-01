vcapply <- function(x, fun, ...) {
  vapply(x, fun, character(1), ...)
}


vlapply <- function(x, fun, ...) {
  vapply(x, fun, logical(1), ...)
}


`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}


package_type <- function(binary, sysname = Sys.info()[["sysname"]]) {
  if (!binary) {
    return("source")
  }
  switch(tolower(sysname),
         windows = "win.binary",
         darwin = "mac.binary",
         "source")
}


file_copy <- function(from, to) {
  stopifnot(file.copy(from, to, overwrite = TRUE))
}


assert_named <- function (x, unique = FALSE, name = deparse(substitute(x))) {
  if (is.null(names(x))) {
    stop(sprintf("'%s' must be named", name), call. = FALSE)
  }
  if (unique && any(duplicated(names(x)))) {
    stop(sprintf("'%s' must have unique names", name), call. = FALSE)
  }
}


file_copy <- function(from, to) {
  stopifnot(file.copy(from, to, overwrite = TRUE))
}


r_version2 <- function(version = getRversion()) {
  as.character(version[1, 1:2])
}


dir_create <- function(path) {
  dir.create(path, FALSE, TRUE)
}


git_run <- function(args, repo, check = TRUE) {
  processx::run("git", c("-C", repo, args), error_on_status = check)
}


squote <- function(x) {
  sprintf("'%s'", x)
}

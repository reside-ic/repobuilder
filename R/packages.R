vcapply <- function(x, fun, ...) {
  vapply(x, fun, character(1), ...)
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


temp_dir <- function(type, workdir = NULL) {
  ret <- tempfile(tmpdir = workdir %||% tempdir(),
                  pattern = sprintf("pb_%s_", type))
  dir_create(ret)
  ret
}

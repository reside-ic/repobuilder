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
  ok <- file.copy(from, to, overwrite = TRUE)
  if (!all(ok)) {
    stop("Error copying files")
  }
}


assert_named <- function (x, unique = FALSE, name = deparse(substitute(x))) {
  if (is.null(names(x))) {
    stop(sprintf("'%s' must be named", name), call. = FALSE)
  }
  if (unique && any(duplicated(names(x)))) {
    stop(sprintf("'%s' must have unique names", name), call. = FALSE)
  }
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


has_gh_pages <- function(repo = ".") {
  "origin/gh-pages" %in% gert::git_branch_list(repo)$name
}


write_lines_gz <- function(text, filename, ...) {
  con <- gzfile(filename)
  on.exit(close(con))
  writeLines(text, con, ...)
}


contrib_url <- function(path, type, version) {
  if (type == "source") {
    file.path(path, "contrib", "src")
  } else {
    platform <- switch(type,
                       "win.binary" = "windows",
                       "mac.binary" = "macosx",
                       "mac.binary.mavericks" = "macosx/mavericks",
                       "mac.binary.el-capitan" = "macosx/el-capitan")
    file.path(path, "bin", platform, "contrib", version)
  }
}

same_path <- function(a, b) {
  file.exists(a) && file.exists(b) && normalizePath(a) == normalizePath(b)
}


is_mac <- function() {
  tolower(Sys.info()[["sysname"]]) == "darwin"
}


is_windows <- function() {
  tolower(Sys.info()[["sysname"]]) == "windows"
}

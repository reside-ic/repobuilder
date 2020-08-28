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

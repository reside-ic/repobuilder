rb_build_source <- function(filename, workdir, prev) {
  config <- read_config(filename)
  packages <- download_sources(config$packages, workdir)
  packages <- check_packages_version(packages, prev)
  yaml::write_yaml(packages, file.path(workdir, "src", "packages.yml"))
  build <- any(vlapply(packages, "[[", "build"))

  writeLines(as.character(build),
             file.path(workdir, "src", "build"))

  if (!build) {
    message("Nothing to update!")
    return()
  }

  lib <- prepare_library(packages, workdir)
  build_packages(packages, lib, FALSE, workdir)
}


rb_build_binaries <- function(workdir) {
  packages <- yaml::read_yaml(file.path(workdir, "src", "packages.yml"))
  build <- any(vlapply(packages, "[[", "build"))

  if (!build) {
    message("Nothing to update!")
    return()
  }

  lib <- prepare_library(packages, workdir)
  build_packages(packages, lib, TRUE, workdir)
}

gha_source <- function() {
  config <- read_config("repobuilder.yml")
  prev <- fetch_previous_index(".")
  workdir <- "gha"
  packages <- download_sources(config$packages, workdir)
  packages <- check_prev(prev, packages)
  yaml::write_yaml(packages, file.path(workdir, "src", "packages.yml"))

  lib <- prepare_library(packages, workdir)
  build_source_packages(packages, lib, workdir)
}


gha_binaries <- function() {
  workdir <- "gha"
  packages <- yaml::read_yaml(file.path(workdir, "src", "packages.yml"))
  lib <- prepare_library(packages, workdir)
  build_binary_packages(packages, lib, workdir)
}

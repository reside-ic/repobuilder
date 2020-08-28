rb_build <- function(filename = NULL, binary = TRUE, workdir = NULL) {
  config <- read_config(filename %||% "repobuilder.yml")

  if (is.null(workdir)) {
    workdir <- tempfile()
  }
  dir_create(workdir)

  packages <- config$packages
  pkg_type <- package_type(binary)

  message("Downloading sources")
  packages <- update_sources(packages, workdir)
  on.exit(unlink(packages, recursive = TRUE))

  message("Building library")
  lib <- prepare_library(packages, workdir)
  on.exit(unlink(lib, recursive = TRUE))

  message(sprintf("Building '%s' packages", package_type(binary)))
  dest <- file.path(workdir, "packages", package_type(binary), r_version2())
  pkg <- build_packages(packages, dest, lib, binary = binary)

  pkg
}


gha_build <- function() {
  rb_build("repobuilder.yml", TRUE, "work")
}


read_config <- function(filename) {
  dat <- yaml::read_yaml(filename)
  base <- basename(filename)
  assert_named(dat$packages, unique = TRUE, sprintf("%s:packages", base))
  for (i in seq_along(dat$packages)) {
    if (!is.null(dat$packages[[i]])) {
      stop(sprintf("Options not supported (for %s:packages[%d])", base, i))
    }
    dat$packages[[i]] <- pkgdepends::parse_pkg_ref(names(dat$packages)[[i]])
  }
  dat
}


## Build one big consistent library (we hope!)
prepare_library <- function(packages, workdir) {
  lib <- temp_dir("lib", workdir)
  dir_create(lib)
  lib <- normalizePath(lib, mustWork = TRUE)

  config <- list(library = lib, dependencies = TRUE)
  refs <- paste0("local::", vcapply(packages, "[[", "src"))
  proposal <- pkgdepends::new_pkg_installation_proposal(refs, config)

  proposal$solve()
  proposal$stop_for_solution_error()
  proposal$download()
  proposal$stop_for_download_error()
  plan <- proposal$get_install_plan()
  print(pkgdepends::install_package_plan(plan = plan, lib = lib))
  lib
}


update_sources <- function(packages, workdir) {
  for (i in seq_along(packages)) {
    packages[[i]]$src <- pkgbuilder::pb_source(packages[[i]]$ref, workdir)
  }
  packages
}


build_packages <- function(packages, dest, lib, binary) {
  vcapply(packages, function(p)
    build_package(p$src, dest, lib, binary = binary))
}


build_package <- function(path, dest, lib, ..., vignettes = FALSE) {
  dir_create(dest)
  withr::with_libpaths(
    lib,
    pkgbuild::build(path, dest, ..., vignettes = vignettes))
}

gha_source <- function() {
  config <- read_config("repobuilder.yml")
  prev <- fetch_previous_index(".")
  workdir <- "gha"
  packages <- download_sources(config$packages, workdir)
  packages <- check_prev(prev, packages)
  yaml::write_yaml(packages, file.path(workdir, "src", "packages.yml"))
  build <- any(vlapply(packages, "[[", "build"))

  cat(sprintf("::set-env name=REPOBUILDER_BUILD::%s\n",
              as.character(build)))

  if (!build) {
    message("Nothing to update!")
    return()
  }

  lib <- prepare_library(packages, workdir)
  build_packages(packages, lib, FALSE, workdir)
}


gha_binaries <- function() {
  workdir <- "gha"
  packages <- yaml::read_yaml(file.path(workdir, "src", "packages.yml"))
  build <- any(vlapply(packages, "[[", "build"))

  if (!build) {
    message("Nothing to update!")
    return()
  }

  lib <- prepare_library(packages, workdir)
  build_packages(packages, lib, TRUE, workdir)
}


gha_site <- function() {
  workdir <- "gha"
  gh_pages_prep()
  update_site(workdir, ".")
}


gh_pages_prep <- function() {
  if (has_gh_pages(".")) {
    gert::git_branch_create("gh-pages", "origin/gh-pages", TRUE)
  } else {
    ## Not yet supported in gert:
    processx::run("git", c("checkout", "--orphan", "gh-pages"))
    ## gert's rm does not remove enough
    processx::run("git", c("rm", "-rf", "--quiet", "."))
    ## gert's commit requires at least one file present
    processx::run("git", c("commit", "--allow-empty", "-m", "gh-pages root"))
  }
}

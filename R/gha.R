gha_source <- function() {
  config <- read_config("repobuilder.yml")
  prev <- fetch_previous_index(".")
  workdir <- "gha"
  packages <- download_sources(config$packages, workdir)
  packages <- check_prev(prev, packages)
  yaml::write_yaml(packages, file.path(workdir, "src", "packages.yml"))

  lib <- prepare_library(packages, workdir)
  build_packages(packages, lib, FALSE, workdir)
}


gha_binaries <- function() {
  workdir <- "gha"
  packages <- yaml::read_yaml(file.path(workdir, "src", "packages.yml"))
  lib <- prepare_library(packages, workdir)
  build_packages(packages, lib, TRUE, workdir)
}


gha_site <- function() {
  workdir <- "gha"
  dest <- "gh-pages"
  gh_site_prep(".", dest)
  update_site(workdir, dest)
}


gh_site_prep <- function(path, dest) {
  if (has_gh_pages(path)) {
    gert::git_branch_create("gh-pages", "origin/gh-pages", FALSE, repo = path)
    gert::git_clone(path, dest, "gh-pages")
    gert::git_remote_remove("origin", repo = dest)
  } else {
    gert::git_init(dest)
    ## Not yet supported in gert:
    processx::run("git", c("-C", dest, "checkout", "--orphan", "gh-pages"))
  }
  url <- gert::git_remote_list(path)$url
  gert::git_remote_add("origin", url, repo = dest)

  gert::git_config_set("user.email", "actions@github.com", repo = dest)
  gert::git_config_set("user.name", "GitHub Actions", repo = dest)
}

gha_source <- function() {
  config <- read_config("repobuilder.yml")
  prev <- fetch_previous_index(".")
  rb_build_source(config, "gha", prev)
}


gha_binaries <- function() {
  rb_build_binaries("gha")
}


gha_site <- function() {
  gh_pages_prep(".")
  rb_build_site("gha", ".")
  rb_update_site("gha", ".")
}

gha_source <- function() {
  prev <- fetch_previous_index(".")
  rb_build_source("repobuilder.yml", "gha", prev)
}


gha_binaries <- function() {
  rb_build_binaries("gha")
}


gha_site <- function() {
  gh_pages_prep(".")
  rb_build_site("gha", ".")
  rb_update_site("gha", ".")
}


gh_pages_prep <- function(path) {
  if (has_gh_pages(path)) {
    gert::git_branch_create("gh-pages", "origin/gh-pages", TRUE, path)
  } else {
    ## Not yet supported in gert:
    git_run(c("checkout", "--orphan", "gh-pages"), path)
    ## gert's rm does not remove enough
    git_run(c("rm", "-rf", "--quiet", path), path)
    ## gert's commit requires at least one file present
    git_run(c("commit", "--allow-empty", "-m", "gh-pages root"), path)
  }
}

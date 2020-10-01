rb_build_site <- function(workdir, dest) {
  init_repo(dest)

  ## Look at all the binary directories. Unfortunately, this is pretty
  ## tightly tied to how the gha lays out the artefacts for now. In
  ## particular, sources contains an extra path layer here.
  paths <- c(file.path(workdir, "sources", "packages"),
             dir(workdir, pattern = "^binaries-", full.names = TRUE))

  for (p in paths) {
    build_site_dir(p, dest)
  }

  update_index(workdir, dest)
}


rb_update_site <- function(workdir, path) {
  dat <- yaml::read_yaml(file.path(workdir, "sources", "src", "packages.yml"))
  msg <- commit_message(dat)
  gert::git_add(c("packages.yml", "bin", "contrib"), repo = path)
  gert::git_commit(msg, repo = path)
}


build_site_dir <- function(path, dest) {
  packages <- yaml::read_yaml(file.path(path, "packages.yml"))
  nms <- basename(vcapply(packages$packages, "[[", "filename"))
  pkgdir <- contrib_url(dest, packages$type, packages$version)
  dir_create(pkgdir)
  file_copy(file.path(path, nms), pkgdir)
  write_packages(pkgdir, packages$type)
}


commit_message <- function(dat) {
  paste(
    c(sprintf("Updated %s", paste(names(dat), collapse = ", ")),
      "",
      sprintf("  * %s %s @ %s",
              names(dat),
              vcapply(dat, "[[", "version"),
              substr(vcapply(dat, "[[", "sha256"), 1, 7))),
    collapse = "\n")
}


update_index <- function(workdir, path) {
  if (file.exists(file.path(path, "packages.yml"))) {
    prev <- yaml::read_yaml(file.path(file.path(path, "packages.yml")))
  } else {
    prev <- NULL
  }

  ## Using the set of packages built as source as our final list
  dat <- yaml::read_yaml(
    file.path(workdir, "sources", "packages", "packages.yml"))
  yml <- c(prev, lapply(unname(dat$packages), function(x)
    x[c("package", "version", "sha256", "ref")]))
  yaml::write_yaml(yml, file.path(path, "packages.yml"))
}


init_repo <- function(path) {
  dir_create(contrib_url(path, "source", NULL))

  platforms <- c("win.binary", "mac.binary", "mac.binary.mavericks",
                 "mac.binary.el-capitan")
  versions <- c("3.1", "3.2", "3.3", "3.4", "3.5", "3.6", "4.0")

  for (platform in platforms) {
    for (version in versions) {
      p <- contrib_url(path, platform, version)
      pp <- file.path(p, "PACKAGES")
      ppz <- paste0(pp, ".gz")
      dir_create(p)
      if (!file.exists(pp)) {
        writeLines(character(0), pp)
      }
      if (!file.exists(ppz)) {
        write_lines_gz(character(0), ppz)
      }
    }
  }

  path
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

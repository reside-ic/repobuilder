update_site <- function(workdir, dest) {
  init_repo(dest)

  ## Look at all the binary directories:
  paths <- c(file.path(workdir, "sources", "packages"),
             dir(workdir, pattern = "^binaries-", full.names = TRUE))

  for (p in paths) {
    packages <- yaml::read_yaml(file.path(p, "packages.yml"))
    nms <- basename(vcapply(packages$packages, "[[", "filename"))
    pkgdir <- contrib_url(dest, packages$type, packages$version)
    dir_create(pkgdir)
    file_copy(file.path(p, nms), pkgdir)
    tools::write_PACKAGES(pkgdir, type = packages$type, verbose = TRUE)
  }

  ## Sort out the final packages lines too.
  if (file.exists(file.path(dest, "packages.yml"))) {
    prev <- yaml::read_yaml(file.path(file.path(dest, "packages.yml")))
  } else {
    prev <- NULL
  }

  ## This has our final say in what was actually done
  dat <- yaml::read_yaml(file.path(workdir, "sources", "src", "packages.yml"))
  yml <- c(prev, lapply(unname(dat), function(x)
    x[c("package", "version", "sha256", "ref")]))
  yaml::write_yaml(yml, file.path(dest, "packages.yml"))

  ## Finally, we'd build a landing page here, but that can wait

  ## Nice commit message
  msg <- c(sprintf("Updated %s", paste(names(dat), collapse = ", ")),
    "",
    sprintf("  * %s %s @ %s",
            names(dat),
            vcapply(dat, "[[", "version"),
            substr(vcapply(dat, "[[", "sha256"), 1, 7)))

  gert::git_add(c("packages.yml", "bin", "contrib"), repo = dest)
  gert::git_commit(paste(msg, collapse = "\n"), repo = dest)
}


contrib_url <- function(path, type, version) {
  if (type == "source") {
    file.path(path, "contrib", "src")
  } else {
    platform <- switch(type,
                       "win.binary" = "windows",
                       "mac.binary" = "macosx",
                       "mac.binary.mavericks" = "macosx/mavericks",
                       "mac.binary.el-capitan" = "macosx/el-capitan")
    file.path(path, "bin", platform, "contrib", version)
  }
}


has_gh_pages <- function(repo = ".") {
  "origin/gh-pages" %in% gert::git_branch_list(repo)$name
}


init_repo <- function(path) {
  ## TODO: Hit metacran or something here to get the current R version
  ## so that this can be bumped.  3.5 should last for the next year or
  ## so though.
  dir_create(contrib_url(path, "source"))

  platforms <- c("windows", "macosx", "macosx/mavericks",
                     "macosx/el-capitan")
  versions <- c("3.1", "3.2", "3.3", "3.4", "3.5", "3.6", "4.0")

  for (platform in platforms) {
    for (version in versions) {
      p <- file.path(path, "bin", platform, "contrib", version)
      pp <- file.path(p, "PACKAGES")
      ppz <- paste0(pp, ".gz")
      dir_create(p)
      if (!file.exists(pp)) {
        writeLines(character(0), pp)
      }
      if (!file.exists(ppz)) {
        writeLines_gz(character(0), ppz)
      }
    }
  }

  path
}


writeLines_gz <- function(text, filename, ...) {
  con <- gzfile(filename)
  on.exit(close(con))
  writeLines(text, con, ...)
}

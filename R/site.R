update_site <- function(workdir, dest) {
  if (has_gh_pages(path)) {
    gert::git_branch_create("gh-pages", "origin/gh-pages", FALSE, path)
    gert::git_clone(path, dest, "gh-pages")
  }
  init_repo(file.path(path, dest))

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

  gert::git_add(dest)
  gert::git_commit(dest)
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

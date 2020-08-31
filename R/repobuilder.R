## There are two strategies for reading the previous index; we might
## look at the version that is on the gh-pages branch, which we can do
## with "git show", or we can download it from the gh-pages
## site. Neither are great given the quality of the tools we can
## easily pull on here.  However, we might do it in the shell commands
## before
fetch_previous_index <- function(path = ".") {
  if (has_gh_pages(path)) {
    gert::git_branch_create("gh-pages", "origin/gh-pages", FALSE, path)
    gert::git_clone(path, "gh-pages", "gh-pages")
    prev <- yaml::read_yaml("gh-pages/packages.yml")
  } else {
    prev <- NULL
  }

  prev
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
  lib <- tempfile()
  dir_create(lib)
  lib <- normalizePath(lib, mustWork = TRUE)

  config <- list(library = lib, dependencies = TRUE)
  refs <- sprintf("local::%s/%s", workdir, vcapply(packages, "[[", "path"))
  proposal <- pkgdepends::new_pkg_installation_proposal(refs, config)

  proposal$solve()
  proposal$stop_for_solution_error()
  proposal$download()
  proposal$stop_for_download_error()
  proposal$install()

  lib
}


download_sources <- function(packages, workdir) {
  ret <- lapply(packages, download_source, workdir)
  names(ret) <- vcapply(ret, "[[", "package")
  if (any(duplicated(names(ret)))) {
    stop("Duplicate package!")
  }
  ret
}


download_source <- function(ref, workdir) {
  dest <- file.path("src", ids::random_id())
  dest_full <- file.path(workdir, dest)
  dir_create(dest_full)
  ref$url <- sprintf("https://github.com/%s/%s", ref$username, ref$repo)
  gert::git_clone(ref$url, dest_full)

  git_ref <- ref$commitish
  if (nzchar(git_ref)) {
    message(sprintf("Checking out ref '%s'", git_ref))
    git_ref_origin <- paste0("origin/", git_ref)
    if (git_ref_origin %in% gert::git_branch_list(dest_full)$name) {
      message(sprintf("Interpreting ref '%s' as branch", git_ref))
      git_ref <- git_ref_origin
    }
    ## branch naming could be alieved if we dropped the .git directory?
    gert::git_branch_create("pkgbuilder/src")
    gert::git_reset("hard", git_ref, repo = src)
  }

  if (nzchar(ref$subdir)) {
    dest <- file.path(dest, ref$subdir)
  }

  sha256 <- gert::git_info(dest_full)$commit

  ## Makes things much smaller!
  unlink(file.path(dest_full, ".git"), recursive = TRUE)

  info <- desc::desc(file = dest_full)
  list(package = info$get_field("Package"),
       version = info$get_field("Version"),
       sha256 = sha256,
       ref = ref$ref,
       path = dest)
}


build_package <- function(path, dest, lib, ..., vignettes = FALSE) {
  dir_create(dest)
  withr::with_libpaths(
    lib,
    basename(pkgbuild::build(path, dest, ..., vignettes = vignettes)))
}


check_prev <- function(prev, packages) {
  prev <- data.frame(
    package = vcapply(prev, "[[", "package"),
    version = numeric_version(vcapply(prev, "[[", "version")),
    sha256 = vcapply(prev, "[[", "sha256"),
    stringsAsFactors = FALSE)
  prev <- prev[order(prev$package, prev$version), ]
  rownames(prev) <- NULL

  for (p in names(packages)) {
    prev_p <- prev[prev$package == p, ]
    if (nrow(prev_p) == 0L) {
      prev_version <- numeric_version("0.0.0")
      prev_sha256 <- NA_character_
    } else {
      prev_version <- prev_p$version[nrow(prev_p)]
      prev_sha256 <- prev_p$sha256[nrow(prev_p)]
    }

    curr_version <- package_version(packages[[p]]$version)
    packages[[p]]$build <- curr_version > prev_version
    if (curr_version < prev_version) {
      stop(sprintf("Version of '%s' has decreased from '%s' to '%s'",
                   p, prev_version, curr_version))
    }
    if (curr_version == prev_version && prev_sha256 != packages[[p]]$sha256) {
      stop(sprintf(
        "Version of '%s' has not changed, but source has (%s => %s)",
        p, prev_sha256, packages[[p]]$sha256))
    }
  }

  packages
}


build_packages <- function(packages, lib, binary, workdir, dest = "packages") {
  for (i in seq_along(packages)) {
    p <- packages[[i]]
    if (p$build) {
      packages[[i]]$filename <- withr::with_dir(
        workdir,
        build_package(p$path, dest, lib, binary = binary))
    }
  }

  dat <- list(type = package_type(binary),
              version = r_version2(),
              packages = packages)
  yaml::write_yaml(dat, file.path(workdir, dest, "packages.yml"))
  invisible(dest)
}

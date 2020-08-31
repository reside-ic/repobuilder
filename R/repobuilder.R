## There are two strategies for reading the previous index; we might
## look at the version that is on the gh-pages branch, which we can do
## with "git show", or we can download it from the gh-pages
## site. Neither are great given the quality of the tools we can
## easily pull on here.  However, we might do it in the shell commands
## before
fetch_previous_index <- function(repo = ".") {
  ## if (!("origin/gh-pages" %in% gert::git_branch_list(repo)$name)) {
  ##   return(NULL)
  ## }

  ## ## TODO: this can fail for lots of reasons, none fun to catch and
  ## ## some are fine as errors.
  ## prev <- suppressWarnings(
  ##   system2("git", c("show", "origin/gh-pages:repobuilder.yml"),
  ##           stdout = TRUE, stderr = TRUE))
  ## code <- attr(prev, "status")
  ## if (code != 0) {
  ##   return(NULL)
  ## }

  ## yaml::yaml.load(prev)
  ## Just for now
  return(NULL)
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
    pkgbuild::build(path, dest, ..., vignettes = vignettes))
}


check_prev <- function(prev, packages) {
  for (p in names(packages)) {
    prev_version <- package_version(prev[[p]] %||% "0.0.0")
    curr_version <- package_version(packages[[p]]$version)
    packages[[p]]$build <- curr_version > prev_version
    if (curr_version < prev_version) {
      stop(sprintf("Version of '%s' has decreased from '%s' to '%s'",
                   p, prev_version, curr_version))
    }
    if (curr_version == prev_version && prev[[p]]$sha256 != curr[[p]]) {
      ## Perhaps too strict?
      stop(sprintf(
        "Version of '%s' has not changed, but source has (%s => %s)",
        p, prev[[p]]$sha256, curr[[p]]$sha256))
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

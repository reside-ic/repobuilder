context("integration")

test_that("can build a local source repo", {
  skip("long running test")
  filename <- "repobuilder.yml"
  workdir <- tempfile()
  prev <- NULL

  res_src <- rb_build_source(filename, workdir, prev)

  expect_setequal(
    dir(workdir),
    c("src", "packages"))

  expect_equal(readLines(file.path(workdir, "src", "build")), "TRUE")
  expect_true("packages.yml" %in% dir(file.path(workdir, "src")))

  expect_true("packages.yml" %in% dir(file.path(workdir, "packages")))
  pkgs_src <- yaml::read_yaml(file.path(workdir, "packages", "packages.yml"))
  expect_equal(pkgs_src$type, "source")
  expect_equal(pkgs_src$version, r_version2())
  expect_setequal(names(pkgs_src$packages), c("ring", "dde"))

  files <- vcapply(pkgs_src$packages, "[[", "filename")
  expect_match(files, "\\.tar\\.gz$", all = TRUE)
  expect_true(all(file.exists(file.path(workdir, "packages", files))))

  paths <- vcapply(pkgs_src$packages, "[[", "path")
  expect_true(all(file.exists(file.path(workdir, paths))))

  dir_create(file.path(workdir, "sources"))
  file.copy(file.path(workdir, "packages"),
            file.path(workdir, "sources"),
            recursive = TRUE)

  ## This is nasty:
  if (is_mac()) {
    expected <- list(type = "mac.binary", ext = ".tgz")
  } else if (is_windows()) {
    expected <- list(type = "win.binary", ext = ".zip")
  } else {
    testthat::skip("Binaries not supported")
  }

  res_bin <- rb_build_binaries(workdir)
  pkgs_bin <- yaml::read_yaml(file.path(workdir, "packages", "packages.yml"))
  expect_equal(res_bin, pkgs_bin)

  expect_equal(pkgs_bin$type, expected$type)
  expect_equal(pkgs_bin$version, r_version2())
  expect_setequal(names(pkgs_bin$packages), c("ring", "dde"))

  files <- vcapply(pkgs_bin$packages, "[[", "filename")
  expect_match(files, sprintf("\\%s$", expected$ext), all = TRUE)
  expect_true(all(file.exists(file.path(workdir, "packages", files))))

  file.rename(file.path(workdir, "packages"),
              file.path(workdir, "binaries-platform-version"))

  dest <- tempfile()
  rb_build_site(workdir, dest)

  expect_setequal(dir(dest), c("bin", "contrib", "packages.yml"))

  pkgs_index <- yaml::read_yaml(file.path(dest, "packages.yml"))
  expect_equal(
    pkgs_index,
    lapply(unname(pkgs_src$packages), function(x)
      x[c("package", "version", "sha256", "ref")]))

  files <- sprintf("%s_%s.tar.gz",
                   vcapply(pkgs_index, function(x) x$package),
                   vcapply(pkgs_index, function(x) x$version))
  expect_true(all(file.exists(
    file.path(contrib_url(dest, "source", NULL), files))))
})

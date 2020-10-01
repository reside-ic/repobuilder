context("local")

test_that("short circuit build if no updated packages", {
  mock_build_packages <- mockery::mock()
  mock_prepare_library <- mockery::mock()
  mock_read_yaml <- mockery::mock(
    list(a = list(package = "a", version = "1.2.3", build = FALSE)),
    NULL)
  workdir <- tempfile()

  with_mock(
    "yaml::read_yaml" = mock_read_yaml,
    "repobuilder:::build_packages" = mock_build_packages,
    "repobuilder:::prepare_library" = mock_prepare_library, {
      expect_message(rb_build_binaries(workdir), "Nothing to update!")
      expect_message(rb_build_binaries(workdir), "Nothing to update!")
    })

  mockery::expect_called(mock_prepare_library, 0)
  mockery::expect_called(mock_build_packages, 0)
})


test_that("short circuit build if no updated packages", {
  dat <- list(a = list(package = "a", version = "1.2.3", build = TRUE))
  lib <- tempfile()

  mock_build_packages <- mockery::mock()
  mock_prepare_library <- mockery::mock(lib)
  mock_read_yaml <- mockery::mock(dat)
  workdir <- tempfile()

  with_mock(
    "yaml::read_yaml" = mock_read_yaml,
    "repobuilder:::build_packages" = mock_build_packages,
    "repobuilder:::prepare_library" = mock_prepare_library,
    rb_build_binaries(workdir))

  mockery::expect_called(mock_prepare_library, 1)
  expect_equal(
    mockery::mock_args(mock_prepare_library)[[1]],
    list(dat, workdir))

  mockery::expect_called(mock_build_packages, 1)
  expect_equal(
    mockery::mock_args(mock_build_packages)[[1]],
    list(dat, lib, TRUE, workdir))
})


test_that("build source short circuit", {
  prev <- list(list(package = "a", version = "1.2.3", sha256 = "abc"))
  dat <- list(a = list(package = "a", version = "1.2.3", sha256 = "abc"))
  workdir <- tempfile()
  config <- list(packages = list("user/a" = NULL))

  mock_download_sources <- mockery::mock(dat)
  mock_build_packages <- mockery::mock()
  mock_prepare_library <- mockery::mock()

  with_mock(
    "repobuilder:::download_sources" = mock_download_sources,
    "repobuilder:::build_packages" = mock_build_packages,
    "repobuilder:::prepare_library" = mock_prepare_library,
    expect_message(rb_build_source(config, workdir, prev),
                   "Nothing to update!"))

  expect_equal(readLines(file.path(workdir, "src", "build")), "FALSE")
  expect_equal(
    yaml::read_yaml(file.path(workdir, "src", "packages.yml")),
    list(a = c(dat$a, list(build = FALSE))))

  mockery::expect_called(mock_prepare_library, 0)
  mockery::expect_called(mock_build_packages, 0)
})


test_that("build source build", {
  prev <- list(list(package = "a", version = "1.2.1", sha256 = "def"))
  dat <- list(a = list(package = "a", version = "1.2.3", sha256 = "abc"))
  workdir <- tempfile()
  config <- list(packages = list("user/a" = NULL))
  lib <- tempfile()

  mock_download_sources <- mockery::mock(dat)
  mock_build_packages <- mockery::mock()
  mock_prepare_library <- mockery::mock(lib)

  with_mock(
    "repobuilder:::download_sources" = mock_download_sources,
    "repobuilder:::build_packages" = mock_build_packages,
    "repobuilder:::prepare_library" = mock_prepare_library,
    rb_build_source(config, workdir, prev))

  expected <- list(a = c(dat$a, list(build = TRUE)))

  expect_equal(readLines(file.path(workdir, "src", "build")), "TRUE")
  expect_equal(
    yaml::read_yaml(file.path(workdir, "src", "packages.yml")),
    expected)

  mockery::expect_called(mock_prepare_library, 1)
  expect_equal(
    mockery::mock_args(mock_prepare_library)[[1]],
    list(expected, workdir))

  mockery::expect_called(mock_build_packages, 1)
  expect_equal(
    mockery::mock_args(mock_build_packages)[[1]],
    list(expected, lib, FALSE, workdir))
})

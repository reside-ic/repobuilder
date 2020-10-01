context("packages")


test_that("validating packages against empty set", {
  packages <- list(
    a = list(version = "1.2.3", sha256 = "abc"),
    b = list(version = "0.0.1", sha256 = "123"))
  expected <- lapply(packages, function(x) c(x, list(build = TRUE)))
  expect_equal(check_packages_version(packages, NULL), expected)
})


test_that("validating packages against out of date set set", {
  packages <- list(
    a = list(version = "1.2.3", sha256 = "abc"),
    b = list(version = "0.0.2", sha256 = "123"))
  prev <- list(list(package = "a",
                    version = "1.2.2",
                    sha256 = "def"),
               list(package = "a",
                    version = "1.2.1",
                    sha256 = "789"),
               list(package = "b",
                    version = "0.0.1",
                    sha256 = "456"),
               list(package = "c",
                    version = "2.3.4",
                    sha256 = "741"))

  expected <- lapply(packages, function(x) c(x, list(build = TRUE)))
  expect_equal(check_packages_version(packages, prev), expected)
})


test_that("validating packages when one updated", {
  packages <- list(
    a = list(version = "1.2.3", sha256 = "abc"),
    b = list(version = "0.0.2", sha256 = "123"))
  prev <- list(list(package = "a",
                    version = "1.2.3",
                    sha256 = "abc"),
               list(package = "a",
                    version = "1.2.1",
                    sha256 = "789"),
               list(package = "b",
                    version = "0.0.1",
                    sha256 = "456"),
               list(package = "c",
                    version = "2.3.4",
                    sha256 = "741"))

  res <- check_packages_version(packages, prev)
  expect_equal(res$a, c(packages$a, list(build = FALSE)))
  expect_equal(res$b, c(packages$b, list(build = TRUE)))
})


test_that("error for decreased version number", {
  packages <- list(
    a = list(version = "1.1.3", sha256 = "abc"),
    b = list(version = "0.0.2", sha256 = "123"))
  prev <- list(list(package = "a",
                    version = "1.2.3",
                    sha256 = "def"),
               list(package = "a",
                    version = "1.2.1",
                    sha256 = "789"),
               list(package = "b",
                    version = "0.0.1",
                    sha256 = "456"),
               list(package = "c",
                    version = "2.3.4",
                    sha256 = "741"))

  expect_error(
    check_packages_version(packages, prev),
    "Version of 'a' has decreased from '1.2.3' to '1.1.3'",
    fixed = TRUE)
})


test_that("error for source change and no increase", {
  packages <- list(
    a = list(version = "1.2.3", sha256 = "abc"),
    b = list(version = "0.0.2", sha256 = "123"))
  prev <- list(list(package = "a",
                    version = "1.2.3",
                    sha256 = "def"),
               list(package = "a",
                    version = "1.2.1",
                    sha256 = "789"),
               list(package = "b",
                    version = "0.0.1",
                    sha256 = "456"),
               list(package = "c",
                    version = "2.3.4",
                    sha256 = "741"))

  expect_error(
    check_packages_version(packages, prev),
    "Version of 'a' has not changed, but source has (def => abc)",
    fixed = TRUE)
})


test_that("fetch_previous_index returns NULL if no gh-pages branch", {
  mock_has_gh_pages <- mockery::mock(FALSE)
  mock_git_run <- mockery::mock()
  path <- tempfile()
  res <- with_mock(
    "repobuilder:::has_gh_pages" = mock_has_gh_pages,
    "repobuilder:::git_run" = mock_git_run,
    fetch_previous_index(path))
  expect_null(res)

  mockery::expect_called(mock_has_gh_pages, 1)
  expect_equal(mockery::mock_args(mock_has_gh_pages)[[1]], list(path))

  mockery::expect_called(mock_git_run, 0)
})


test_that("fetch_previous_index returns NULL if file not found", {
  mock_has_gh_pages <- mockery::mock(TRUE)
  mock_git_run <- mockery::mock(
    list(status = 128L,
         stdout = "",
         stderr = "fatal: Invalid object name 'origin/gh-pages'.\n",
         timeout = FALSE))
  path <- tempfile()
  res <- with_mock(
    "repobuilder:::has_gh_pages" = mock_has_gh_pages,
    "repobuilder:::git_run" = mock_git_run,
    fetch_previous_index(path))
  expect_null(res)

  mockery::expect_called(mock_has_gh_pages, 1)
  expect_equal(mockery::mock_args(mock_has_gh_pages)[[1]], list(path))

  mockery::expect_called(mock_git_run, 1)
  expect_equal(mockery::mock_args(mock_git_run)[[1]],
               list(c("show", "origin/gh-pages:packages.yml"), path, FALSE))
})


test_that("fetch_previous_index returns index if found", {
  mock_has_gh_pages <- mockery::mock(TRUE)
  contents <- c(
    "- package: a",
    "  version: 1.2.3",
    "  sha256: abc",
    "  ref: user/a",
    "- package: b",
    "  version: 2.3.4",
    "  sha256: def",
    "  ref: user/b")

  mock_git_run <- mockery::mock(
    list(status = 0L,
         stdout = paste(contents, collapse = "\n"),
         stderr = "",
         timeout = FALSE))
  path <- tempfile()
  res <- with_mock(
    "repobuilder:::has_gh_pages" = mock_has_gh_pages,
    "repobuilder:::git_run" = mock_git_run,
    fetch_previous_index(path))

  expect_equal(res, yaml::yaml.load(paste(contents, collapse = "\n")))

  mockery::expect_called(mock_has_gh_pages, 1)
  expect_equal(mockery::mock_args(mock_has_gh_pages)[[1]], list(path))

  mockery::expect_called(mock_git_run, 1)
  expect_equal(mockery::mock_args(mock_git_run)[[1]],
               list(c("show", "origin/gh-pages:packages.yml"), path, FALSE))
})

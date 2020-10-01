context("utils")

test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("package_type returns sensible types", {
  expect_equal(package_type(FALSE, NULL), "source")
  expect_equal(package_type(TRUE, "windows"), "win.binary")
  expect_equal(package_type(TRUE, "darwin"), "mac.binary")
  expect_equal(package_type(TRUE, "linux"), "source")
})


test_that("has_gh_pages works", {
  mock_branch_list <- mockery::mock(
    list(name = c("master", "feature", "origin/master", "origin/feature")),
    list(name = c("origin/master", "origin/feature", "origin/gh-pages")))
  path <- tempfile()

  with_mock("gert::git_branch_list" = mock_branch_list, {
    expect_false(has_gh_pages(path))
    expect_true(has_gh_pages(path))
  })

  mockery::expect_called(mock_branch_list, 2)
  expect_equal(mockery::mock_args(mock_branch_list),
               list(list(path), list(path)))
})


test_that("git_run calls git with expected arguments", {
  mock_processx_run <- mockery::mock()
  path <- tempfile()
  with_mock("processx::run" = mock_processx_run, {
    git_run(c("a", "b", "c"), path, TRUE)
    git_run(c("a", "b", "c"), path, FALSE)
  })

  mockery::expect_called(mock_processx_run, 2)
  expect_equal(
    mockery::mock_args(mock_processx_run)[[1]],
    list("git", c("-C", path, "a", "b", "c"), error_on_status = TRUE))
  expect_equal(
    mockery::mock_args(mock_processx_run)[[2]],
    list("git", c("-C", path, "a", "b", "c"), error_on_status = FALSE))
})


test_that("file_copy errors for impossible copy", {
  expect_error(file_copy(tempfile(), tempfile()),
               "Error copying files")
})


test_that("file_copy does copy files", {
  a <- tempfile()
  b <- tempfile()
  writeLines(letters, a)
  file_copy(a, b)
  expect_equal(readLines(b), letters)

  writeLines(LETTERS, a)
  file_copy(a, b)
  expect_equal(readLines(b), LETTERS)
})


test_that("write_packages builds package index", {
  pkg <- tempfile()
  dir_create(pkg)
  writeLines(c(
    "Package: tmp",
    "Version: 0.0.1",
    "Author: An Author"),
    file.path(pkg, "DESCRIPTION"))
  zip <- pkgbuild::build(pkg, quiet = TRUE)

  path <- tempfile()
  dir_create(path)
  file_copy(zip, path)

  write_packages(path, "source")
  m <- read.dcf(file.path(path, "PACKAGES"))
  expect_equal(unname(m[, "Package"]), "tmp")
  expect_equal(unname(m[, "Version"]), "0.0.1")
})

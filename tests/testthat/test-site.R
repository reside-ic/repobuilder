context("site")

test_that("Can create empty repo", {
  path <- tempfile()
  expect_identical(init_repo(path), path)
  expect_setequal(dir(path), c("bin", "contrib"))
  expect_setequal(dir(file.path(path, "contrib")), "src")
  expect_setequal(dir(file.path(path, "contrib", "src")), character(0))

  ## pick a random binary path:
  p <- file.path(path, "bin", "windows", "contrib", "4.0")
  expect_setequal(dir(p), c("PACKAGES", "PACKAGES.gz"))
  expect_equal(file.size(file.path(p, "PACKAGES")), 0)
  expect_gt(file.size(file.path(p, "PACKAGES.gz")), 0)
  expect_equal(readLines(file.path(p, "PACKAGES.gz")), character(0))
})


test_that("Do not overwrite index files with contents", {
  path <- tempfile()
  init_repo(path)
  p <- file.path(path, "bin", "windows", "contrib", "4.0")
  contents <- letters
  writeLines(contents, file.path(p, "PACKAGES"))
  write_lines_gz(contents, file.path(p, "PACKAGES.gz"))

  init_repo(path)
  expect_equal(readLines(file.path(p, "PACKAGES")), contents)
  expect_equal(readLines(file.path(p, "PACKAGES.gz")), contents)
})


test_that("Can update site", {
  path <- tempfile()
  workdir <- tempfile()
  dat <- list(a = list(version = "1.2.3", sha256 = "abc"),
              b = list(version = "2.3.4", sha256 = "def"))
  msg <- commit_message(dat)

  mock_read_yaml <- mockery::mock(dat)
  mock_git_add <- mockery::mock()
  mock_git_commit <- mockery::mock()
  with_mock(
    "yaml::read_yaml" = mock_read_yaml,
    "gert::git_add" = mock_git_add,
    "gert::git_commit" = mock_git_commit,
    rb_update_site(workdir, path))

  mockery::expect_called(mock_read_yaml, 1)
  expect_equal(
    mockery::mock_args(mock_read_yaml)[[1]],
    list(file.path(workdir, "sources", "src", "packages.yml")))

  mockery::expect_called(mock_git_add, 1)
  expect_equal(
    mockery::mock_args(mock_git_add)[[1]],
    list(c("packages.yml", "bin", "contrib"), repo = path))

  mockery::expect_called(mock_git_commit, 1)
  expect_equal(
    mockery::mock_args(mock_git_commit)[[1]],
    list(msg, repo = path))
})


test_that("construct sensible commit message", {
  dat <- list(a = list(version = "1.2.3", sha256 = "abc"),
              b = list(version = "2.3.4", sha256 = "def"))
  expect_equal(
    commit_message(dat[1]),
    paste(
      "Updated a",
      "",
      "  * a 1.2.3 @ abc",
      sep = "\n"))

  expect_equal(
    commit_message(dat),
    paste(
      "Updated a, b",
      "",
      "  * a 1.2.3 @ abc",
      "  * b 2.3.4 @ def",
      sep = "\n"))
})


test_that("can update index", {
  dat1 <- list(
    a = list(package = "a", version = "1.2.3", sha256 = "abc", ref = "user/a"),
    b = list(package = "b", version = "2.3.4", sha256 = "def", ref = "user/b"))
  dat2 <- list(
    a = list(package = "a", version = "1.2.4", sha256 = "ghi", ref = "user/a"),
    c = list(package = "c", version = "0.0.1", sha256 = "jkl", ref = "user/c"))

  dest <- tempfile()
  dir_create(dest)
  path_index <- file.path(dest, "packages.yml")

  workdir <- tempfile()
  path_packages <- file.path(workdir, "sources", "src", "packages.yml")
  dir_create(dirname(path_packages))

  yaml::write_yaml(dat1, path_packages)
  update_index(workdir, dest)

  expect_equal(
    yaml::read_yaml(path_index),
    unname(dat1))

  yaml::write_yaml(dat2, path_packages)
  update_index(workdir, dest)

  expect_equal(
    yaml::read_yaml(path_index),
    c(unname(dat1), unname(dat2)))
})

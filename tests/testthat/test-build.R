context("build")

test_that("can prepare library", {
  mock_proposal <- list(
    solve = mockery::mock(),
    stop_for_solution_error = mockery::mock(),
    download = mockery::mock(),
    stop_for_download_error = mockery::mock(),
    install = mockery::mock())
  mock_new_proposal <- mockery::mock(mock_proposal)

  packages <- list(
    list(path = "a"),
    list(path = "b"))
  workdir <- tempfile()

  lib <- with_mock(
    "pkgdepends::new_pkg_installation_proposal" = mock_new_proposal,
    prepare_library(packages, workdir))

  expect_true(file.exists(lib))
  mockery::expect_called(mock_new_proposal, 1)
  args <- mockery::mock_args(mock_new_proposal)[[1]]

  expect_equal(args[[1]], sprintf("local::%s/%s", workdir, c("a", "b")))
  expect_equal(args[[2]], list(library = lib, dependencies = TRUE))

  ## NOTE: order here not checked but is trivial
  mockery::expect_called(mock_proposal$solve, 1)
  mockery::expect_called(mock_proposal$stop_for_solution_error, 1)
  mockery::expect_called(mock_proposal$download, 1)
  mockery::expect_called(mock_proposal$stop_for_download_error, 1)
  mockery::expect_called(mock_proposal$install, 1)
})


test_that("Can download repo", {
  sha256 <- ids::random_id()
  path <- ids::random_id()

  mock_random_id <- mockery::mock(path)
  mock_git_clone <- mockery::mock()
  mock_git_info <- mockery::mock(list(commit = sha256))
  mock_git_run <- mockery::mock()
  mock_desc <- mockery::mock(
    list(get_field = function(name)
      switch(name,
             Package = "packagename",
             Version = "1.2.3",
             stop("Do not call"))))

  ref <- pkgdepends::parse_pkg_ref("user/repo")
  workdir <- tempfile()

  res <- with_mock(
    "ids::random_id" = mock_random_id,
    "gert::git_clone" = mock_git_clone,
    "gert::git_info" = mock_git_info,
    "repobuilder:::git_run" = mock_git_run,
    "desc::desc" = mock_desc,
    download_source(ref, workdir))

  expect_equal(
    res,
    list(package = "packagename",
         version = "1.2.3",
         sha256 = sha256,
         ref = "user/repo",
         path = file.path("src", path)))

  mockery::expect_called(mock_random_id, 1)

  mockery::expect_called(mock_git_clone, 1)
  expect_equal(
    mockery::mock_args(mock_git_clone)[[1]],
    list("https://github.com/user/repo",
         file.path(workdir, "src", path)))

  mockery::expect_called(mock_git_run, 0)

  mockery::expect_called(mock_git_info, 1)
  expect_equal(
    mockery::mock_args(mock_git_info)[[1]],
    list(file.path(workdir, "src", path)))

  mockery::expect_called(mock_desc, 1)
  expect_equal(
    mockery::mock_args(mock_desc)[[1]],
    list(file = file.path(workdir, "src", path)))
})


test_that("Can set subdirectory", {
  sha256 <- ids::random_id()
  path <- ids::random_id()

  mock_random_id <- mockery::mock(path)
  mock_git_clone <- mockery::mock()
  mock_git_info <- mockery::mock(list(commit = sha256))
  mock_desc <- mockery::mock(
    list(get_field = function(name)
      switch(name,
             Package = "packagename",
             Version = "1.2.3",
             stop("Do not call"))))

  ref <- pkgdepends::parse_pkg_ref("user/repo/subdir")
  workdir <- tempfile()

  res <- with_mock(
    "ids::random_id" = mock_random_id,
    "gert::git_clone" = mock_git_clone,
    "gert::git_info" = mock_git_info,
    "desc::desc" = mock_desc,
    download_source(ref, workdir))

  expect_equal(
    res,
    list(package = "packagename",
         version = "1.2.3",
         sha256 = sha256,
         ref = "user/repo/subdir",
         path = file.path("src", path, "subdir")))
})


test_that("Can set git reference", {
  sha256 <- ids::random_id()
  path <- ids::random_id()

  mock_random_id <- mockery::mock(path)
  mock_git_clone <- mockery::mock()
  mock_git_info <- mockery::mock(list(commit = sha256))
  mock_git_run <- mockery::mock()
  mock_desc <- mockery::mock(
    list(get_field = function(name)
      switch(name,
             Package = "packagename",
             Version = "1.2.3",
             stop("Do not call"))))

  ref <- pkgdepends::parse_pkg_ref("user/repo@someref")
  workdir <- tempfile()

  res <- with_mock(
    "ids::random_id" = mock_random_id,
    "gert::git_clone" = mock_git_clone,
    "gert::git_info" = mock_git_info,
    "repobuilder:::git_run" = mock_git_run,
    "desc::desc" = mock_desc,
    download_source(ref, workdir))

  expect_equal(
    res,
    list(package = "packagename",
         version = "1.2.3",
         sha256 = sha256,
         ref = "user/repo@someref",
         path = file.path("src", path)))

  mockery::expect_called(mock_random_id, 1)

  mockery::expect_called(mock_git_clone, 1)
  expect_equal(
    mockery::mock_args(mock_git_clone)[[1]],
    list("https://github.com/user/repo",
         file.path(workdir, "src", path)))

  mockery::expect_called(mock_git_run, 1)
  expect_equal(
    mockery::mock_args(mock_git_run)[[1]],
    list(c("checkout", "someref", "--"), file.path(workdir, "src", path)))

  mockery::expect_called(mock_git_info, 1)
  expect_equal(
    mockery::mock_args(mock_git_info)[[1]],
    list(file.path(workdir, "src", path)))

  mockery::expect_called(mock_desc, 1)
  expect_equal(
    mockery::mock_args(mock_desc)[[1]],
    list(file = file.path(workdir, "src", path)))
})


test_that("build_package sets .libPaths()", {
  mock_build <- mockery::mock(.libPaths())

  path <- tempfile()
  dest <- tempfile()
  lib <- tempfile()
  dir_create(lib)
  on.exit(unlink(lib, recursive = TRUE))

  res <- with_mock(
    "pkgbuild::build" = mock_build,
    build_package(path, dest, lib))

  expect_true(same_path(res[[1]], lib))
  mockery::expect_called(mock_build, 1)

  expect_equal(mockery::mock_args(mock_build)[[1]],
               list(path, dest, vignettes = FALSE))
})


test_that("build_packages updates package names", {
  packages <- list(
    a = list(
      build = TRUE,
      path = tempfile()),
    b = list(
      build = FALSE,
      path = tempfile()))

  workdir <- tempfile()
  lib <- tempfile()
  binary <- FALSE

  dir_create(workdir)
  dir_create(lib)

  mock_build_package <- mockery::mock("path/to/a_0.1.2.tar.gz")
  res <- with_mock(
    "repobuilder:::build_package" = mock_build_package,
    build_packages(packages, lib, binary, workdir))

  expect_equal(res$type, "source")
  expect_equal(res$version, r_version2())
  expect_equal(res$packages$a, c(packages[[1]], filename = "a_0.1.2.tar.gz"))
  expect_equal(res$packages$b, packages[[2]])

  mockery::expect_called(mock_build_package, 1)
  expect_equal(
    mockery::mock_args(mock_build_package)[[1]],
    list(packages[[1]]$path, "packages", lib, binary = binary))
})


test_that("download_sources checks for duplicate package names", {
  packages <- list("user/repo1" = NULL,
                   "user/repo2" = NULL)
  workdir <- tempfile()
  mock_download_source <- mockery::mock(list(package = "a"),
                                        list(package = "a"))

  with_mock(
    "repobuilder:::download_source" = mock_download_source,
    expect_error(
      download_sources(packages, workdir),
      "Duplicate package 'a'"))
})


test_that("download_sources checks for duplicate package names", {
  packages <- list("user/repo1" = NULL,
                   "user/repo2" = NULL)
  workdir <- tempfile()
  mock_download_source <- mockery::mock(list(package = "repo1"),
                                        list(package = "repo2"))

  res <- with_mock(
    "repobuilder:::download_source" = mock_download_source,
    download_sources(packages, workdir))

  expect_equal(res,
               list(repo1 = list(package = "repo1"),
                    repo2 = list(package = "repo2")))
})

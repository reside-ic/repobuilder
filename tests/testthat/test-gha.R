context("gha")

test_that("gha_source fetches index and builds source", {
  config <- list(packages = list("user/repo" = NULL))

  mock_fetch_previous_index <- mockery::mock(NULL)
  mock_build_source <- mockery::mock()
  mock_read_config <- mockery::mock(config)

  with_mock(
    "repobuilder:::read_config" = mock_read_config,
    "repobuilder:::fetch_previous_index" = mock_fetch_previous_index,
    "repobuilder:::rb_build_source" = mock_build_source,
    gha_source())

  mockery::expect_called(mock_read_config, 1)
  expect_equal(
    mockery::mock_args(mock_read_config)[[1]],
    list("repobuilder.yml"))

  mockery::expect_called(mock_fetch_previous_index, 1)
  expect_equal(
    mockery::mock_args(mock_fetch_previous_index)[[1]],
    list("."))

  mockery::expect_called(mock_build_source, 1)
  expect_equal(
    mockery::mock_args(mock_build_source)[[1]],
    list(config, "gha", NULL))
})


test_that("gha_binaries runs in gha", {
  mock_build_binaries <- mockery::mock()
  with_mock(
    "repobuilder:::rb_build_binaries" = mock_build_binaries,
    gha_binaries())

  mockery::expect_called(mock_build_binaries, 1)
  expect_equal(
    mockery::mock_args(mock_build_binaries)[[1]],
    list("gha"))
})


test_that("gha_site runs in gha", {
  mock_gh_pages_prep <- mockery::mock()
  mock_build_site <- mockery::mock()
  mock_update_site <- mockery::mock()

  with_mock(
    "repobuilder:::gh_pages_prep" = mock_gh_pages_prep,
    "repobuilder:::rb_build_site" = mock_build_site,
    "repobuilder:::rb_update_site" = mock_update_site,
    gha_site())

  mockery::expect_called(mock_gh_pages_prep, 1)
  expect_equal(
    mockery::mock_args(mock_gh_pages_prep)[[1]],
    list("."))

  mockery::expect_called(mock_build_site, 1)
  expect_equal(
    mockery::mock_args(mock_build_site)[[1]],
    list("gha", "."))

  mockery::expect_called(mock_update_site, 1)
  expect_equal(
    mockery::mock_args(mock_update_site)[[1]],
    list("gha", "."))
})

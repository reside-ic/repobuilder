context("repobuilder")

test_that("config must contain a packages section", {
  expect_error(
    check_config(list()),
    "Expected a section 'packages' in repobuilder.yml")
  expect_error(
    check_config(list(packages = NULL)),
    "Expected a section 'packages' in repobuilder.yml")
})


test_that("packages section must be named and unique", {
  expect_error(
    check_config(list(packages = list("a/b", "c/d"))),
    "'repobuilder.yml:packages' must be named")
  expect_error(
    check_config(list(packages = list("a/b" = NULL, "a/b" = NULL))),
    "'repobuilder.yml:packages' must have unique names")
})


test_that("packages entries must be empty", {
  expect_error(
    check_config(list(packages = list("a/b" = list(build = FALSE)))),
    "Options not supported (for repobuilder.yml:packages:a/b)",
    fixed = TRUE)
})


test_that("packages entries must be github refs", {
  expect_error(
    check_config(list(packages = list("a/b" = NULL, "local::path" = NULL))),
    "Only GitHub references supported (for repobuilder.yml:packages:local::",
    fixed = TRUE)
})


test_that("config gets repo spec parsed", {
  dat <- check_config(list(packages = list("user/repo" = NULL)))
  expect_equal(dat$packages[[1]]$username, "user")
  expect_equal(dat$packages[[1]]$repo, "repo")
  expect_equal(dat$packages[[1]]$ref, "user/repo")
  expect_equal(dat$packages[[1]]$type, "github")
})


test_that("read_config", {
  expect_identical(
    read_config("repobuilder.yml"),
    check_config(yaml::read_yaml("repobuilder.yml")))
})

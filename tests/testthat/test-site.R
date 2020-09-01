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

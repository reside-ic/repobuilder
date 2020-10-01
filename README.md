## repobuilder

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/reside-ic/repobuilder/workflows/R-CMD-check/badge.svg)](https://github.com/reside-ic/repobuilder/actions)
[![codecov.io](https://codecov.io/github/reside-ic/repobuilder/coverage.svg?branch=master)](https://codecov.io/github/reside-ic/repobuilder?branch=master)
[![CodeFactor](https://www.codefactor.io/repository/github/reside-ic/repobuilder/badge)](https://www.codefactor.io/repository/github/reside-ic/repobuilder)
<!-- badges: end -->

This package makes it straightforward to maintain a CRAN-like repository of R packages that can consistently be installed against each other. It is considerably smaller in scope than related work by r-hub and rOpenSci.

To configure, we create a file at the root of the repository, on the default branch, called `repobuilder.yml` containing `remote`-style ref-specs

```
packages:
  mrc-ide/ring: ~
  mrc-ide/dde: ~
```

The `gh-pages` branch of this repository will end up containing the actual repository.

### Principles

* Versioned files are not replaced. Once you have pushed version `x.y.z` of a package, that version will not be replaced *even if the source package has been updated*. Increase your version number you animal.
* Supports source, windows and mac binaries for the current and previous R release
* Packages are a *consistent set*; meaning that dependencies are resolved across the full set of your packages
* No testing is done; this works best if the package references point at a branch already protected by a PR-style workflow

## Installation

```r
remotes::install_github("reside-ic/repobuilder")
```

But you'd be better off using it via GitHub actions

## License

MIT © Imperial College of Science, Technology and Medicine

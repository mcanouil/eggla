
<!-- README.md is generated from README.Rmd. Please edit that file -->

# EGG Longitudinal Analysis <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![GitHub
tag](https://img.shields.io/github/tag/mcanouil/eggla.svg?label=latest%20tag&include_prereleases)](https://github.com/mcanouil/eggla)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6979433.svg)](https://doi.org/10.5281/zenodo.6979433)  
[![codecov](https://codecov.io/gh/mcanouil/eggla/branch/main/graph/badge.svg?token=D0R3SAZTWA)](https://codecov.io/gh/mcanouil/eggla)
[![R-CMD-check](https://github.com/mcanouil/eggla/actions/workflows/check.yml/badge.svg)](https://github.com/mcanouil/eggla/actions/workflows/check.yml)
<!-- badges: end -->

Tools for longitudinal analysis within the [EGG (Early Growth Genetics)
Consortium](http://egg-consortium.org/).

[![Open in GitHub Codespaces](https://github.com/codespaces/badge.svg)](https://codespaces.new/mcanouil/eggla?quickstart=1&devcontainer_path=..devcontainer%2Feggla-latest%2Fdevcontainer.json)

## Installation

- Install [`pak`](https://pak.r-lib.org/)

  ``` r
  temp_library <- file.path(".", "R")
  dir.create(temp_library, recursive = TRUE)
  .libPaths(temp_library)
  utils::install.packages(
    pkgs = "pak",
    lib = temp_library,
    repos = sprintf(
      "https://r-lib.github.io/p/pak/stable/%s/%s/%s",
      .Platform[["pkgType"]], R.Version()[["os"]], R.Version()[["arch"]]
    )
  )
  ```

- Install `eggla` and its dependencies

  ``` r
  library(pak)
  VERSION <- "latest"
  utils::download.file(
    url = sprintf(
      "https://raw.githubusercontent.com/mcanouil/eggla/%s/.github/.devcontainer/eggla/R/pkg.lock",
      VERSION
    ),
    destfile = "pkg.lock"
  )
  lockfile_install(lockfile = "pkg.lock", lib = temp_library)
  pkg_install(sprintf("mcanouil/eggla@%s", VERSION), lib = temp_library, upgrade = FALSE, dependencies = FALSE)
  ```

## Docker Images

- `docker pull ghcr.io/mcanouil/eggla:devel`.
- `docker pull ghcr.io/mcanouil/eggla:latest`.

## License

MIT © [Mickaël Canouil](https://mickael.canouil.fr/), Nicole Warrington,
Kimberley Burrows, and Anni Heiskala.

## Code of Conduct

Please note that the `eggla` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).  
By contributing to this project, you agree to abide by its terms.

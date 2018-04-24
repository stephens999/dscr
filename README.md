dscr
====

[![Build Status](https://travis-ci.org/stephens999/dscr.png?branch=master)](https://travis-ci.org/stephens999/dscr)

This repo contains work on an R package for performing dynamic statistical comparisons (DSCs) in R.

For a simple vignette see [here] (https://github.com/stephens999/dscr/blob/master/vignettes/one_sample_location.rmd)

For a brief introduction to DSCs see [here](https://github.com/stephens999/dscr/blob/master/intro.md).

# Installation

One vignette depends on the `ashr` package, so to get that to work you will have to install both `ashr` and `dscr`.
You can do this using:

`devtools::install_github("stephens999/ashr")` 

`devtools::install_github("stephens999/dscr",build_vignettes=TRUE)` 

in R.

Or in terminal use:

    make deps
    make docs
    make build
    make vignettes
    make run-methods

The result of `make vignettes` will appear under `vignettes`, and the result of `make run-methods` will appear in `inst/examples/one_sample_location/`. To clean up the generated files, just use `make vignettes-clean` and `make run-methods-clean`.


Once the vignette is generated you should be able to see it in R by executing:

    browseVignettes("dscr")


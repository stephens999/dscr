# dscr: dynamic statistical comparisons in R

[![Build Status](https://travis-ci.org/stephens999/dscr.png?branch=master)](https://travis-ci.org/stephens999/dscr)

This repository contains work on an R package for performing dynamic
statistical comparisons (DSCs) in R. For a brief introduction to DSCs,
see [here](intro.md).

# Quick Start

1. In R, install the latest version of smashr (with the vignettes
   enabled) using [devtools][devtools]:

   ```R
   install.packages("devtools")
   library(devtools)
   install_github("stephens999/dscr",build_vignettes = TRUE)
   ```

   This will build the smashr package with the vignettes.

2. Once the vignettes are generated, you should be able to view them
   in R or RStudio by executing:

   ```R
   browseVignettes("dscr")
   ```
   
[devtools]: https://github.com/r-lib/devtools

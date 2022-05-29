# Analysis of RAS discontinuation

<!-- badges: start -->
![Languages](https://img.shields.io/badge/Languages-R-6498d3)
![Conducted By](https://img.shields.io/badge/Conducted%20By-The%20George%20Institute%20for%20Global%20Health-72297c)
<!-- badges: end -->

## Overview

This code includes a reprex to analyse time to RAS discontinuation. I've done my best to include all quirks I've encountered in the CREDENCE data in the synthetic data created at the beginning of the code.

If you clone this repository you should be able to run it without too many issues (however I've not tested it).

## Dependencies

To get this code to work, please install all dependencies. The code for this, displayed below, is at the beginning of the script.

``` r
# Library names
libs = c("here", "survival", "tidyverse")

# Install libraries
install.packages(setdiff(libs, rownames(installed.packages())))
```

## Folder structure

Below is an overview of the folders in this repository that are actively synchronised with GitHub.

### code

`code` contains the reprex code: `ras_discontinuation.R`

### figs

`figs` contains the logo for The George Institute for Global Health used for the README

### src

`src` contains any files that are called via `source()` in the scripts in `code`, i.e. functions.

## Contact

If you encounter a clear bug or have any questions/suggestions, please feel free to contact me via my [email](mailto:rfletcher@georgeinstitute.org.au?subject=Inquiry).
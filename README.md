# Reporting package for BRICK

R package **reportBrick**, version **0.1.0**

[![CRAN status](https://www.r-pkg.org/badges/version/reportBrick)](https://cran.r-project.org/package=reportBrick)  [![R build status](https://github.com/pik-piam/reportBrick/workflows/check/badge.svg)](https://github.com/pik-piam/reportBrick/actions) [![codecov](https://codecov.io/gh/pik-piam/reportBrick/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/reportBrick) 

## Purpose and Functionality

This package contains BRICK-specific routines to report model results. The main functionality is to generate a mif-file from a given BRICK model run folder.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("reportBrick")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Robin Hasse <robin.hasse@pik-potsdam.de>.

## Citation

To cite package **reportBrick** in publications use:

Hasse R (2024). _reportBrick: Reporting package for BRICK_. R package version 0.1.0, <https://github.com/pik-piam/reportBrick>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {reportBrick: Reporting package for BRICK},
  author = {Robin Hasse},
  year = {2024},
  note = {R package version 0.1.0},
  url = {https://github.com/pik-piam/reportBrick},
}
```

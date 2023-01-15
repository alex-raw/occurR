<!-- badges: start -->
[![R-CMD-check](https://github.com/alex-raw/occurR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/alex-raw/occurR/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/alex-raw/occurR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/alex-raw/occurR?branch=main)
![GitHub License](https://img.shields.io/github/license/alex-raw/occurR)
<!-- badges: end -->

## occurR

Tools for working with word frequency lists and calculating lexical statistical
measures. The main aim is to offer a streamlined and idiomatic interface while
keeping it simple, fast, and light on dependencies.

This package is consistent with, collects from, and improves upon some R code
popular among linguists, e.g. Flach (2022) `collexemes` package (currently
archived), Gries' (2022) `Coll.analysis.R`, and Gries' (2010) `dispersions.R`,
and functionality of Evert's UCS/R (2005)

<!--
Flach, Susanne. 2021. Collostructions: An R implementation for the family of collostructional methods. Package version v.0.2.0, https://sfla.ch/collostructions/.
Gries, Stefan Th. 2010. Dispersions and adjusted frequencies in corpora: further explorations. In Stefan Th. Gries, Stefanie Wulff, & Mark Davies (eds.), Corpus linguistic applications: current studies, new directions, 197-212. Amsterdam: Rodopi.
Evert, Stefanie. 2005. The statistics of word cooccurrences: word pairs and collocations. http://dx.doi.org/10.18419/opus-2556.
-->

## Install

```
install.packages("remotes")
remotes::install_github("alex-raw/occurR")
```

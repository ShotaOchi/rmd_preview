# rmd_preview

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## About

rmd_preview is a shiny application that enables us to preview rmarkdown.

## Usage
Run the following R code to use rmd_preview.
```
shiny::runGitHub("ShotaOchi/rmd_preview")
```
Note that rmd_preview requires **rmarkdown** package, **shiny** package, and **callr** package.
```
install.packages(c("callr", "rmarkdown", "shiny"))
```

## Motivation

We can preview rmarkdown with RStudio.

However, it's a bother to install RStudio just to preview rmarkdown.

That's why I made rmd_preview.
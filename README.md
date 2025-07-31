
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Epoch: Epileptogenic Zone Localization Based on neural Fragility EEG marker

[![](https://img.shields.io/badge/devel%20version-1.0.0-blue.svg)](https://github.com/Jiefei-Wang/Epoch)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/github/languages/code-size/Jiefei-Wang/Epoch.svg)](https://github.com/Jiefei-Wang/Epoch)
[![](https://img.shields.io/github/last-commit/Jiefei-Wang/Epoch.svg)](https://github.com/Jiefei-Wang/Epoch/commits/main)
[![R-CMD-check](https://github.com/Jiefei-Wang/Epoch/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Jiefei-Wang/Epoch/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/Jiefei-Wang/Epoch/graph/badge.svg)](https://app.codecov.io/gh/Jiefei-Wang/Epoch)


## Install Package
To install this package, run
```{r}
remotes::install_github("Jiefei-Wang/Epoch")
```


## Use
This package provides a downloader for downloading example data
```{r}
dl <- EpochDownloader()
```

Use `$` or `[[` to get a single file, or `[` to get a list of files
```{r}
epoch <- dl[[1]]
```

The downloader does not create a permanent copy of the data, so you must save the Epoch object if you want to use it later


## Epoch
You can access the Epoch data using `tblData`, the column, row, and object meta using `colData`, `rowData`, and `metaData` respectively. 
```{r}
tblData(epoch)
colData(epoch)
rowData(epoch)
metaData(epoch)
```

You can subset the `epoch` object using the `[` operator. 
```{r}
epoch[1:10, 1:10]
```

You can also use `crop` to crop the data by time (in seconds) 
```{r}
crop(epoch, from = -10, to = 10)
```

For more information, see the package vignette.


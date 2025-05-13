## Install Package
This package depends on `TableContainer` package, which is not available on CRAN. You can install it from GitHub using the following command:
```{r}
remotes::install_github("Jiefei-Wang/TableContainer")
```

To install this package, run
```{r}
remotes::install_github("Jiefei-Wang/Epoch")
```


## Use
This package provides a downloader for downloading example data
```{r}
dl <- EpochDownloader()
```

Use `$` to get a single file, or `[[` to get a list of files
```{r}
epoch <- dl$Retrostudy_subpt2_1
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
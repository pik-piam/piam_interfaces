# Project specific interfaces to REMIND / MAgPIE

R package **piamInterfaces**, version **0.12.3**

[![CRAN status](https://www.r-pkg.org/badges/version/piamInterfaces)](https://cran.r-project.org/package=piamInterfaces)  [![R build status](https://github.com/pik-piam/piamInterfaces/workflows/check/badge.svg)](https://github.com/pik-piam/piamInterfaces/actions) [![codecov](https://codecov.io/gh/pik-piam/piamInterfaces/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/piamInterfaces) [![r-universe](https://pik-piam.r-universe.dev/badges/piamInterfaces)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

Project specific interfaces to REMIND / MAgPIE.

## Tutorials

- To understand how to submit to the IIASA database, read this [REMIND tutorial](https://github.com/remindmodel/remind/blob/develop/tutorials/13_Submit_to_IIASA_database.md).

- To edit a template in `R`, use:
  ```
  templatedata <- getTemplate("AR6")
  ...
  write.csv2(templatedata, "test.csv", na = "", row.names = FALSE, quote = FALSE)
  ```

- The github diff on a large semicolon-separated file is often unreadable.
For a human-readable output, save the old version of the mapping and run:
  ```
  remind2::compareScenConf(fileList = c("oldfile.csv", "mappingfile.csv"), row.names = NULL)
  ```

- To compare the results of different models, pass as `modeldata` a [quitte](https://github.com/pik-piam/quitte/) object or a csv/xlsx file. You get a PDF document for each scenario and each model with area plots for all the summation groups in `AR6` (or `NAVIGATE`) [summation files](https://github.com/pik-piam/piamInterfaces/tree/master/inst/summations) plus line plots for each variable in the `lineplotVariables` vector you supplied. It takes some time, better use a `slurm` job for:
  ```
  plotIntercomparison(modeldata, summationsFile = "AR6", lineplotVariables = c("Temperature|Global Mean", "Population"))
  ```

- If your `modeldata` is not well filtered such that for example model regions are not too different, you can use `interactive = TRUE` which allows to select models, regions, scenarios and variables that you like in your PDF. As `lineplotVariables`, you can also specify template names.
  ```
  plotIntercomparison(modeldata, summationsFile = "AR6", lineplotVariables = c("AR6", "AR6_NGFS"), interactive = TRUE)
  ```

## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("piamInterfaces")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Falk Benke <benke@pik-potsdam.de>.

## Citation

To cite package **piamInterfaces** in publications use:

Benke F, Richters O (2023). _piamInterfaces: Project specific interfaces to REMIND / MAgPIE_. R package version 0.12.3, <https://github.com/pik-piam/piamInterfaces>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {piamInterfaces: Project specific interfaces to REMIND / MAgPIE},
  author = {Falk Benke and Oliver Richters},
  year = {2023},
  note = {R package version 0.12.3},
  url = {https://github.com/pik-piam/piamInterfaces},
}
```

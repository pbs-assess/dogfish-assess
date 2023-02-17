## Data and code for BC Pacific Spiny Dogfish operating models

- Raw datasets from PBS databases are in `data/raw`.
- Derived datasets are in `data/generated`.
- Analysis scripts are in `analysis`.
- Geostatistical indexes are pushed in `data/generated` so the corresponding scripts in `analysis` can be skipped.

### Required non-CRAN packages

```r
remotes::install_github("pbs-assess/gfdata")
remotes::install_github("pbs-assess/gfplot")
remotes::install_github("pbs-assess/ggmse")
remotes::install_github("pbs-assess/csasdown")
remotes::install_github("pbs-assess/rosettafish")
```

[CRAN sdmTMB](https://CRAN.R-project.org/package=sdmTMB) works or the development version:

```r
remotes::install_github("pbs-assess/sdmTMB")
```

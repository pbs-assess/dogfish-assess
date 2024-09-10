## Data and code for BC Pacific Spiny Dogfish operating models

- Raw datasets from PBS databases are in `data/raw`.
- Derived datasets are in `data/generated`.
- Analysis scripts are in `analysis`.
- Geostatistical indexes are pushed in `data/generated` so the corresponding scripts in `analysis` can be skipped.
- SS3 model configurations are in `ss3`.
- R code to run SS3 models and generate figures from model output are in `ss3`.

### Required CRAN packages

```r
install.packages("remotes")
install.packages("tidyverse")
install.packages("cowplot")
install.packages("RColorBrewer")
install.packages("here")
install.packages("future")
install.packages("rnaturalearth")
install.packages("snowfall")
install.packages("viridis")
install.packages("reshape2")
install.packages("raster")
install.packages("terra")

install.packages("MSEtool")
install.packages("DLMtool")
install.packages("SAMtool")
install.packages("sdmTMB")
install.packages("r4ss")
```

### Required non-CRAN packages

```r
remotes::install_github("pbs-assess/gfdata")
remotes::install_github("pbs-assess/gfiphc")
remotes::install_github("pbs-assess/gfplot")
remotes::install_github("pbs-assess/csasdown")
remotes::install_github("pbs-assess/rosettafish")
```

### Stock Synthesis executable

Available at <https://github.com/nmfs-stock-synthesis/stock-synthesis/releases/tag/v3.30.20>

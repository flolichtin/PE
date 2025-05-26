# PE

This repository contains the analysis-ready data from the Swiss Mobility Panel (wave 3; priority evaluator). The priority evaluator tool (elaborating on the survey method and the implementation details) can be found here: https://github.com/DeSciL/SmpPriorityEvaluator.

## Usage

The repository is structured as an R-package and can be installed as such either directly

```R
devtools::install_github("flolichtin/PE", build_vignettes = TRUE)
```

or from the `.tar.gz` file. For non-R users, see the main data of interest in `inst/csv`. A codebook for the variable description (underlying `data_w`) can be found here: https://istp.ethz.ch/research/swiss-mobility-panel.html. For `data_pe` see `inst/csv/codebook_data_pe.csv`.

## Dev dependencies

>This is not relevant for the data user...

### Packages

- Install `labelr` package: `devtools::install_github("ivt-baug-ethz/labelr")` (Version: 0.0.2.9000)
- Install `Heimisc` package: `devtools::install_github("dheimgartner/Heimisc")`

### Script chronology

1. `./data-raw/data_pe_analytic.R`
2. `./data-raw/data_w.R`
3. `./data-raw/data_pe.R`
4. `./data_raw/data_accessibility.R`
5. `./data-raw/sample_definition.R`
6. `./data-raw/data_replication.R`

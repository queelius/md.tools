
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R package: `md.tools`

<!-- badges: start -->
<!-- badges: end -->

Masked data tools.

## Installation

You can install the development version of `md.tools` from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
#> Installing package into '/usr/local/lib/R/site-library'
#> (as 'lib' is unspecified)
devtools::install_github("queelius/md.tools")
#> Skipping install of 'md.tools' from a github remote, the SHA1 (52300660) has not changed since last install.
#>   Use `force = TRUE` to force installation
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
#> ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
#> ✓ tibble  3.1.6     ✓ dplyr   1.0.7
#> ✓ tidyr   1.1.4     ✓ stringr 1.4.0
#> ✓ readr   2.1.1     ✓ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
```

## Example

We demonstrate the use of `md.tools` with a simple data set:

``` r
exp_series_data_1 <- read_csv("./raw-data/exp_series_data_1.csv")
#> Rows: 1000 Columns: 9
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> dbl (6): s, k, w, t.1, t.2, t.3
#> lgl (3): c.1, c.2, c.3
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
usethis::use_data(exp_series_data_1, overwrite = TRUE)
#> ✓ Setting active project to '/home/spinoza/filetopia/gdrive/filetopia/sources/md.tools'
#> ✓ Saving 'exp_series_data_1' to 'data/exp_series_data_1.rda'
#> • Document your data (see 'https://r-pkgs.org/data.html')
```

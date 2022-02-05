
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R package: `md.tools`

<!-- badges: start -->
<!-- badges: end -->

Masked data tools.

## Installation

You can install the development version of `md.tools` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("queelius/md.tools")
#> Skipping install of 'md.tools' from a github remote, the SHA1 (453091e3) has not changed since last install.
#>   Use `force = TRUE` to force installation
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
#> ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
#> ✓ tibble  3.1.6     ✓ dplyr   1.0.7
#> ✓ tidyr   1.1.4     ✓ stringr 1.4.0
#> ✓ readr   2.1.2     ✓ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(md.tools)
```

## Example

We demonstrate the use of `md.tools` with a simple data set:

``` r
knitr::kable(head(exp_series_data_1))
```

|         s |   k |   w |       t.1 |       t.2 |       t.3 | c.1   | c.2   | c.3   |
|----------:|----:|----:|----------:|----------:|----------:|:------|:------|:------|
| 0.1441526 |   2 |   2 | 0.2811524 | 0.1441526 | 0.2658110 | TRUE  | TRUE  | FALSE |
| 0.0105258 |   1 |   2 | 0.0105258 | 0.0140527 | 0.0633002 | TRUE  | FALSE | TRUE  |
| 0.0363167 |   2 |   2 | 0.1047424 | 0.0363167 | 0.5452473 | TRUE  | TRUE  | FALSE |
| 0.0097178 |   1 |   2 | 0.0097178 | 0.2512075 | 0.0960429 | TRUE  | FALSE | TRUE  |
| 0.0376568 |   3 |   2 | 0.0936712 | 0.0942795 | 0.0376568 | TRUE  | FALSE | TRUE  |
| 0.0957521 |   3 |   2 | 0.2832620 | 0.3908009 | 0.0957521 | FALSE | TRUE  | TRUE  |

``` r
df <- md.tools::md_read_csv_with_meta("./raw-data/exp_series_data_1.csv")
#> Rows: 1000 Columns: 9
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> dbl (6): s, k, w, t.1, t.2, t.3
#> lgl (3): c.1, c.2, c.3
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#usethis::use_data(exp_series_data_1, overwrite = TRUE)
```

``` r
md.tools::boolean_matrix_to_integer_list(df,"c")
#> # A tibble: 1,000 × 10
#>          s     k     w     t.1     t.2    t.3 c.1   c.2   c.3   c        
#>      <dbl> <dbl> <dbl>   <dbl>   <dbl>  <dbl> <lgl> <lgl> <lgl> <list>   
#>  1 0.144       2     2 0.281   0.144   0.266  TRUE  TRUE  FALSE <int [2]>
#>  2 0.0105      1     2 0.0105  0.0141  0.0633 TRUE  FALSE TRUE  <int [2]>
#>  3 0.0363      2     2 0.105   0.0363  0.545  TRUE  TRUE  FALSE <int [2]>
#>  4 0.00972     1     2 0.00972 0.251   0.0960 TRUE  FALSE TRUE  <int [2]>
#>  5 0.0377      3     2 0.0937  0.0943  0.0377 TRUE  FALSE TRUE  <int [2]>
#>  6 0.0958      3     2 0.283   0.391   0.0958 FALSE TRUE  TRUE  <int [2]>
#>  7 0.169       3     2 0.197   1.01    0.169  FALSE TRUE  TRUE  <int [2]>
#>  8 0.270       3     2 0.322   0.371   0.270  FALSE TRUE  TRUE  <int [2]>
#>  9 0.299       3     2 0.390   0.401   0.299  TRUE  FALSE TRUE  <int [2]>
#> 10 0.00794     2     2 0.524   0.00794 0.120  FALSE TRUE  TRUE  <int [2]>
#> # … with 990 more rows
md.tools::is_md(df)
#> [1] FALSE
head(md.tools::matrix_from(df,"c"))
#>        c.1   c.2   c.3
#> [1,]  TRUE  TRUE FALSE
#> [2,]  TRUE FALSE  TRUE
#> [3,]  TRUE  TRUE FALSE
#> [4,]  TRUE FALSE  TRUE
#> [5,]  TRUE FALSE  TRUE
#> [6,] FALSE  TRUE  TRUE
head(md.tools::matrix_from(df,"t"))
#>              t.1        t.2        t.3
#> [1,] 0.281152420 0.14415257 0.26581097
#> [2,] 0.010525786 0.01405274 0.06330024
#> [3,] 0.104742431 0.03631670 0.54524729
#> [4,] 0.009717816 0.25120751 0.09604295
#> [5,] 0.093671209 0.09427946 0.03765681
#> [6,] 0.283262043 0.39080088 0.09575208
md.tools::md_latent(df)
#> [1] "k"   "t.1" "t.2" "t.3"
df <- md.tools::md_unmark_latent(df,"t.1")
md.tools::md_latent(df)
#> [1] "k"   "t.2" "t.3"
df <- md.tools::md_mark_latent(df,"k")
md.tools::md_latent(df)
#> [1] "k"   "t.2" "t.3"
df <- md.tools::md_unmark_latent(df,"k")
md.tools::md_latent(df)
#> [1] "t.2" "t.3"
df <- md.tools::md_mark_latent(df,"aabk")
md.tools::md_latent(df)
#> [1] "aabk" "t.2"  "t.3"
df <- md.tools::md_unmark_latent(df,"aabk")
md.tools::md_latent(df)
#> [1] "t.2" "t.3"
df <- md.tools::md_unmark_latent(df,"aabkasdfads")
md.tools::md_latent(df)
#> [1] "t.2" "t.3"
```

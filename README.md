
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R package: `md.tools`

<!-- badges: start -->
<!-- badges: end -->

A miscellaneous set of tools for working with masked data. The tool set
takes inspiration from functional programming, with inputs and outputs
defined over data frames instead of other kinds of objects or values.
Relatedly, it is compatible with the *tidyverse* way of doing things,
where we may compose together multiple simple functions through, say,
the pipe operator to generate more complex transformations of data
frames.

## Installation

You can install the development version of `md.tools` from
[GitHub](https://github.com/queelius/md.tools) with:

``` r
# install.packages("devtools")
devtools::install_github("queelius/md.tools")
library(tidyverse)
library(md.tools)
```

A lot of space in `md.tools` is devoted to working with matrices encoded
in the columns of data frames. We could directly store matrices in a
column, or even general lists, but we prefer to work with columns
defined over primitive types like `boolean`.

So, suppose we have a `boolean` matrix `C` of size `10`-by-`3`:

``` r
C <- matrix(sample(c(T,F), size=3*10, replace=TRUE), nrow=10)
```

We may represent this in a data frame of `10` rows with the columns
`c.1`, `c.2`, and `c.3` with:

``` r
df <- md.tools::encode_matrix(C,"c")
print(df)
#> # A tibble: 10 × 1
#>    c1[,1] [,2]  [,3] 
#>    <lgl>  <lgl> <lgl>
#>  1 TRUE   TRUE  TRUE 
#>  2 FALSE  FALSE FALSE
#>  3 FALSE  FALSE TRUE 
#>  4 TRUE   TRUE  FALSE
#>  5 FALSE  FALSE TRUE 
#>  6 FALSE  TRUE  FALSE
#>  7 TRUE   TRUE  TRUE 
#>  8 FALSE  TRUE  TRUE 
#>  9 TRUE   TRUE  FALSE
#> 10 TRUE   FALSE FALSE
```

We prefer to work with plaintext data files. To store data frames, we
prefer to work with plaintext files like CSV files, where each row
corresponds to some set of measurements of some experimental unit.

However, we may also want to store *metadata* about the experiment that
generated the data, or we may wish to store more information about the
experimental units that does not naturally fit into the data frame
model.

To store metadata, we take the general approach of storing JSON
(Javscript Object Notation) in the *comments* of the tabular data file
(like CSV), where a comment by default is anything after the `#`
character on a line.

``` r
md <- md.tools::md_read_csv_with_meta("./raw-data/exp_series_data_1.csv")
#> Rows: 1000 Columns: 9
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> dbl (6): s, k, w, t.1, t.2, t.3
#> lgl (3): c.1, c.2, c.3
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
print(md)
#> # A tibble: 1,000 × 9
#>          s     k     w     t.1     t.2    t.3 c.1   c.2   c.3  
#>  *   <dbl> <dbl> <dbl>   <dbl>   <dbl>  <dbl> <lgl> <lgl> <lgl>
#>  1 0.144       2     2 0.281   0.144   0.266  TRUE  TRUE  FALSE
#>  2 0.0105      1     2 0.0105  0.0141  0.0633 TRUE  FALSE TRUE 
#>  3 0.0363      2     2 0.105   0.0363  0.545  TRUE  TRUE  FALSE
#>  4 0.00972     1     2 0.00972 0.251   0.0960 TRUE  FALSE TRUE 
#>  5 0.0377      3     2 0.0937  0.0943  0.0377 TRUE  FALSE TRUE 
#>  6 0.0958      3     2 0.283   0.391   0.0958 FALSE TRUE  TRUE 
#>  7 0.169       3     2 0.197   1.01    0.169  FALSE TRUE  TRUE 
#>  8 0.270       3     2 0.322   0.371   0.270  FALSE TRUE  TRUE 
#>  9 0.299       3     2 0.390   0.401   0.299  TRUE  FALSE TRUE 
#> 10 0.00794     2     2 0.524   0.00794 0.120  FALSE TRUE  TRUE 
#> # … with 990 more rows
```

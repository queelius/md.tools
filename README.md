
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R package: `md.tools`

<!-- badges: start -->
<!-- badges: end -->

A miscellaneous set of tools for working with *masked data* and common
features of masked data. The tool set takes inspiration from functional
programming, with inputs and outputs defined over masked data frames of
type `tbl_md` (or just data frames), making it consistent with the
*tidyverse* way of doing things.

We provide a set of simple functions on masked data frames, which may be
used to compose more complicated functions, particularly when using the
pipe operator `%>%`.

#### Installation

You can install the development version of `md.tools` from
[GitHub](https://github.com/queelius/md.tools) with:

``` r
# install.packages("devtools")
devtools::install_github("queelius/md.tools")
#> 
#>      checking for file ‘/tmp/RtmpuZoRjX/remotes1cecf94219146e/queelius-md.tools-6a06123/DESCRIPTION’ ...  ✓  checking for file ‘/tmp/RtmpuZoRjX/remotes1cecf94219146e/queelius-md.tools-6a06123/DESCRIPTION’
#>   ─  preparing ‘md.tools’:
#>      checking DESCRIPTION meta-information ...  ✓  checking DESCRIPTION meta-information
#>   ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>   ─  building ‘md.tools_0.1.0.tar.gz’
#>      
#> 
library(tidyverse)
library(md.tools)
```

## Matrices

A lot of space in `md.tools` is devoted to working with matrices encoded
in the columns of data frames. We could directly store matrices in a
column, but we prefer to work with columns defined over primitive types
like `boolean`.

Consider the `boolean` matrix `C` of size `10`-by-`3`:

``` r
n <- 5
m <- 3
C <- matrix(sample(c(T,F), size=m*n, replace=TRUE), nrow=n)
```

We may represent this in a data frame of 5 rows with the columns `c.1`,
`c.2`, and `c.3` with:

``` r
md <- md_encode_matrix(C,"c")
print(md)
#> # A tibble: 5 × 3
#>   c1    c2    c3   
#>   <lgl> <lgl> <lgl>
#> 1 FALSE TRUE  TRUE 
#> 2 TRUE  FALSE TRUE 
#> 3 TRUE  TRUE  FALSE
#> 4 TRUE  TRUE  TRUE 
#> 5 FALSE FALSE TRUE
```

We may also decode a matrix stored in a data frame with:

``` r
print(all(C == md_decode_matrix(md,"c")))
#> [1] TRUE
```

We may want to work with a Boolean matrix as a list. The function
`md_boolean_matrix_to_list` uses the following transformation:

If we have a *n*-by-*m* Boolean matrix, then if the (*j*,*k*)-element is
`TRUE`, the *j*-th vector in the list contains the integer *k*.

For instance, suppose we wish to show `md` with a candidate set as a
“set” of integers:

``` r
as.data.frame(md %>% dplyr::select(-starts_with("c")) %>%
  mutate("candidates"=lapply(
    md_boolean_matrix_to_list(C),
    function(x) { paste0("{",toString(x),"}") })))
#>   candidates
#> 1     {2, 3}
#> 2     {1, 3}
#> 3     {1, 2}
#> 4  {1, 2, 3}
#> 5        {3}
```

For completion, we allow converting between these two representations.
Thus, the inverse of `md_boolean_matrix_to_list` is just
`md_list_to_boolean_matrix`.

## Decorators

We now consider some data frame transformations that adds additional
columns with information that may be inferred from what is already in
the data frame. For this reason, we have chosen to call them
*decorators*.

In a masked data frame, we may have a column `k` that stores the failed
component. We simulate failed components and mark them as *latent* with:

``` r
md <- md %>% mutate(k=sample(1:m,n,replace=TRUE)) %>%
  md_mark_latent("k")
print(md)
#> Latent variables:  k 
#> # A tibble: 5 × 4
#>   c1    c2    c3        k
#>   <lgl> <lgl> <lgl> <int>
#> 1 FALSE TRUE  TRUE      2
#> 2 TRUE  FALSE TRUE      2
#> 3 TRUE  TRUE  FALSE     3
#> 4 TRUE  TRUE  TRUE      1
#> 5 FALSE FALSE TRUE      3
```

We may additionally have a candidate set encoded by the Boolean columns
`c1`,…,`cm`, in which case we may infer whether the candidate set
*contains* the failed component `k` with:

``` r
md$k <- sample(1:m,n,replace=TRUE)
md <- md %>% md_cand_contains("c")
print(md)
#> Latent variables:  k 
#> # A tibble: 5 × 5
#>   c1    c2    c3        k contains
#>   <lgl> <lgl> <lgl> <int> <lgl>   
#> 1 FALSE TRUE  TRUE      2 TRUE    
#> 2 TRUE  FALSE TRUE      1 TRUE    
#> 3 TRUE  TRUE  FALSE     3 FALSE   
#> 4 TRUE  TRUE  TRUE      1 TRUE    
#> 5 FALSE FALSE TRUE      3 TRUE
```

We see that there is a new column, `contains`, that tells us whether the
candidate set actually contains the failed component. No new information
is given by this column, it only presents what information that is
already there in a potentially more conventient format.

Given the same data frame and candidate set, we may determine the
*cardinality* of the candidate sets with:

``` r
md <- md %>% md_cand_sizes("c")
print(md)
#> Latent variables:  k 
#> # A tibble: 5 × 6
#>   c1    c2    c3        k contains     w
#>   <lgl> <lgl> <lgl> <int> <lgl>    <int>
#> 1 FALSE TRUE  TRUE      2 TRUE         2
#> 2 TRUE  FALSE TRUE      1 TRUE         2
#> 3 TRUE  TRUE  FALSE     3 FALSE        2
#> 4 TRUE  TRUE  TRUE      1 TRUE         3
#> 5 FALSE FALSE TRUE      3 TRUE         1
```

We may *unmark* a column variable as latent with:

``` r
md <- md %>% md.tools::md_unmark_latent("k")
print(md)
#> # A tibble: 5 × 6
#>   c1    c2    c3        k contains     w
#>   <lgl> <lgl> <lgl> <int> <lgl>    <int>
#> 1 FALSE TRUE  TRUE      2 TRUE         2
#> 2 TRUE  FALSE TRUE      1 TRUE         2
#> 3 TRUE  TRUE  FALSE     3 FALSE        2
#> 4 TRUE  TRUE  TRUE      1 TRUE         3
#> 5 FALSE FALSE TRUE      3 TRUE         1
```

The latent variable specification is metadata about the masked data
frame, but it does not necessarily impose any requirements on algorithms
applied to it.

More generally, a masked data frame may have a lot more metadata about
it, and we provide some tools for working with them. However, for the
most part, you are expected to handle the metadata yourself. The
metadata is stored in the *attributes*, and so underneath the hood, a
masked data frame is just a data frame and may be treated as one.

## Metadata

To read and write data frames for sharing with others, including
yourself, we prefer to work with plaintext files like CSV files, where
each row corresponds to some set of measurements of some experimental
unit.

However, we may also want to store *metadata* about the experiment that
generated the data, or we may wish to store more information about the
experimental units that does not naturally fit into the data frame
model.

To store metadata, we take the general approach of storing JSON
(Javscript Object Notation) in the *comments* of the tabular data file
(like CSV), where a comment by default is anything after the `#`
character on a line.

``` r
data <- md.tools::md_read_csv_with_meta("./raw-data/exp_series_data_1.csv")
#> Rows: 1000 Columns: 9
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: ","
#> dbl (6): s, k, w, t.1, t.2, t.3
#> lgl (3): c.1, c.2, c.3
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
print(data)
#> Latent variables:  k t.1 t.2 t.3 
#> # A tibble: 1,000 × 9
#>          s     k     w     t.1     t.2    t.3 c.1   c.2   c.3  
#>      <dbl> <dbl> <dbl>   <dbl>   <dbl>  <dbl> <lgl> <lgl> <lgl>
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

We may view all of the metadata stored in `data`, minus the `row.names`,
with:

``` r
attr(data,"row.names") <- NULL
print(attributes(data))
#> $class
#> [1] "tbl_md"     "tbl_df"     "tbl"        "data.frame"
#> 
#> $names
#> [1] "s"   "k"   "w"   "t.1" "t.2" "t.3" "c.1" "c.2" "c.3"
#> 
#> $comment
#> [1] "this is a simulation test."
#> 
#> $family
#> [1] "masked.data::md_exp_series"
#> 
#> $index
#> [1] 3 4 5
#> 
#> $nodes
#>        family index
#> 1 exponential     3
#> 2 exponential     4
#> 3 exponential     5
#> 
#> $candidate_model
#> [1] "md_cand_m1"
#> 
#> $call
#> [1] "masked.data::md_exp_series(n=1000,rate=c(3,4,5),cand_model=masked.data::md_cand_m1(alpha=rep(1,1000),w=rep(2,1000)))"
#> 
#> $latent
#> [1] "k"   "t.1" "t.2" "t.3"
#> 
#> $observable
#> [1] "s"   "w"   "c.1" "c.2" "c.3"
#> 
#> $num_comp
#> [1] 3
#> 
#> $sample_size
#> [1] 1000
```

A lot of the metadata for `data` has to do with how the data was
generated. In particular, we see this data is the result of a simulation
for a series system with 3 exponentially distributed nodes parameterized
by *λ* = (3,4,5)′ and candidate model `md_cand_m1`.

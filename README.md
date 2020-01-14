
# `runner` an R package for running operations.

# <img src="man/figures/hexlogo.png" align="right" />

[![](https://cranlogs.r-pkg.org/badges/runner)](https://CRAN.R-project.org/package=runner)
[![](https://travis-ci.org/gogonzo/runner.svg?branch=master)](https://travis-ci.org/gogonzo/runner)
[![](https://ci.appveyor.com/api/projects/status/github/gogonzo/runner?branch=master&svg=true)](https://ci.appveyor.com/project/gogonzo/runner)
[![](https://codecov.io/gh/gogonzo/runner/branch/master/graph/badge.svg)](https://codecov.io/gh/gogonzo/runner/branch/master)

## About

Package contains standard running functions (aka. windowed, rolling,
cumulative) with additional options. `runner` provides extended
functionality like date windows, handling missings and varying window
size. `runner` brings also rolling streak and rolling which, what
extends beyond range of functions already implemented in R packages.

## Installation

Install package from from GitHub or from CRAN.

``` r
# devtools::install_github("gogonzo/runner")
install.packages("runner")
```

## Using runner

`runner` package provides functions applied on running windows. User can
apply any R function `f` in running window of length defined by `k`,
window `lag`, observation indexes `idx`.

![](man/figures/using_runner.png)

For example 14-days trimmed mean.

``` r
library(runner)
x <- rnorm(20)
dates <- seq.Date(Sys.Date(), Sys.Date() + 19, by = "1 day")

runner(x, 
       k = 14, 
       idx = dates, 
       f = function(xi) {
        mean(xi, na.rm = TRUE, trim = 0.05)
       })
```

    ##  [1]  0.10468129 -1.10512383 -0.64865486 -0.55765740 -0.62782432 -0.50965895
    ##  [7] -0.61416410 -0.54063542 -0.52521129 -0.56928930 -0.47981832 -0.41331350
    ## [13] -0.43998375 -0.41538428 -0.41326382 -0.12030370 -0.07638326 -0.16421363
    ## [19] -0.02382919 -0.04859132

## Running windows

In each section below specific options of the `runner` are explained
using illustrations and `window_run` function which creates list of
running windows. Diagram below illustrates what running windows are - in
this case running `k = 4` windows. For each of 15 elements of a vector
each window contains current 4 elements.

![](man/figures/running_windows_explain.png)

### Window size

`k` denotes number of elements in window. If `k` is a single value then
window size is constant for all elements of x. For varying window size
one should specify `k` as integer vector of `length(k) == length(x)`
where each element of `k` defines window length. If `k` is empty it
means that window will be cumulative (like `base::cumsum`). Example
below illustrates window of `k = 4` for 10th element of vector `x`.

![](man/figures/constant_window.png)

``` r
window_run(1:15, k = 4)
```

### Window lag

`lag` denotes how many observations windows will be lagged by. If `lag`
is a single value than it’s constant for all elements of x. For varying
lag size one should specify `lag` as integer vector of `length(lag) ==
length(x)` where each element of `lag` defines lag of window. Default
value of `lag = 0`. Example below illustrates window of `k = 4` lagged
by `lag = 2` for 10’th element of vector `x`. Lag can also be negative
value, which shifts window forward instead of backward.

![](man/figures/lagged_window_k_lag.png)

``` r
window_run(1:15, k = 4, lag = 2)
```

### Windows depending on date

Sometimes data points in dataset are not equally spaced (missing
weekends, holidays, other missings) and thus window size should vary to
keep expected time frame. If one specifies `idx` argument, than running
functions are applied on windows depending on date. `idx` should be the
same length as `x` of class `Date` or `integer`. Including `idx` can be
combined with varying window size, than k will denote number of periods
in window different for each data point. Example below illustrates
window of size `k = 5` lagged by `lag = 2`. In parentheses ranges for
each window.

![](man/figures/running_date_windows_explain.png)

``` r
idx <- c(4, 6, 7, 13, 17, 18, 18, 21, 27, 31, 37, 44, 47, 48)
window_run(x = 1:14, 
           k = 5, 
           lag = 1, 
           idx = idx)
```

### running at

Runner by default returns vector of the same size as `x` unless one puts
any-size vector to `at` argument. Each element of `at` is an index on
which runner calculates function. Below illustrates output of runner for
`at = c(13, 27, 45, 31)` which gives windows in ranges enclosed in
square brackets. Range for `at = 27` is `[22, 26]` which is not
available in current indices.

![](man/figures/runner_at_date.png)

``` r
idx <- c(4, 6, 7, 13, 17, 18, 18, 21, 27, 31, 37, 44, 47, 48)
window_run(x = idx, 
           k = 5, 
           lag = 1, 
           idx = idx, 
           at = c(18, 27, 45, 31))
```

### `NA` padding

Using `runner` one can also specify `na_pad = TRUE` which would return
`NA` for any window which is partially out of range - meaning that there
is no sufficient number of observations to fill the window. By default
`na_pad = FALSE`, which means that incomplete windows are calculated
anyway. `na_pad` is applied on normal cumulative windows and on windows
depending on date.

### Build-in functions

With `runner` one can use any R functions, but some of them are
optimized for speed reasons. These functions are:  
\- aggregating functions - `length_run`, `min_run`, `max_run`,
`minmax_run`, `sum_run`, `mean_run`, `streak_run`  
\- utility functions - `fill_run`, `lag_run`, `which_run`

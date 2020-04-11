
# `runner` an R package for running operations.

# <img src="man/figures/hexlogo.png" align="right" />

<!-- badges: start -->

[![](https://cranlogs.r-pkg.org/badges/runner)](https://CRAN.R-project.org/package=runner)
[![](https://travis-ci.org/gogonzo/runner.svg?branch=master)](https://travis-ci.org/gogonzo/runner)
[![](https://ci.appveyor.com/api/projects/status/github/gogonzo/runner?branch=master&svg=true)](https://ci.appveyor.com/project/gogonzo/runner)
[![](https://codecov.io/gh/gogonzo/runner/branch/master/graph/badge.svg)](https://codecov.io/gh/gogonzo/runner/branch/master)
<!-- badges: end -->

## About

Package contains standard running functions (aka. rolling) with
additional options like varying window size, lagging, handling missings
and windows depending on date. `runner` brings also rolling streak and
rolling which, what extends beyond range of functions already
implemented in R packages. This package can be successfully used to
manipulate and aggregate time series or longitudinal data.

## Installation

Install package from from GitHub or from CRAN.

``` r
# devtools::install_github("gogonzo/runner")
install.packages("runner")
```

## Using runner

`runner` package provides functions applied on running windows. The most
universal function is `runner::runner` which gives user possibility to
apply any R function `f` in running window. In example below quarterly
correlation is calculated lagged by 1 month.

``` r
library(runner)

x <- data.frame(
  date = seq.Date(Sys.Date(), Sys.Date() + 365, length.out = 20),
  a = rnorm(20),
  b = rnorm(20)
)

runner(
  x, 
  lag = "1 months",
  k = "4 months", 
  idx = x$date, 
  f = function(x) {
    cor(x$a, x$b)
  }
)
```

There are different kinds of running windows and all of them are
implemented in `runner`.

## Running windows

Following diagram illustrates what running windows are - in this case
running windows of length `k = 4`. For each of 15 elements of a vector
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
runner(1:15, k = 4, f = function(x) x)
```

### Window lag

`lag` denotes how many observations windows will be lagged by. If `lag`
is a single value than it is constant for all elements of x. For varying
lag size one should specify `lag` as integer vector of `length(lag) ==
length(x)` where each element of `lag` defines lag of window. Default
value of `lag = 0`. Example below illustrates window of `k = 4` lagged
by `lag = 2` for 10-th element of vector `x`. Lag can also be negative
value, which shifts window forward instead of backward.

![](man/figures/lagged_window_k_lag.png)

``` r
runner(
  1:15, 
  k = 4, 
  lag = 2, 
  f = function(x) x
)
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
each
window.

![](man/figures/runningdatewindows.png)

``` r
idx <- Sys.Date() + c(4, 6, 7, 13, 17, 18, 18, 21, 27, 31, 37, 42, 44, 47, 48)
runner(
  x = 1:15, 
  k = "5 days", 
  lag = "1 days", 
  idx = idx,
  function(x) x
)
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
idx <- c(4, 6, 7, 13, 17, 18, 18, 21, 27, 31, 37, 42, 44, 47, 48)
runner(
  x = idx, 
  k = 5, 
  lag = 1, 
  idx = idx, 
  at = c(18, 27, 48, 31),
  function(x) x
)
```

### `NA` padding

Using `runner` one can also specify `na_pad = TRUE` which would return
`NA` for any window which is partially out of range - meaning that there
is no sufficient number of observations to fill the window. By default
`na_pad = FALSE`, which means that incomplete windows are calculated
anyway. `na_pad` is applied on normal cumulative windows and on windows
depending on date. In example below two windows exceed range given by
`idx` so for these windows are empty for `na_pad = TRUE`. If used sets
`na_pad = FALSE` first window will be empty (no single element within
`[-2, 3]`) and last window will return elements within matching `idx`.

![](man/figures/runner_at_date_na_pad.png)

``` r
idx <- c(4, 6, 7, 13, 17, 18, 18, 21, 27, 31, 37, 42, 44, 47, 48)
runner(
  x = idx, 
  k = 5, 
  lag = 1, 
  idx = idx, 
  at = c(4, 18, 48, 51),
  na_pad = TRUE,
  f = function(x) x
)
```

### Build-in functions

With `runner` one can use any R functions, but some of them are
optimized for speed reasons. These functions are:  
\- aggregating functions - `length_run`, `min_run`, `max_run`,
`minmax_run`, `sum_run`, `mean_run`, `streak_run`  
\- utility functions - `fill_run`, `lag_run`, `which_run`

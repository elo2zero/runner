# runner 0.4.1
* fix `runner(..., na_pad)` for vectors to return `NA` when windows is incomplete.
Other methods already consistent.

# runner 0.4.0
* defunct `type` argument in favor of `simplify`.
* fixed error when using `runner::runner`.
* fixed `max_run` to return `NA` instead of `NaN` in incomplete or `NA` windows.

# runner 0.3.8
* implement parallel windows computation.
* added `simplify` identical to argument used in `sapply`.
* add runner for `xts` objects.
* fix runner for matrix.
* `type` argument is being deprecated.
* reduce number of dependencies.

# runner 0.3.7
* add `run_by` function to prespecify arguments for multiple runner calls
* `runner` supports `grouped_df`

# runner 0.3.5
* `runner` function to be applied on `data.frame` or `matrix` - running windows
constructed by subsetting rows.
* `runner` output can be a list.
* `at` `lag` and `k` allows to specify values as `POSIXt` sequence increment - 
same as `by` in `seq.POSIXt`.
* enhanced function documentation by images.

# runner 0.3.2
* `runner` function with default `type = "auto"` which automatically guess the type
of returned object.
* speed up `runner` function.

# runner 0.3.1

* added `at` argument to all functions to return output with specific indexes.

# runner 0.3.0

* switch to c++11 compiler
* fix lag to get latest value within window instead of outside of the window
* change argument in lag from k to lag
* runner able to return output specified by user in type argument

# runner 0.2.3

* allow negative lag
* `na_pad` in all functions with lag or k

# runner 0.2.2

* modified `runner` to accept any types of input vectors 
* added `lag` to all runner functions
* added `minmax_run` which calculates running minimum and maximum to denote current highs and lows
* improved vignettes

# runner 0.2.1
* added `runner` function which allows to apply custom function on running windows - so far returning only numeric

# runner 0.2.0

* all functions have additional `idx` argument which allows to compute running windows within specified date/time/indexes range.
* added `lag_run` function - lagged value able to be computed on date/time lag.
* added `length_run` function to compute number of elements within specified date/time/indexes range.

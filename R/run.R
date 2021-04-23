#' Apply running function
#'
#' Applies custom function on running windows.
#' @param x (`vector`, `data.frame`, `matrix`, `xts`)\cr
#'  Input in runner custom function `f`.
#'
#' @param k (`integer` vector or single value)\cr
#'  Denoting size of the running window. If `k` is a single value then window
#'  size is constant for all elements, otherwise if `length(k) == length(x)`
#'  different window size for each element. One can also specify `k` in the same
#'  way as `by` argument in \code{\link[base]{seq.POSIXt}}.
#'  See 'Specifying time-intervals' in details section.
#'
#' @param lag (`integer` vector or single value)\cr
#'  Denoting window lag. If `lag` is a single value then window lag is constant
#'  for all elements, otherwise if `length(lag) == length(x)` different window
#'  size for each element. Negative value shifts window forward. One can also
#'  specify `lag` in the same way as `by` argument in
#'  \code{\link[base]{seq.POSIXt}}. See 'Specifying time-intervals' in details
#'  section.
#'
#' @param idx (`integer`, `Date`, `POSIXt`)\cr
#'  Optional integer vector containing sorted (ascending) index of observation.
#'  By default `idx` is index incremented by one. User can provide index with
#'  varying increment and with duplicated values. If specified then `k` and
#'  `lag` are depending on `idx`. Length of `idx` have to be equal of length
#'  `x`.
#'
#' @param f (`function`)\cr
#'  Applied on windows created from `x`. This function is meant to summarize
#'  windows and create single element for each window, but one can also specify
#'  function which return multiple elements (runner output will be a list).
#'  By default runner returns windows as is (`f = function(x)`).
#'
#' @param at (`integer`, `Date`, `POSIXt`, `character` vector)\cr
#'  Vector of any size and any value defining output data points. Values of the
#'  vector defines the indexes which data is computed at. Can be also `POSIXt`
#'  sequence increment used in `at` argument in \code{\link[base]{seq.POSIXt}}.
#'  See 'Specifying time-intervals' in details section.
#'
#' @param na_pad (`logical` single value)\cr
#'  Whether incomplete window should return `NA` (if `na_pad = TRUE`)
#'  Incomplete window is when some parts of the window are out of range.
#'
#' @param type (defunct)\cr
#'  argument defunct from version 0.4.0. Use `simplify` instead.
#'
#' @param simplify (`logical` or `character` value)\cr
#'  should the result be simplified to a vector, matrix or higher dimensional
#'  array if possible. The default value, `simplify = TRUE`, returns a vector or
#'  matrix if appropriate, whereas if `simplify = "array"` the result may be an
#'  array of "rank" `(=length(dim(.)))` one higher than the result of output
#'  from the function `f` for each window. Consequences of `simplify` in `runner`
#'  are identical to `sapply`.
#'
#' @param cl (`cluster`) *experimental*\cr
#'  Create and pass the cluster to the `runner` function to run each window
#'  calculation in parallel. See \code{\link[parallel]{makeCluster}} in details.
#'
#'
#' @param ... (optional)\cr
#'   other arguments passed to the function `f`.
#'
#' @details
#' Function can apply any R function on running windows defined by `x`,
#' `k`, `lag`, `idx` and `at`. Running window can be calculated
#' on several ways:
#' \itemize{
#'  \item{**Cumulative windows**}{\cr
#'    applied when user doesn't specify `k` argument or specify `k = length(x)`,
#'    this would mean that `k` is equal to number of available elements \cr
#'    \if{html}{\figure{cumulativewindows.png}{options: width="75\%" alt="Figure: cumulativewindows.png"}}
#'    \if{latex}{\figure{cumulativewindows.pdf}{options: width=7cm}}
#'  }
#'  \item{**Constant sliding windows**}{\cr
#'    applied when user specify `k` as constant value keeping `idx` and
#'    `at` unspecified. `lag` argument shifts windows left (`lag > 0`)
#'    or right (`lag < 0`). \cr
#'    \if{html}{\figure{incrementalindex.png}{options: width="75\%" alt="Figure: incrementalindex.png"}}
#'    \if{latex}{\figure{incrementalindex.pdf}{options: width=7cm}}
#'  }
#'  \item{**Windows depending on date**}{\cr
#'    If one specifies `idx` this would mean that output windows size might
#'    change in size because of unequally spaced indexes. Fox example 5-period
#'    window is different than 5-element window, because 5-period window might
#'    contain any number of observation (7-day mean is not the same as 7-element
#'    mean)
#'     \cr
#'    \if{html}{\figure{runningdatewindows.png}{options: width="75\%" alt="Figure: runningdatewindows.png"}}
#'    \if{latex}{\figure{runningdatewindows.pdf}{options: width=7cm}}
#'  }
#'  \item{**Window at specific indices**}{\cr
#'    `runner` by default returns vector of the same size as `x` unless one
#'    specifies `at` argument. Each element of `at` is an index on which runner
#'    calculates function - which means that output of the runner is now of
#'    length equal to `at`. Note that one can change index of `x` by specifying
#'    `idx`. Illustration below shows output of `runner` for
#'    `at = c(18, 27, 45, 31)` which gives windows in ranges enclosed in square
#'    brackets. Range for `at = 27` is `[22, 26]` which is not available in
#'    current indices. \cr
#'    \if{html}{\figure{runnerat.png}{options: width="75\%" alt="Figure: runnerat.png"}}
#'    \if{latex}{\figure{runnerat.pdf}{options: width=7cm}}
#'  }
#' }
#' ## Specifying time-intervals
#'  `at` can also be specified as interval of the output defined by
#'  `at = "<increment>"` which results in indices sequence defined by
#'  `seq.POSIXt(min(idx), max(idx), by = "<increment>")`. Increment of sequence
#'  is the same as in \code{\link[base]{seq.POSIXt}} function.
#'  It's worth noting that increment interval can't be more frequent than
#'  interval of `idx` - for `Date` the most frequent time-unit is a `"day"`,
#'  for `POSIXt` a `sec`.
#'
#'  `k` and `lag` can also be specified as using time sequence increment.
#'  Available time units are
#'`"sec", "min", "hour", "day", "DSTday", "week", "month", "quarter" or "year"`.
#'  To increment by number of units one can also specify `<number> <unit>s`
#'  for example `lag = "-2 days"`, `k = "5 weeks"`.
#'
#'  Setting `k` and `lag` as a sequence increment can be also a vector can be a
#'  vector which allows to stretch and lag/lead each window freely on in time
#'  (on indices).
#' \cr
#' ## Parallel computing
#'  Beware that executing R call in parallel not always
#'  have the edge over single-thread even if the
#'  `cl <- registerCluster(detectCores())` was specified before.
#'  \cr
#'  Parallel windows are executed in the independent environment, which means
#'  that objects other than function arguments needs to be copied to the
#'  parallel environment using \code{\link[parallel]{clusterExport}}`. For
#'  example using `f = function(x) x + y + z` will result in error as
#'  \code{clusterExport(cl, varlist = c("y", "z"))} needs to be called before.
#'
#' @return vector with aggregated values for each window. Length of output is
#'  the same as `length(x)` or `length(at)` if specified. Type of the output
#'  depends on the output from a function `f`.
#'
#' @md
#' @rdname runner
#' @importFrom methods is
#' @importFrom parallel clusterExport parLapply
#' @export
runner <- function(
  x,
  f = function(x) x,
  k = integer(0),
  lag = integer(1),
  idx = integer(0),
  at = integer(0),
  na_pad = FALSE,
  type,
  simplify = TRUE,
  cl = NULL,
  ...
  ) {
  if (!missing(type)) {
    warning(
      "Argument 'type' is defunct and will be completely removed in the next release. #nolint
      Please use 'simplify' argument to manage the output type."
    )
  }

  UseMethod("runner", x)
}

#' @rdname runner
#' @examples
#'
#' # runner returns windows as is by default
#' runner(1:10)
#'
#' # mean on k = 3 elements windows
#' runner(1:10, f = mean, k = 3)
#'
#' # mean on k = 3 elements windows with different specification
#' runner(1:10, k = 3, f = function(x) mean(x, na.rm = TRUE))
#'
#' # concatenate two columns
#' runner(
#'   data.frame(
#'     a = letters[1:10],
#'     b = 1:10
#'   ),
#'   f = function(x) paste(paste0(x$a, x$b), collapse = "+")
#' )
#'
#' # concatenate two columns with additional argument
#' runner(
#'   data.frame(
#'     a = letters[1:10],
#'     b = 1:10
#'   ),
#'   f = function(x, xxx) {
#'     paste(paste0(x$a, xxx, x$b), collapse = " + ")
#'   },
#'   xxx = "..."
#' )
#'
#' # number of unique values in each window (varying window size)
#' runner(letters[1:10],
#'        k = c(1, 2, 2, 4, 5, 5, 5, 5, 5, 5),
#'        f = function(x) length(unique(x)))
#'
#' # concatenate only on selected windows index
#' runner(letters[1:10],
#'        f = function(x) paste(x, collapse = "-"),
#'        at = c(1, 5, 8))
#'
#' # 5 days mean
#' idx <- c(4, 6, 7, 13, 17, 18, 18, 21, 27, 31, 37, 42, 44, 47, 48)
#' runner::runner(
#'   x = idx,
#'   k = "5 days",
#'   lag = 1,
#'   idx = Sys.Date() + idx,
#'   f = function(x) mean(x)
#' )
#'
#'# 5 days mean at 4-indices
#' runner::runner(
#'   x = 1:15,
#'   k = 5,
#'   lag = 1,
#'   idx = idx,
#'   at = c(18, 27, 48, 31),
#'   f = mean
#' )
#' @export
runner.default <- function(  #nolint
  x,
  f = function(x) x,
  k = integer(0),
  lag = integer(1),
  idx = integer(0),
  at = integer(0),
  na_pad = FALSE,
  type,
  simplify = TRUE,
  cl = NULL,
  ...
) {
  if (any(is.na(k))) {
    stop("Function doesn't accept NA values in k vector");
  }
  if (any(is.na(lag))) {
    stop("Function doesn't accept NA values in lag vector");
  }
  if (any(is.na(idx))) {
    stop("Function doesn't accept NA values in idx vector");
  }
  if (!is(f, "function")) {
    stop("f should be a function")
  }

  # use POSIXt.seq
  at <- seq_at(at, idx)
  k <- k_by(k, if (length(at > 0)) at else idx, "k")
  lag <- k_by(lag, if (length(at > 0)) at else idx, "lag")

  w <- window_run(
    x = x,
    k = k,
    lag = lag,
    idx = idx,
    at = at,
    na_pad = na_pad
  )
  if (!is.null(cl) && is(cl, "cluster")) {
    answer <- parLapply(
      cl = cl,
      X = w,
      fun = f,
      ...
    )
  } else {
    answer <- lapply(w, function(.this_window)
      if (length(.this_window) == 0) {
        NA
      } else {
        f(.this_window, ...)
      }
    )
  }

  if (!isFALSE(simplify) && length(answer)) {
    simplify2array(answer, higher = (simplify == "array"))
  } else {
    answer
  }
}

#' @rdname runner
#' @examples
#'
#' # runner with data.frame
#' df <- data.frame(
#'   a = 1:13,
#'   b = 1:13 + rnorm(13, sd = 5),
#'   idx = seq(Sys.Date(), Sys.Date() + 365, by = "1 month")
#' )
#' runner(
#'   x = df,
#'   idx = "idx",
#'   at = "6 months",
#'   f = function(x) {
#'     cor(x$a, x$b)
#'   }
#' )
#'
#' # parallel computing
#' library(parallel)
#' data <- data.frame(
#'   a = runif(100),
#'   b = runif(100),
#'   idx = cumsum(sample(rpois(100, 5)))
#' )
#' const <- 0
#' cl <- makeCluster(1)
#' clusterExport(cl, "const", envir = environment())
#'
#' runner(
#'   x = data,
#'   k = 10,
#'   f = function(x) {
#'     cor(x$a, x$b) + const
#'   },
#'   idx = "idx",
#'   cl = cl
#' )
#' stopCluster(cl)
#' @export
runner.data.frame <- function( #nolint
  x,
  f = function(x) x,
  k = integer(0),
  lag = integer(1),
  idx = integer(0),
  at = integer(0),
  na_pad = FALSE,
  type,
  simplify = TRUE,
  cl = NULL,
  ...
) {
  # set arguments from attrs (set by run_by)
  k <- set_from_attribute_difftime(x, k) # no deep copy
  lag <- set_from_attribute_difftime(x, lag)
  idx <- set_from_attribute_index(x, idx)
  at <- set_from_attribute_at(x, at)
  na_pad <- set_from_attribute_logical(x, na_pad)

  if (any(is.na(k))) {
    stop("Function doesn't accept NA values in k vector");
  }
  if (any(is.na(lag))) {
    stop("Function doesn't accept NA values in lag vector");
  }
  if (any(is.na(idx))) {
    stop("Function doesn't accept NA values in idx vector");
  }
  if (!is(f, "function")) {
    stop("f should be a function")
  }

  # use POSIXt.seq
  at  <- seq_at(at, idx)
  k   <- k_by(k, if (length(at) > 0) at else idx, "k")
  lag <- k_by(lag, if (length(at) > 0) at else idx, "lag")

  w <- window_run(
    x = seq_len(nrow(x)),
    k = k,
    lag = lag,
    idx = idx,
    at = at,
    na_pad = na_pad
  )

  answer <- if (!is.null(cl) && is(cl, "cluster")) {
    clusterExport(cl, varlist = c("x", "f"), envir = environment())
    parLapply(
      cl = cl,
      X = w,
      fun = function(.this_window_idx) {
        if (length(.this_window_idx) == 0) {
          NA
        } else {
          f(x[.this_window_idx, ], ...)
        }
      }
    )

  } else {
    lapply(w, function(.this_window_idx) {
      if (length(.this_window_idx) == 0) {
        NA
      } else {
        f(x[.this_window_idx, ], ...)
      }
    })
  }

  if (!isFALSE(simplify) && length(answer)) {
    simplify2array(answer, higher = (simplify == "array"))
  } else {
    answer
  }
}

#' @rdname runner
#' @export
runner.grouped_df <- function(
  x,
  f = function(x) x,
  k = integer(0),
  lag = integer(1),
  idx = integer(0),
  at = integer(0),
  na_pad = FALSE,
  type,
  simplify = TRUE,
  cl = NULL,
  ...
) {
  runner.data.frame(
    x = this_group(x),
    f = f,
    lag = lag,
    idx = idx,
    at = at,
    na_pad = na_pad,
    simplify = simplify,
    cl = cl,
    ...
  )
}

#' @rdname runner
#' @examples
#'
#' # runner with matrix
#' data <- matrix(data = runif(100, 0, 1), nrow = 20, ncol = 5)
#' runner(
#'   x = data,
#'   f = function(x) {
#'     tryCatch(
#'       cor(x),
#'       error = function(e) NA
#'     )
#'  })
#' @export
runner.matrix <- function(
  x,
  f = function(x) x,
  k = integer(0),
  lag = integer(1),
  idx = integer(0),
  at = integer(0),
  na_pad = FALSE,
  type,
  simplify = TRUE,
  cl = NULL,
  ...
) {
  if (any(is.na(k))) {
    stop("Function doesn't accept NA values in k vector");
  }
  if (any(is.na(lag))) {
    stop("Function doesn't accept NA values in lag vector");
  }
  if (any(is.na(idx))) {
    stop("Function doesn't accept NA values in idx vector");
  }
  if (!is(f, "function")) {
    stop("f should be a function")
  }

  # use POSIXt.seq
  at  <- seq_at(at, idx)
  k   <- k_by(k,   if (length(at) > 0) at else idx, "k")
  lag <- k_by(lag, if (length(at) > 0) at else idx, "lag")

  w <- window_run(
    x = seq_len(nrow(x)),
    k = k,
    lag = lag,
    idx = idx,
    at = at,
    na_pad = na_pad
  )

  answer <- if (!is.null(cl) && is(cl, "cluster"))  {
    clusterExport(cl, varlist = c("x", "f"), envir = environment())
    parLapply(
      cl = cl,
      X = w,
      fun = function(.this_window_idx) {
        if (length(.this_window_idx) == 0) {
          NA
        } else {
          f(x[.this_window_idx, , drop = FALSE], ...)
        }
      },
      ...
    )
  } else {
    lapply(
      X = w,
      FUN = function(.this_window_idx) {
      if (length(.this_window_idx) == 0) {
        NA
      } else {
        f(x[.this_window_idx, , drop = FALSE], ...)
      }
    })
  }
  if (!isFALSE(simplify) && length(answer)) {
    simplify2array(answer, higher = (simplify == "array"))
  } else {
    answer
  }
}

#' @rdname runner
#' @export
runner.xts <- function(
  x,
  f = function(x) x,
  k = integer(0),
  lag = integer(1),
  idx = integer(0),
  at = integer(0),
  na_pad = FALSE,
  type,
  simplify = TRUE,
  cl = NULL,
  ...
) {
  if (!identical(idx, integer(0))) {
    warning(
      "'idx' argument has been specified and will mask index
      of the 'xts' object."
    )
  } else {
    idx <- structure(
      .Data = as.vector(attr(x, "index")),
      class = attr(attr(x, "index"), "tclass"),
      tz = attr(attr(x, "index"), "tzone")
    )
  }

  runner.matrix(
    x = x,
    f = f,
    k = k,
    lag = lag,
    idx = idx,
    at = at,
    na_pad = na_pad,
    simplify = simplify,
    cl,
    ...
  )
}

# utilities -----
get_runner_call_arg_names <- function() {
  runner_call_idx <- which(
    vapply(
      X =  rev(sys.calls()),
      FUN = function(x) {
        x[[1]] == as.name("runner") ||
          x[[1]] == as.name("runner::runner")
      },
      FUN.VALUE = logical(1)
    )
  ) - 1

  cl <- sys.call(-runner_call_idx)
  f <- get(
    x = as.character("runner"),
    mode = "function",
    envir = sys.frame(-runner_call_idx)
  )
  cl <- match.call(definition = f, call = cl)
  names(cl)
}

is_datetime_valid <- function(x) {
  all(
    grepl("^(sec|min|hour|day|DSTday|week|month|quarter|year)$",
          x = x) |
      grepl("^-*[0-9]+ (sec|min|hour|day|DSTday|week|month|quarter|year)s",
            x = x)
  )
}


#' Converts k and lag from time-unit-interval to int
#'
#' Converts k and lag from time-unit-interval to int
#' @inheritParams runner
#' @param param name of the parameter to be printed in error message
#' @examples
#' k <-  "1 month"
#' idx <- seq(
#'   as.POSIXct("2019-01-01 03:02:01"),
#'   as.POSIXct("2020-01-01 03:02:01"),
#'   by = "month"
#' )
#' k_difftime <- runner:::k_by(k, idx, param = "k")
#' idx - k_difftime
k_by <- function(k, idx, param) {
  if (is.character(k)) {
    k <- if (param == "k") {
      reformat_k(k, only_positive = TRUE)
    } else {
      reformat_k(k, only_positive = FALSE)
    }

    from <- if (length(k) != 1) {
      if (length(idx) == 0) {
        stop(
          sprintf(
            "`idx` can't be empty while specifying `%s` as time interval",
            param
          )
        )
      }

      mapply(
        FUN = function(x, y) {
          seq(x, by = y, length.out = 2)[2]
        },
        x = idx,
        y = k
      )
    } else if (length(k) == 1) {
      if (length(idx) == 0) {
        stop(
          sprintf(
            "`idx` can't be empty while specifying `%s` as time interval",
            param
          )
        )
      }

      vapply(
        idx,
        function(x) seq(x, by = k, length.out = 2)[2],
        FUN.VALUE = double(1)
      )
    }

    return(as.integer(idx) - from)

  } else if (is(k, "difftime")) {
    k <- if (param == "k") {
      if (any(k < 0)) {
        stop("`k` can't be negative.")
      }
      abs(k)
    } else {
      k
    }

    if (length(idx) == 0) {
      stop(
        sprintf("`idx` can't be empty while specifying %s as difftime", param)
      )
    }
    from <- idx - k
    k <- as.integer(idx) - as.integer(from)
  }

  return(k)
}



#' Formats time-unit-interval to valid for runner
#'
#' Formats time-unit-interval to valid for runner. User specifies \code{k} as
#' positive number but this means that this interval needs to be substracted
#' from \code{idx} - because windows length extends window backwards in time.
#' The same situation for lag.
#' @param k (k or lag) object from runner to be formatted
#' @param only_positive for \code{k} is \code{TRUE},
#'   for \code{lag} is \code{FALSE}
#' @examples
#' runner:::reformat_k("1 days")
#' runner:::reformat_k("day")
#' runner:::reformat_k("10 days")
#' runner:::reformat_k("-10 days", only_positive = FALSE)
#' runner:::reformat_k(c("-10 days", "2 months"), only_positive = FALSE)
reformat_k <- function(k, only_positive = TRUE) {
  if (only_positive && any(grepl("^-", k))) {
    stop("k can't be negative")
  }

  k[grepl("^[a-zA-Z]", k)] <- sprintf("1 %ss", k[grepl("^[a-zA-Z]", k)])
  positive <- grepl("^[^-]", k)
  negative <- grepl("^-", k)

  k[positive] <- sprintf("-%s", k[positive])
  k[negative] <- gsub("^-", "", k[negative])

  return(k)
}


#' Creates sequence for at as time-unit-interval
#'
#' Creates sequence for at as time-unit-interval
#' @param at object from runner
#' @param idx object from runner
seq_at <- function(at, idx) { # nolint
  if (length(at) == 1 &&
      (
        (is.character(at) && is_datetime_valid(at)) ||
        is(at, "difftime")
      )
    ) {

    if (length(idx) == 0) {
      stop(
        sprintf("`idx` can't be empty while specifying `at` as time interval")
      )
    }


    if (inherits(idx, c("Date", "POSIXct", "POSIXxt", "POSIXlt"))) {
      at <- if ((is.character(at) && grepl("^-", at)) ||
                (is(at, "difftime") && at < 0)) {
        seq(max(idx), min(idx), by = at)
      } else {
        seq(min(idx), max(idx), by = at)
      }
    }
  }
  return(at)
}

set_from_attribute_at <- function(x, attrib) { #nolint
  runner_args <- get_runner_call_arg_names()
  arg_name <- deparse(substitute(attrib))

  # no arg overwriting
  if (!is.null(attr(x, arg_name)) && !arg_name %in% runner_args) {
    if (length(attr(x, arg_name)) == 1 &&
        is.character(attr(x, arg_name)) &&
        attr(x, arg_name) %in% names(x)) {

      attrib <- x[[attr(x, arg_name)]]
    } else if (is.character(attr(x, arg_name))) {
      stop(
        sprintf(
          "`%s` should be either:
         - column name of `x`
         - vector of type `numeric`, `Date`, `POSIXct` or `POSIXlt`
         - character value describing dates sequence step as in `by` argument of `seq.POSIXct`", #nolint
          arg_name
        ),
        call. = FALSE
      )
    } else {
      attrib <- attr(x, arg_name)
    }

    # arg overwriting (runner masks run_by)
  } else {
    if (!is.null(attr(x, arg_name))) {
      warning(
        sprintf(
          "`%1$s` set in run_by() will be ignored in favour of `%1$s` specified in runner() call", #nolint
          arg_name
        )
      )
    }

    if (is.character(attrib) && length(attrib) == 1 && attrib %in% names(x)) {
      attrib <- x[[attrib]]
    } else if (length(attrib) == 1 && all(is_datetime_valid(attrib))) {
      # do nothing
    } else if (is.numeric(attrib) ||
               inherits(attrib, c("Date", "POSIXct", "POSIXxt", "POSIXlt"))) {
      # do nothing
    } else {
      stop(
        sprintf(
          "`%s` should be either:
         - column name of `x`
         - vector of type `numeric`, `Date`, `POSIXct` or `POSIXlt`",
          arg_name
        ),
        call. = FALSE
      )
    }
  }

  return(attrib)
}


set_from_attribute_difftime <- function(x, attrib) { #nolint
  runner_args <- get_runner_call_arg_names()
  arg_name <- deparse(substitute(attrib))

  if (!is.null(attr(x, arg_name)) && !arg_name %in% runner_args) {
    #  - argument has not been specified so it can be overwritten
    if (length(attr(x, arg_name)) == 1 && attr(x, arg_name) %in% names(x)) {
      # attr is a variable name
      attrib <- x[[attr(x, arg_name)]]
    } else {
      # attr is a vector of values - length validation later
      attrib <- attr(x, arg_name)
    }

  } else if (arg_name %in% runner_args) {
    # - argument has been specified
    if (!is.null(attr(x, arg_name))) {
      warning(
        sprintf(
          "`%1$s` set in run_by() will be ignored in favour of `%1$s` specified in runner() call", #nolint
          arg_name
        )
      )
    }

    if (is.character(attrib)) {
      if (length(attrib) == 1 && attrib %in% names(x)) {
        # argument as variable name
        attrib <- x[[attrib]]
      } else if (all(is_datetime_valid(attrib))) {
        # argument as a difftime character
      } else {
        stop(
          sprintf(
            "`%s` is invalid, should be either:
           - column name of `x`
           - difftime class or character describing diffitme (see at argument in seq.POSIXt) #nolint
           - numeric or integer vector",
            arg_name
          ),
          call. = FALSE
        )
      }
    } else if (is.numeric(attrib) || is(attrib, "difftime")) {
      # do nothing
    } else {
      stop(
        sprintf(
          "`%s` is invalid, should be either:
           - column name of `x`
           - difftime class or character describing diffitme (see at argument in `seq.POSIXt`) #nolint
           - numeric or integer vector",
          arg_name
        ),
        call. = FALSE
      )
    }
  }

  return(attrib)
}

set_from_attribute_index <- function(x, attrib) { #nolint
  arg_name <- deparse(substitute(attrib))
  runner_args <- get_runner_call_arg_names()

  # No arg overwriting
  #  - attribute not empty and argument not specified
  if (!is.null(attr(x, arg_name)) && !arg_name %in% runner_args) {
    if (length(attr(x, arg_name)) == 1 &&
        is.character(attr(x, arg_name)) &&
        attr(x, arg_name) %in% names(x)) {

      attrib <- x[[attr(x, arg_name)]]
    } else if (is.character(attr(x, arg_name))) {
      stop(
        sprintf(
          "`%s` should be either:
         - column name of `x`
         - vector of type `numeric`, `Date`, `POSIXct` or `POSIXlt`",
          arg_name
        ),
        call. = FALSE
      )
    } else {
      attrib <- attr(x, arg_name)
    }

  # arg overwriting (runner masks run_by)
  } else {
    if (!is.null(attr(x, arg_name))) {
      warning(
        sprintf(
          "`%1$s` set in run_by() will be ignored in favour of `%1$s` specified in runner() call", # nolint
          arg_name
        )
      )
    }

    if (is.character(attrib) && length(attrib) == 1 && attrib %in% names(x)) {
      attrib <- x[[attrib]]
    } else if (is.numeric(attrib) ||
               inherits(attrib, c("Date", "POSIXct", "POSIXxt", "POSIXlt"))) {
      # do nothing
    } else {
      stop(
        sprintf(
          "`%s` should be either:
         - column name of `x`
         - vector of type `numeric`, `Date`, `POSIXct` or `POSIXlt`",
          arg_name
        ),
        call. = FALSE
      )
    }
  }

  return(attrib)
}

set_from_attribute_logical <- function(x, attrib) {
  runner_args <- get_runner_call_arg_names()
  arg_name <- deparse(substitute(attrib))

  # no arg overwriting
  if (!is.null(attr(x, arg_name)) && !arg_name %in% runner_args) {
    attrib <- attr(x, arg_name)

  # arg overwriting (runner masks run_by)
  } else if (!is.null(attr(x, arg_name))) {
    warning(
      sprintf(
        "`%1$s` set in run_by() will be ignored in favour of `%1$s` specified in runner() call",  #nolint
        arg_name
      )
    )
  }

  return(attrib)
}


#' Access group data in mutate
#'
#' Access group data in `dplyr::mutate` after
#' `dplyr::group_by`.
#' Function created because data available in `dplyr::group_by %>% mutate`
#' scheme is not filtered by group - in mutate function `.` is still initial
#' dataset. This function creates `data.frame` using `dplyr::groups`
#' information.
#' @param x (`data.frame`)\cr
#'   object which can be `grouped_df` in special case.
#' @md
#' @return data.frame filtered by current `dplyr::groups()`
this_group <- function(x) {
  attrs <- attributes(x)
  attrs <- attrs[names(attrs) != "row.names"]

  new_env <- new.env(parent = parent.frame(n = 2)$.top_env)
  df_call <- as.call(
    append(
      as.name("data.frame"),
      lapply(names(x), as.name)
    )
  )

  x <- eval(df_call, envir = new_env)
  for (i in seq_along(attrs)) {
    attr(x, names(attrs)[i]) <- attrs[[i]]
  }
  return(x)
}

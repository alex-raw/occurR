#' Association measures for collocation and collostruction
#'
#' Calculates association measures from vectors of frequencies and
#' joint frequencies.
#'
#' @param o11 numeric vector with joint frequencies
#' @param f1 numeric vector
#' @param f2 numeric, if not provided, sum of o11 is used
#' @param n numeric, if not provided, sum of f1 is used
#' @param fun character vector for built-in measures (see Details).
#' alternatively a custom function, expression, or call. if names are supplied,
#' they are used in the output
#' @param flip character names of measures for which to flip the sign, intended
#' to signify negative association with two-sided measures
#'
#' @return matrix
#' @details TODO:
#'
#' @export
collexemes <- function(o11, f1, f2 = sum(o11), n = NULL,
                       fun = "ll", flip = NULL) {
  min_n <- sum(f1 + f2)
  if (is.null(n)) n <- min_n

  vars <- extract_vars(fun, builtin_assoc())

  stopifnot(
    is.character(flip) || is.null(flip),
    "`n` cannot be less than the sum of f1 and f2" = n >= min_n,
    is.numeric(o11),
    is.numeric(f1),
    is.numeric(f2),
    is.numeric(n),
    length(n) == 1 || identical(length(f1), length(n)),
    identical(length(f1), length(o11)),
    all(o11 <= f1),
    all(o11 <= f2),
    all(o11 <= n)
  )

  list(f1 = f1, o11 = o11, f2 = f2, n = n) |>
    coll(vars, flip)
}

coll <- function(input, vars, flip = NULL) {
  ans <- eval_exprs(input, vars) |>
    withCallingHandlers(warning = \(w) w <<- w$message)

  if (w == "NAs produced by integer overflow") {
    warning("Coercing values to numeric due to integer overflow")
    ans <- eval_exprs(lapply(input, as.numeric), vars)
  }

  ans <- do.call(cbind, ans[attr(vars, "labels")])

  ans
}

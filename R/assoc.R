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
collexemes <- \(.x, ...) UseMethod("collexemes")

collexemes.data.frame <- function(.x, ...) {
  # TODO:
  collexemes.default(o11, f1, f2, n)
}

#' @export
collexemes.data.table <- function(.x, ...) {
  # TODO:
  collexemes.default(o11, f1, f2, n)
}

#' @export
collexemes.table <- function(.x, ...) {
  # TODO:
  collexemes.default(o11, f1, f2, n)
}

collexemes.default <- function(o11, f1, f2 = sum(o11), n = NULL,
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
    length(n)  == 1 || identical(length(f1), length(n)),
    length(f2) == 1 || identical(length(f1), length(f2)),
    identical(length(f1), length(o11)),
    all(o11 <= f1),
    all(o11 <= f2),
    all(o11 <= n)
  )

  list(f1 = f1, o11 = o11, f2 = f2, n = n) |>
    coll(vars, flip)
}

coll <- function(.x, vars, flip = NULL) {
  if (any(lengths(.x) == 0)) return(numeric(0))

  w <- ""
  ans <- eval_exprs(.x, vars) |>
    withCallingHandlers(warning = \(w) w <<- w$message) |>
    suppressWarnings()

  if (w == "NAs produced by integer overflow") {
    warning("Coercing values to numeric due to integer overflow")
    ans <- eval_exprs(lapply(.x, as.numeric), vars)
  }

  ans <- do.call(cbind, ans[attr(vars, "labels")])

  if (is.null(flip)) return(ans)

  repulsed <- .x$o11 < .x$f1 * .x$f2 / .x$n
  two_sided <- colnames(ans) %in% flip
  ans[repulsed, two_sided] <- -ans[repulsed, two_sided]
  ans
}


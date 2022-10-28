#' Association measures for collocation and collostruction
#'
#' Calculates association measures from vectors of frequencies and
#' joint frequencies.
#'
#' @param .x data.frame or list containing data
#' @param o11 numeric vector with joint frequencies
#' @param f1 numeric vector
#' @param f2 numeric, if not provided, sum of o11 is used
#' @param n numeric, if not provided, sum of f1 is used
#' @param fun character vector for built-in measures (see Details).
#' alternatively a custom function, expression, or call. if names are supplied,
#' they are used in the output
#' @param flip character names of measures for which to flip the sign, intended
#' to signify negative association with two-sided measures
#' @param ... further arguments to be passed to or from other methods
#'
#' @return matrix
#' @details TODO:
#'
#' @export
collexemes <- \(.x, ...) UseMethod("collexemes")

#' @rdname collexemes
#' @export
collexemes.data.frame <- function(.x, o11, f1, f2 = NULL, n = NULL, fun = "ll",
                                  flip = NULL, ...) {
  res <- coll(
    o11 = eval(substitute(o11), .x),
    f1 = eval(substitute(f1), .x),
    f2 = if (!is.null(f2)) eval(substitute(f2), .x),
    n = if (!is.null(n)) eval(substitute(n), .x),
    fun = fun,
    flip = flip
  )
  cbind(type = Filter(is.character, .x), res[, fun])
}

#' @rdname collexemes
#' @export
collexemes.matrix <- function(.x, o11, f1, f2 = NULL, n = NULL, fun = "ll",
                                  flip = NULL, ...) {
  res <- coll(
    o11 = .x[, eval(substitute(o11))],
    f1 = .x[, eval(substitute(f1))],
    f2 = if (!is.null(f2)) .x[, eval(substitute(f2))],
    n = if (!is.null(n)) .x[, eval(substitute(n))],
    fun = fun,
    flip = flip
  )
  # rownames(res) <- rownames(.x)
  # colnames(res) <- colnames(.x)
  res
}

#' @rdname collexemes
#' @export
collexemes.data.table <- function(.x, ...) {
  # TODO:
  coll(o11, f1, f2, n)
}

#' @rdname collexemes
#' @export
collexemes.default <- function(.x, o11, f1, f2 = NULL, n = NULL, fun = "ll",
                               flip = NULL, ...) {
  coll(o11 = o11, f1 = f1, f2 = f2, n = n, fun = fun, flip = flip, ...)
}

coll <- function(o11, f1, f2 = NULL, n = NULL, fun = "ll",
                               flip = NULL, ...) {
  if (is.null(f2)) f2 <- sum(o11)
  min_n <- sum(f1 + f2)
  if (is.null(n)) n <- min_n

  exprs <- builtin_assoc()
  if (is.character(fun)) check_funs(fun, exprs)

  stopifnot(
    is.character(flip) || is.null(flip),
    # "`n` cannot be less than the sum of f1 and f2" = n >= min_n,
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

  if (!length(f1)) return(numeric(0))

  w <- ""
  vars <- extract_vars(fun, exprs)
  .x <- list(f1 = f1, o11 = o11, f2 = f2, n = n)

  ans <- eval_exprs(.x, vars) |>
    withCallingHandlers(warning = \(w) w <<- w$message) |>
    suppressWarnings()

  if (w == "NAs produced by integer overflow") {
    warning("Coercing values to numeric due to integer overflow")
    ans <- eval_exprs(lapply(.x, as.numeric), vars)
  }

  ans <- do.call(cbind, ans[attr(vars, "labels")])

  if (is.null(flip)) return(ans)

  repulsed <- o11 < f1 * f2 / n
  two_sided <- colnames(ans) %in% flip
  ans[repulsed, two_sided] <- -ans[repulsed, two_sided]
  ans
}

utils::globalVariables(c("f1", "f2", "o11", "n"))

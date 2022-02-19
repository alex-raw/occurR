#' Vectorized calculation of association measures
#'
#' Calculates association measures from vectors of frequencies and
#' joint frequencies.
#'
#' @param f1 numeric
#' @param o11 numeric joint frequencies
#' @param f2 numeric, if not provided, sum of o11 is used
#' @param n numeric, if not provided, sum of f1 is used
#' @param fun character vector for built-in measures (see Details).
#' alternatively a custom function, expression, or call. if names are supplied,
#' they are used in the output
#'
#' @return matrix
#' @details coming soon ...
#'
#' @export
v_assoc <- function(f1, o11, f2 = NULL, n = NULL, fun = "ll") {
  if (is.null(f2)) f2 <- sum(o11)
  if (is.null(n)) n <- sum(f1)
  stopifnot(is.numeric(f1), is.numeric(o11), is.numeric(n), is.numeric(f2))
  check_funs(fun, builtin_assoc())

  if (any(o11 > f1) || any(o11 > f2) || any(o11 > n))
    warning("Some observed frequencies are too large")

  exprs <- switch(class(fun),
    "character"  = builtin_assoc()[fun],
    "function"   = as.expression(body(fun)),
    "call"       = as.expression(fun),
    "expression" = fun,
    stop("invalid type of `fun`: ", class(fun))
  )

  input <- list(f1 = f1, o11 = o11, f2 = f2, n = n)
  # assert_all_numeric(input)
  vars  <- sapply(all.vars(exprs), get_assoc_vars, input, simplify = FALSE)
  out   <- vapply(exprs, eval, numeric(length(f1)), vars)

  if (is.function(fun))     colnames(out) <- deparse(substitute(fun))
  if (!is.null(names(fun))) colnames(out) <- names(fun)
  out
}

coll <- function(x, o11 = NULL, n = NULL, f2 = NULL,
                 fun = "ll", decreasing = TRUE, one_sided = FALSE) {
  # generic function coming here for data.frames, data.tables and matrix input
}

# TODO: data.table method?
ll_dt <- function(x, n = sum(x$f1), f2 = sum(x$o11), fun = "ll",
                  one_sided = TRUE, sorted = TRUE) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package \"data.table\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  datatable.aware = TRUE
  `:=` <- data.table::`:=`

  # TODO: matrix output doesn't work
  x[, (fun) := data.frame(v_assoc(f1, o11, f2, n, fun))]
  if (isTRUE(one_sided)) x[o11 < e11, `:=`(assoc, -assoc)]
  if (isTRUE(sorted)) data.table::setorder(x, -assoc)
}

# due to NSE notes in R CMD check
o11 <- f1 <- assoc <- e11 <- NULL

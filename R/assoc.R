#'  Association measures for collocation and collostruction
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
coll <- function(f1, o11, f2 = sum(o11), n = sum(f1), fun = "ll") {
  stopifnot(is.numeric(f1), is.numeric(o11), is.numeric(n), is.numeric(f2))
  check_funs(fun, builtin_assoc())
  exprs <- get_fun(fun)

  if (any(o11 > f1) || any(o11 > f2) || any(o11 > n)) {
    stop("Joint frequencies cannot be larger than individual counts")
  }

  # out <- UseMethod("coll")

  input <- list(f1 = f1, o11 = o11, f2 = f2, n = n)
  vars <- sapply(all.vars(exprs), get_assoc_vars, input, simplify = FALSE)
  out <- vapply(exprs, eval, numeric(length(f1)), vars)

  if (is.function(fun)) colnames(out) <- deparse(substitute(fun))
  if (!is.null(names(fun))) colnames(out) <- names(fun)
  out
}

coll.default <- function() {
}

coll.data.frame <- function() {
}

coll.data.table <- function() {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package \"data.table\" needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  # datatable.aware <- TRUE
}

# due to NSE notes in R CMD check
o11 <- f1 <- assoc <- e11 <- NULL

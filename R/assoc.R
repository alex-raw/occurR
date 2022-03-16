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
#'
#' @return matrix
#' @details TODO:
#'
#' @export
coll <- function(o11, f1, f2 = sum(o11), n = NULL,
                 fun = "ll", neg_repulsed = FALSE) {
  min_n <- sum(f1 + f2)
  if (is.null(n)) n <- min_n

  # FIXME: decide how to handle NAs; currently throws error in this `if`
  if (n < min_n) stop("`n` cannot be less than the sum of f1 and f2")

  stopifnot(is.numeric(f1), is.numeric(o11), is.numeric(n), is.numeric(f2))
  stopifnot(identical(length(f1), length(o11)))

  if (any(o11 > f1) || any(o11 > f2) || any(o11 > n)) {
    stop("Joint frequencies cannot be larger than individual counts")
  }

  input <- list(f1 = f1, o11 = o11, f2 = f2, n = n)
  ans <- tryCatch(
    run_coll_funs(input, fun),
    warning = function(w) {
      run_coll_funs(lapply(input, as.numeric), fun)
    }
  )

  # TODO: sign swap if one-sided

  ans
}

run_coll_funs <- function(input, fun) {
  if (is.character(fun)) {
    check_funs(fun, builtin_assoc())
    fun <- builtin_assoc()[fun]
  }
  vars <- sapply(all.vars(fun), get_assoc_vars, input, simplify = FALSE)
  vapply(fun, eval, numeric(length(input$f1)), vars)
}

coll.default <- function() { # nolint
}

coll.data.frame <- function() { # nolint
}

coll.data.table <- function() { # nolint
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package \"data.table\" needed for this function to work.
         Please install it.", call. = FALSE
    )
  }
  datatable.aware <- TRUE # nolint
}

# due to NSE notes in R CMD check
o11 <- f1 <- assoc <- e11 <- NULL

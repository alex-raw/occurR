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

  # FIXME: decide how to handle NAs; currently throws error in this `if`
  if (n < min_n) stop("`n` cannot be less than the sum of f1 and f2")

  stopifnot(
    is.numeric(f1), is.numeric(o11), is.numeric(n), is.numeric(f2),
    is.null(flip) || is.character(flip),
    identical(length(f1), length(o11))
  )

  if (any(o11 > f1) || any(o11 > f2) || any(o11 > n)) {
    stop("Joint frequencies cannot be larger than individual counts")
  }

  exprs <- builtin_assoc()
  if (is.expression(fun)) {
    exprs <- c(exprs, fun)
    fun <- names(fun)
  }

  input <- list(f1 = f1, o11 = o11, f2 = f2, n = n)
  coll(input, exprs, fun, flip)
}

coll <- function(input, exprs, labels, flip) {
  ans <- tryCatch(
    eval_exprs(input, labels, exprs),
    warning = function(w) {
      ans <- eval_exprs(lapply(input, as.numeric), labels, exprs)
      message("Note: values coerced to numeric to prevent integer overflow")
      ans
    })

  ans <- do.call(cbind, ans[labels])

  if (is.character(flip)) {
    e11 <- (input$f1 * input$f2) / input$n
    ans <- flip_negative_assoc(ans, input$o11, e11, flip)
  }

  ans
}

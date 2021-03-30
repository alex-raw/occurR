build_args <- function(funs, val_fun, input) {
  fun_args <- unlist(lapply(funs, function(x) names(formals(x))))
  sapply(unique(fun_args), val_fun, input, simplify = FALSE)
}

# maths helper functions, cf. UCS/R ------------------------------------
reg_gamma_inv <- function(a, y, lower = TRUE, log = FALSE) {
  if (log == TRUE) y <- y * log(10)
  stats::qgamma(y, shape = a, scale = 1, lower.tail = lower, log = log)
}

reg_gamma <- function(a, x, lower = TRUE, log = FALSE) {
  ans <- stats::pgamma(x, shape = a, scale = 1, lower.tail = lower, log = log)
  if (log == TRUE) ans / log(10) else ans
}

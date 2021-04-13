make_one_sided <- function(assoc, o11, e11) {
  # negative values for negative association
  repulsed <- o11 < e11
  assoc[repulsed] <- -assoc[repulsed]
  assoc
}

zscore <- function(o11, e11) (o11 - e11) / sqrt(e11)

# maths helper functions, cf. UCS/R ------------------------------------
reg_gamma_inv <- function(a, y, lower = TRUE, log = FALSE) {
  if (log == TRUE) y <- y * log(10)
  stats::qgamma(y, shape = a, scale = 1, lower.tail = lower, log = log)
}

reg_gamma <- function(a, x, lower = TRUE, log = FALSE) {
  ans <- stats::pgamma(x, shape = a, scale = 1, lower.tail = lower, log = log)
  if (log == TRUE) ans / log(10) else ans
}

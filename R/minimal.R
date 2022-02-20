#' Calculate G² (log-likelihood test) from vectors
#'
#' Calculate loglikelihood association measures for vectors of counts.
#'
#' @param o11 numeric or integer with joint frequency
#' @param f1 numeric or integer with frequencies of token
#' @param f2 numeric or integer with frequencies of co-occurring token or
#' construction if not provided, sum of joint frequencies (o11) is taken
#' @param n numeric or integer with overall frequencies if not provided,
#' sum of f1 is taken
#' @param one_sided logical if `TRUE`, repulsion is indicated by a negative sign
#'
#' @return numeric
#' @examples
#' ll(c(this = 41, is = 32, corpus = 41, data = 12), c(12, 1, 0, 3))
#'
#' @export
ll <- function(f1, o11, n = sum(f1), f2 = sum(o11), one_sided = TRUE) {
  o <- cbind(o11,                 o12 = f2 - o11,
             o21 = f1 - o11,      o22 = n - f1 - f2 + o11)
  e <- cbind(e11 = f1 * f2,       e12 = (n - f1) * f2,
             e21 = f1 * (n - f2), e22 = (n - f1) * (n - f2)) / n
  ans <- 2 * rowSums(o * log(o / e), na.rm = TRUE)
  if (isTRUE(one_sided)) {
    repulsed <- o[, 1] < e[, 1]
    ans[repulsed] <- -ans[repulsed]
  }
  ans
}

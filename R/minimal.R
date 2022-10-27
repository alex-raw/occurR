#' Calculate G² (log-likelihood test) from vectors
#'
#' Calculate loglikelihood association measures for vectors of counts.
#'
#' @param f1 numeric or integer with frequencies of tokens
#' @param o11 numeric or integer with joint frequency
#' @param f2 numeric or integer with frequencies of co-occurring tokens or
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
ll <- function(o11, f1, n = sum(f1 + f2), f2 = sum(o11), one_sided = TRUE) {
  o <- cbind(o11 = o11,           o12 = f2 - o11,
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

#' Word Growth Dispersion
#'
#' A distance-based dispersion measure
#'
#' @param cpos integer vector representing the indeces of a token
#' @param f integer count of a token
#' @param size integer corpus size
#' @param corr logical whether or not to apply a geometric correction, see
#' Details
#'
#' @return numeric
#' @details TODO:
#' @examples
#' dwg(c(41, 32, 41, 12), f = 4, size = 100)
#'
#' @export
dwg <- function(cpos, f, size, corr = TRUE) { # word growth dispersion
  dd <- cpos - data.table::shift(cpos, fill = 0L) - (size / f)
  mad <- sum(abs(dd)) / f
  worst_mad <- (size - f + 1 - size / f) / (f / 2)
  ans <- mad / worst_mad
  if (isTRUE(corr)) {
    ans <- ans / (2 * atan(worst_mad) / atan(mad))
  }
  ans
}

#' Deviation of proportions
#'
#' TODO:
#'
#' @param v integer vector with per document frequencies
#' @param tokens character or factor
#' @param parts character or factor
#' @param norm logical whether or not to normalize, see Details
#'
#' @return numeric
#' @details TODO:
#' @examples
#' n <- 50
#' v <- sample(1:100, n, replace = TRUE)
#' tokens <- sample(letters, n, replace = TRUE)
#' parts <- sample(LETTERS[1:3], n, replace = TRUE)
#' dp(tokens, parts, v)
#'
#' @export
dp <- function(tokens, parts, v, norm = TRUE) {
  tokens <- as.factor(tokens)
  parts <- as.factor(parts)
  f <- rowsum(v, tokens)[tokens, ]
  s <- proportions(rowsum(v, parts))[parts, ]
  m <- rowsum(cbind(s, abs(v / f - s)), tokens, reorder = FALSE)
  ans <- (1 - m[, 1L] + m[, 2L]) / 2
  if (isTRUE(norm)) ans <- ans / (1 - min(s))
  ans
}

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
#' ll(c(12, 1, 0, 3), c(this = 41, is = 32, corpus = 41, data = 12))
#'
#' @export
ll <- function(o11, f1, n = sum(f1 + f2), f2 = sum(o11), one_sided = TRUE) {
  o <- cbind(o11 = o11,           o12 = f2 - o11,
             o21 = f1 - o11,      o22 = n - f1 - f2 + o11)
  e <- cbind(e11 = f1 * f2,       e12 = (n - f1) * f2,
             e21 = f1 * (n - f2), e22 = (n - f1) * (n - f2)) / n
  ans <- 2 * rowSums(o * log(o / e), na.rm = TRUE)
  if (one_sided) {
    repulsed <- o[, 1] < e[, 1]
    ans[repulsed] <- -ans[repulsed]
  }
  ans
}

#' Word Growth Dispersion
#'
#' A distance-based dispersion measure
#'
#' @param tokens character raw corpus
#' @param corr logical whether or not to apply a geometric correction
#'
#' @return numeric
#' @details TODO:
#' @examples
#' n <- 50
#' tokens <- sample(letters, n, replace = TRUE)
#' dwg(tokens)
#'
#' @export
dwg <- function(tokens, corr = TRUE) {
  vocab <- unique(tokens)
  itokens <- match(tokens, vocab)
  f <- tabulate(itokens)

  s <- sort.int(itokens, index.return = TRUE)
  sort_ids <- s$ix
  i <- s$x
  l <- length(tokens)

  d <- c(sort_ids[-1], l) - sort_ids
  last <- cumsum(f)
  first <- c(1, last[-length(last)] + 1)
  d[last] <- sort_ids[first] + n - sort_ids[last]

  mad <- rowsum(abs(d - l / f[i]), i)[, 1] / f
  worst_mad <- (l - f + 1 - l / f) / (f / 2)
  ans <- mad / worst_mad
  if (corr) {
    ans <- ans / (2 * atan(worst_mad) / atan(mad))
  }
  names(ans) <- vocab
  ans
}

#' Deviation of proportions
#'
#' TODO:
#'
#' @param v integer vector with per document frequencies
#' @param tokens character or factor of the same length as `v`
#' @param parts character or factor of the same length as `v`
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
  m <- rowsum(cbind(s, abs(v / f - s)), tokens)
  ans <- (1 - m[, 1L] + m[, 2L]) / 2
  if (norm) ans <- ans / (1 - min(s))
  ans
}

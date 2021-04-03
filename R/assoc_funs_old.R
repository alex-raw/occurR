#{{{ old
ll <- function(o, e)
  2 * rowSums(o * log10(o / e), na.rm = TRUE)

fisher_pv <- function(o11, c1, c2, r1)
  -log10(stats::phyper(o11 - 1, c1, c2, r1, lower.tail = FALSE))

z_score <- function(o11, e11)
  (o11 - e11) / sqrt(e11)

z_score_yates <- function(o11, e11)
  z_score(ifelse(o11 > e11, o11 - 0.5, o11 + 0.5), e11)

t_score <- function(o11, e11)
  (o11 - e11) / sqrt(o11)

poisson_stirling <- function(o11, e11)
  o11 * (log10(o11) - log10(e11) - 1)

chi_squared <- function(o11, e11, e22, n)
  (n * ((o11 - e11)^2)) / (e11 * e22)

chi_squared_i <- function(o, e)
  rowSums(((o - e)^2) / e, na.rm = TRUE)

chi_squared_h <- function(o11, o22, o12, o21, n, r1, r2, c1, c2)
  (n * (o11 * o22 - o12 * o21)^2) / (r1 * r2 * c1 * c2)

chi_squared_corr <- function(o11, o22, o12, o21, n, r1, r2, c1, c2)
  (n * (abs(o11 * o22 - o12 * o21) - n / 2)^2) / (r1 * r2 * c1 * c2)

mi <- function(o11, e11)
  log10(o11 / e11)

mi_squared <- function(o11, e11)
  log10(o11^2 / e11)

odds_ratio_plain <- function(o)
  log10((o[, 1] * o[, 4]) / (o[, 2] * o[, 3]))

odds_ratio <- function(o)
  odds_ratio_plain(o + 0.5)

rel_risk <- function(o11, o12, c1, c2)
  log10((o11 * c2) / (o12 * c1))

lidell <- function(n, o11, e11, c1, c2)
  (n * (o11 - e11)) / (c1 * c2)

min_sens <- function(o11, r1, c1) {
  one <- o11 / r1
  two <- o11 / c1
  ifelse(one < two, one, two)
}

gmean <- function(o11, n, e11)
  o11 / sqrt(n * e11)

dice <- function(o11, r1, c1)
  (2 * o11) / (r1 + c1)

jaccard <- function(o11, o12, o21, o22)
  o11 / (o11 + o12 + o21)

mi_conf <- function(o11, e11, alpha = 2)
  log10(reg_gamma_inv(o11, -alpha - log10(2), log = TRUE) / e11)

poisson_pv <- function(o11, e11)
  -reg_gamma(o11, e11, log = TRUE)

# cf. Evert (2004) & http://www.collocations.de
builtin_assoc <- function() {
  expression(
    f1    = f1,
    f2    = f2,
    n     = n,
    r1    = f1,
    c1    = f2,
    r2    = n - f1,
    c2    = n - f2,
    o11   = o11,
    o12   = f2 - o11,
    o21   = f1 - o11,
    o22   = n - f1 - f2 + o11,
    e11   = f1 * f2 / n,
    e12   = (n - f1) * f2 / n,
    e21   = (n - f2) * f1 / n,
    e22   = (n - f1) * (n - f2) / n,
    o     = cbind(o11, o12, o21, o22),
    e     = cbind(e11, e12, e21, e22),
    alpha = 2,

    ll = 2 * rowSums(o * log(o / e), na.rm = TRUE),
    mi = log10(o11 / e11),
    mi_base2 = log2(o11 / e11),
    mi2 = log10(o11^2 / e11),
    mi3 = log10(o11^3 / e11),
    mi_conf = log10(reg_gamma_inv(o11, -alpha - log10(2), log = TRUE) / e11), # TODO:
    fisher_pv = -log10(stats::phyper(o11 - 1, c1, c2, r1, lower.tail = FALSE)),
    poisson_pv = -reg_gamma(o11, e11, log = TRUE),
    t_score = (o11 - e11) / sqrt(o11),
    rel_risk = log10((o11 * c2) / (o12 * c1)),
    liddell = (n * (o11 - e11)) / (c1 * c2),
    gmean = o11 / sqrt(n * e11),
    dice = (2 * o11) / (r1 + c1),
    jaccard = o11 / (o11 + o12 + o21),
    zscore = (o11 - e11) / sqrt(e11),
    zscore_cor = zscore(ifelse(o11 > e11, o11 - 0.5, o11 + 0.5), e11),
    chisq = (n * ((o11 - e11)^2)) / (e11 * e22),
    chisq_i = rowSums(((o - e)^2) / e, na.rm = TRUE),
    chisq_h = n * (o11 * o22 - o12 * o21)^2 / prod(r1, r2, c1, c2),
    chisq_corr = (n * (abs(o11 * o22 - o12 * o21) - n / 2)^2) /
      prod(r1, r2, c1, c2),
    cramer = sqrt(chisq / n),
    min_sens = ifelse((o11 / r1) < (o11 / c1), o11 / r1, o11 / c1),
    poisson_stirling = o11 * (log10(o11) - log10(e11) - 1),
    odds_ratio = {
      o <- o + 0.5
      log10((o[, 1] * o[, 4]) / (o[, 2] * o[, 3]))
    },
    delta_p1 = o11 / r1 - o12 / r2,
    delta_p2 = o11 / c1 - o21 / c2#,
    # random = runif(length(o11), 0, 1)
    # fisher_pv_ln = "fye.ln"
  )
}

zscore <- \(o11, e11) (o11 - e11) / sqrt(e11)

# maths helper functions, cf. UCS/R
reg_gamma_inv <- function(a, y, lower = TRUE, log = FALSE) {
  if (log == TRUE) y <- y * log(10)
  stats::qgamma(y, shape = a, scale = 1, lower.tail = lower, log = log)
}

reg_gamma <- function(a, x, lower = TRUE, log = FALSE) {
  ans <- stats::pgamma(x, shape = a, scale = 1, lower.tail = lower, log = log)
  if (log == TRUE) ans / log(10) else ans
}

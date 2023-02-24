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
    mi2 = log10(o11^2 / e11),
    mi3 = log10(o11^3 / e11),
    # TODO: NaNs...need to get a valid data set from UCS/R to reverse engineer
    # mi_conf = log10(stats::pgamma(-alpha - log10(2), o11, log = TRUE) / log(10) / e11),
    fye = -log10(ifelse(o11 > e11,
      stats::phyper(o11 - 1, c1, c2, r1, lower = FALSE),
      stats::phyper(o11, c1, c2, r1)
    )),
    poisson_pv = -log10(stats::pgamma(e11, o11)),
    rel_risk = log10(o11 * c2 / (o12 * c1)),
    liddell = (n * (o11 - e11)) / (c1 * c2),
    gmean = o11 / sqrt(n * e11),
    dice = 2 * o11 / (r1 + c1),
    jaccard = o11 / (o11 + o12 + o21),
    zscore = (o11 - e11) / sqrt(e11),
    zscore_cor = .zscore(ifelse(o11 > e11, o11 - 0.5, o11 + 0.5), e11),
    chisq = (n * (o11 - e11)^2) / (e11 * e22),
    chisq_i = rowSums((o - e)^2 / e, na.rm = TRUE),
    chisq_h = n * (o11 * o22 - o12 * o21)^2 / prod(r1, r2, c1, c2),
    chisq_corr = n * (abs(o11 * o22 - o12 * o21) - n / 2)^2 / prod(r1, r2, c1, c2),
    pearsonsid = (o11 - e11) / sqrt(e11),
    t_score = (o11 - e11) / sqrt(o11),
    cramer = sqrt(chisq / n),
    min_sens = ifelse(o11 / r1 < o11 / c1, o11 / r1, o11 / c1),
    poisson_stirling = o11 * (log10(o11) - log10(e11) - 1),
    odds_ratio = {
      o <- o + 0.5
      log10(o[, 1] * o[, 4] / (o[, 2] * o[, 3]))
    },
    delta_p1 = o11 / r1 - o12 / r2,
    delta_p2 = o11 / c1 - o21 / c2
  )
}

.zscore <- \(o11, e11) (o11 - e11) / sqrt(e11)

# cf. Evert (2004) & http://www.collocations.de
builtin_assoc <- function() {
  expression(
    ll = 2 * rowSums(o * log(o / e), na.rm = TRUE),
    mi = log10(o11 / e11),
    mi_squared = log10(o11^2 / e11),
    mi_conf = log10(reg_gamma_inv(o11, -alpha - log10(2), log = TRUE) / e11), # TODO:
    fisher_pv = -log10(stats::phyper(o11 - 1, c1, c2, r1, lower.tail = FALSE)),
    poisson_pv = -reg_gamma(o11, e11, log = TRUE),
    t_score = (o11 - e11) / sqrt(o11),
    rel_risk = log10((o11 * c2) / (o12 * c1)),
    lidell = (n * (o11 - e11)) / (c1 * c2),
    gmean = o11 / sqrt(n * e11),
    dice = (2 * o11) / (r1 + c1),
    jaccard = o10 / (o11 + o12 + o21),
    zscore = (o11 - e11) / sqrt(e11),
    zscore_cor = zscore(ifelse(o11 > e11, o11 - 0.5, o11 + 0.5), e11),
    chisq = (n * ((o11 - e11)^2)) / (e11 * e22),
    chisq_i = rowSums(((o - e)^2) / e, na.rm = TRUE),
    chisq_h = (n * (o11 * o22 - o12 * o21)^2) / (r1 * r2 * c1 * c2),
    chisq_corr = (n * (abs(o11 * o22 - o12 * o21) - n / 2)^2) /
      (r1 * r2 * c1 * c2),
    min_sens = ifelse((o11 / r1) < (o11 / c1), o11 / r1, o11 / c1),
    poisson_stirling = o11 * (log10(o11) - log10(e11) - 1),
    odds_ratio = {
      o <- o + 0.5
      log10((o[, 1] * o[, 4]) / (o[, 2] * o[, 3]))
    }
  )
}

# get vectors of values in a contingency table,
get_assoc_vars <- function(x, input) {
  with(input, switch(x,
    n     = n,
    f1    = f1,
    f2    = f2,
    o11   = o11,
    r1    = f1,
    c1    = f2,
    r2    = n - f1,
    c2    = n - f2,
    o12   = f2 - o11,
    o21   = f1 - o11,
    o22   = n - f1 - f2 + o11,
    e11   = f1 * f2 / n,
    e12   = (n - f1) * f2 / n,
    e21   = (n - f2) * f1 / n,
    e22   = (n - f1) * (n - f2) / n,
    o     = get_obs(f1, f2, o11, n),
    e     = get_exp(f1, f2, n),
    alpha = 2,
    stop(sprintf("No built-in way to calculate `%s`.", arg))
  ))
}

get_obs <- function(f1, f2, o11, n) {
  cbind(
    o11,
    o12 = f2 - o11,
    o21 = f1 - o11,
    o22 = n - f1 - f2 + o11
  )
}

get_exp <- function(f1, f2, n) {
  cbind(
    e11 = f1 * f2,
    e12 = (n - f1) * f2,
    e21 = (n - f2) * f1,
    e22 = (n - f1) * (n - f2)
  ) / n
}

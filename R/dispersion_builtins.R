builtin_disp <- function() {
  expression(
    # i = token index
    N        = nlevels(i),              # number of unique types
    ids      = as_factor(parts),        # part index
    n        = nlevels(ids),
    sizes    = sum_by(ids, n, v),

    # vectorized values                 # v = count word in part
    s        = proportions(sizes)[ids], # % part in corpus
    p        = v / sizes[ids],          # % word in part
    f        = sum_by(i, N, v),         # count word in corpus
    v_rel    = v / f[i],
    f_mean   = f / n,

    # shared values
    p_sum    = sum_by(i, N, p),
    s_sum    = sum_by(i, N, s),
    f_sqrt   = sum_by(i, N, sqrt(v)),

    # measures from Gries 2019: Analyzing dispersion
    range    = tabulate(i),
    maxmin   = max_min0(v, i, n, range),
    engvall  = range * f_mean,
    idf      = log2(n / range),
    D        = juilland_d(p, n, range, p_sum, i, N),
    U        = D * f,
    sd_pop   = sd_pop(v, n, range, f_mean, i, N),
    cv_pop   = sd_pop / f_mean,
    D_eq     = 1 - cv_pop / sqrt(n - 1L),
    U_eq     = D_eq * f,
    D2       = carroll_d2(p, p_sum, i, n, N),
    Um       = (f * D2) + (1 - D2) * f_mean,
    f_R      = sum_by(i, N, sqrt(v * s))^2,
    S        = f_R / f,
    dc       = (f_sqrt / n)^2 / f_mean,
    f_R_eq   = f_sqrt^2 / n,
    S_eq     = f_R_eq / f,
    kld      = sum_by(i, N, v_rel * log2(v_rel / s)),
    kld_norm = 1 - 2^-kld,
    dp       = (1 - s_sum + sum_by(i, N, abs(v_rel - s))) / 2,
    dp_norm  = dp / (1 - min(s)),
    chisq    = chisq0(v, s, f, i, s_sum, N),
    D3       = 1 - chisq / (4 * f),
    Ur       = kromer(v, i, N)
  )
}

max_min0 <- function(x, group, n, range) {
  if (requireNamespace("collapse", quietly = TRUE)) {
    maxs <- collapse::fmax.default(x, group, use.g.names = FALSE, na.rm = FALSE)
    mins <- collapse::fmin.default(x, group, use.g.names = FALSE, na.rm = FALSE)
  } else {
    maxs <- tapply(x, group, max, na.rm = FALSE)
    mins <- tapply(x, group, min, na.rm = FALSE)
  }

  non_zero <- range >= n
  maxs[non_zero] <- maxs[non_zero] - mins[non_zero]
  maxs
}

chisq0 <- function(v, s, f, group, s_sum, N) { # nolint
  f_exp <- s * f[group]
  ((1 - s_sum) * f) + sum_by(group, N, (v - f_exp)^2 / f_exp)
}

carroll_d2 <- function(p, p_sum, group, n, N) { # nolint
  x <- p / p_sum[group]
  -sum_by(group, N, x * log2(x)) / log2(n)
}

kromer <- function(x, group, N) { # nolint
  .digamma <- if (requireNamespace("Rfast", quietly = TRUE))
    Rfast::Digamma else digamma

  sum_by(group, N, .digamma(x + 1) - .digamma(1))
}

sd_pop <- function(v, n, range, mean, group, N) { # nolint
  sqrt((sum_by(group, N, (v - mean[group])^2) + ((n - range) * mean^2)) / n)
}

juilland_d <- function(p, n, range, p_sum, group, N) { # nolint
  p_mean <- p_sum / n
  1 - sd_pop(p, n, range, p_mean, group, N) / p_mean / sqrt(n - 1L)
}

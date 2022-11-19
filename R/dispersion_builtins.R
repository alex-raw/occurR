.digamma <- if (requireNamespace("Rfast", quietly = TRUE))
  Rfast::Digamma else digamma

builtin_disp <- function() {
  expression(
    # d = distance to next token of same type (wrapping to first occurrence)
    # d_per_part = distances per part (without wrapping to first occurrence)
    tokens = corpus$tokens$itokens,    # token index whole corpus
    i = corpus$per_part$itokens,       # token index per part
    parts = corpus$tokens$iparts,      # parts part index whole corpus
    ip = corpus$per_part$iparts,       # part index per token
    N = nrow(corpus$lexicon),          # number of unique types
    n = corpus$ndocs,                  # number of parts
    f = as.numeric(corpus$lexicon$f),  # count word in corpus
    l = corpus$ntokens,                # corpus size
    v = as.numeric(corpus$per_part$v), # count word per part
    sizes    = sum_by(ip, n, v),       # TODO: move to corpus constructor?

    # vectorized values
    s         = proportions(sizes)[ip], # % part in corpus
    p         = v / sizes[ip],          # % word in part
    v_rel     = v / f[i],
    f_mean    = f / n,

    # shared values
    p_sum    = sum_by(i, N, p),
    s_sum    = sum_by(i, N, s),
    f_sqrt   = sum_by(i, N, sqrt(v)),
    sum_greater_one = {
      eq_one <- v == 1L
      v[eq_one] <- v[eq_one] - 1L
      sum_by(i, N, v)
    },

    # for distance-based measures
    sum_d_squ  = sum_by(tokens, N, d^2),
    sum_abs_d  = sum_by(tokens, N, abs(d - l / f[tokens])),
    sum_dlogd  = sum_by(tokens, N, d * log10(d)),
    sum_pmin_d = sum_by(tokens, N, pmin.int(d, l / f[tokens])),
    pnearest   = sum_by(tokens, N, 1 / nearest_neighbor(d_per_part)),

    f_R      = sum_by(i, N, sqrt(v * s))^2,
    abs_dev  = sum_by(i, N, abs(v_rel - s)),
    mad = sum_abs_d / f,
    worst_mad = (l - f + 1 - l / f) / (f / 2),

    # measures from Gries 2019: Analyzing dispersion
    Ur       = sum_by(i, N, .digamma(v + 1) - .digamma(1)),
    kld      = sum_by(i, N, v_rel * log2(v_rel / s)),

    range    = tabulate(i),
    maxmin   = max_min0(v, i, n, range),
    engvall  = range * f_mean,
    idf      = log2(n / range),
    D        = juilland_d(p, n, range, p_sum, i, N),
    U        = D * f,
    sd_pop   = .sd_pop(v, n, range, f_mean, i, N),
    cv_pop   = sd_pop / f_mean,
    D_eq     = 1 - cv_pop / sqrt(n - 1L),
    U_eq     = D_eq * f,
    D2       = carroll_d2(p, p_sum, i, n, N),
    Um       = f * D2 + (1 - D2) * f_mean,
    S        = f_R / f,
    dc       = (f_sqrt / n)^2 / f_mean,
    f_R_eq   = f_sqrt^2 / n,
    S_eq     = f_R_eq / f,
    kld_norm = 1 - 2^-kld,
    dp       = (1 - s_sum + abs_dev) / 2,
    dp_norm  = dp / (1 - min(s)),
    chisq    = chisq0(v, s, f, i, s_sum, N),
    D3       = 1 - chisq / (4 * f),

    # distance-based dispersions
    arf = f / l * sum_pmin_d,
    awt = (1 + sum_d_squ / l) / 2,
    f_awt =  l^2 / sum_d_squ, # see Savicky & Hlavacova, appears as sum_d_squ / l^2 in Gries 2008
    ald = sum_dlogd / l,
    f_ald = l * 10^-ald,
    washtell = pnearest / sum_greater_one / (2 * f / l),
    dwg = mad / worst_mad,
    dwg_norm = dwg / (2 * atan(worst_mad) / atan(mad))
  )
}

nearest_neighbor <- function(x) {
  num_inds <- which(is.finite(x))
  left <- x[num_inds]
  next_id <- left + num_inds
  right <- x[next_id]
  x[next_id] <- pmin.int(left, right)
  x
}

max_min0 <- function(x, group, n, range) {
  groups <- split.default(x, group)
  maxs <- vapply(groups, max, 1, na.rm = FALSE)
  mins <- vapply(groups, min, 1, na.rm = FALSE)

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

.sd_pop <- function(v, n, range, mean, group, N) { # nolint
  sqrt((sum_by(group, N, (v - mean[group])^2) + (n - range) * mean^2) / n)
}

juilland_d <- function(p, n, range, p_sum, group, N) { # nolint
  p_mean <- p_sum / n
  1 - .sd_pop(p, n, range, p_mean, group, N) / p_mean / sqrt(n - 1L)
}

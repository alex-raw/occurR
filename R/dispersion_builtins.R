.digamma <- if (requireNamespace("Rfast", quietly = TRUE))
  Rfast::Digamma else digamma

builtin_disp <- function() {
  expression(
    l = l,                         # corpus size
    i = i,                         # token index per part
    j = j,                         # part index per token
    f = as.numeric(f),             # count word in corpus
    v = as.numeric(v),             # count word per part
    N = max(i),                    # number of unique types in sample
    n = max(j),                    # number of parts in sample
    .I = rep.int(seq_len(N), f),   # sorted token index corpus
    sizes = sum_by(j, n, v),       # TODO: move to corpus constructor?

    # vectorized values
    s        = (sizes / sum(sizes))[j], # % part in corpus
    p        = v / sizes[j],          # % word in part
    v_rel    = v / f[i],
    f_mean   = f / n,

    # for distance-based measures
    sort_ids = order(itokens),
    diffs = c(sort_ids[-1L], l) - sort_ids,
    d = group_distances(sort_ids, diffs, f, l), # distance between tokens (wrapping)
    awt_sum  = sum_by(.I, N, d^2),
    mad = sum_by(.I, N, abs(d - l / f[.I])) / f,
    worst_mad = (l - f + 1 - l / f) / (f / 2),

    # shared sums
    p_sum    = sum_by(i, N, p),
    s_sum    = sum_by(i, N, s),
    f_sqrt   = sum_by(i, N, sqrt(v)),

    # measures from Gries 2019: Analyzing dispersion
    Ur       = sum_by(i, N, .digamma(v + 1) - .digamma(1)),
    kld      = sum_by(i, N, v_rel * log2(v_rel / s)),
    sd_pop   = sqrt((sum_by(i, N, (v - f_mean[i])^2) + (n - range) * f_mean^2) / n),
    maxmin   = max_min0(v, i, n, range),
    range    = tabulate(i, N),
    engvall  = range * f_mean,
    idf      = log2(n / range),
    U        = D * f,
    cv_pop   = sd_pop / f_mean,
    D_eq     = 1 - cv_pop / sqrt(n - 1L),
    U_eq     = D_eq * f,
    Um       = f * D2 + (1 - D2) * f_mean,
    S        = sum_by(i, N, sqrt(v * s))^2 / f,
    dc       = (f_sqrt / n)^2 / f_mean,
    f_R_eq   = f_sqrt^2 / n,
    S_eq     = f_R_eq / f,
    kld_norm = 1 - 2^-kld,
    dp       = (1 - s_sum + sum_by(i, N, abs(v_rel - s))) / 2,
    dp_norm  = dp / (1 - min(s)),
    D3       = 1 - chisq / (4 * f),
    chisq    = (1L - s_sum) * f + sum_by(i, N, {
      f_exp <- s * f[i]
      (v - f_exp)^2 / f_exp
    }),
    D = {
      p_mean <- p_sum / n
      D_sum <- sum_by(i, N, (p - p_mean[i])^2)
      1 - sqrt((D_sum + (n - range) * p_mean^2) / n) / p_mean / sqrt(n - 1L)
    },
    D2 = {
      carrol_p <- p / p_sum[i]
      -sum_by(i, N, carrol_p * log2(carrol_p)) / log2(n)
    },

    # distance-based dispersions
    arf = f / l * sum_by(.I, N, pmin.int(d, l / f[.I])),
    awt = (1 + awt_sum / l) / 2,
    f_awt =  l^2 / awt_sum, # see Savicky & Hlavacova, appears as awt_sum / l^2 in Gries 2008
    ald = sum_by(.I, N, d * log10(d)) / l,
    f_ald = l * 10^-ald,
    dwg = mad / worst_mad,
    dwg_norm = dwg / (2 * atan(worst_mad) / atan(mad)),
    washtell = {
      v1 <- v
      eq_one <- v1 == 1L
      v1[eq_one] <- v1[eq_one] - 1L
      sum_gt1 <- sum_by(i, N, v1)
      wash_sum <- 1 / group_distances(sort_ids, diffs, v, l, per_part = TRUE)
      sum_by(.I, N, wash_sum) / # distance between tokens per part (no wrapping)
        sum_gt1 / (2 * f / l)
    }
  )
}

max_min0 <- function(v, i, n, range) {
  # prevent expensive redundant as.factor within split
  ux <- seq_len(n)
  inds <- structure(match(i, ux), levels = ux, class = "factor")
  groups <- split.default(v, inds)
  maxs <- vapply(groups, max, 1, USE.NAMES = FALSE)

  non_zero <- range >= n
  mins <- vapply(groups[non_zero], min, 1, USE.NAMES = FALSE) |> suppressWarnings()
  maxs[non_zero] <- maxs[non_zero] - mins
  maxs
}

group_distances <- function(sort_ids, d, freq, n, per_part = FALSE) {
  last <- cumsum(freq)
  if (per_part) {
    d[last] <- Inf
    pmin.int(d, c(Inf, d[-length(d)])) # nearest neighbor
  } else {
    first <- c(1L, last[-length(last)] + 1L)
    d[last] <- sort_ids[first] + n - sort_ids[last]
    d
  }
}

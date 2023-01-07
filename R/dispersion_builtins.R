.digamma <- if (requireNamespace("Rfast", quietly = TRUE))
  Rfast::Digamma else digamma

builtin_disp <- function() {
  expression(
    # l,        # corpus size
    # i,        # token index per part
    # j,        # part index per token
    # sort_ids, # position in sorted (grouped) corpus
    # sizes,    # sizes of parts
    f = as.numeric(f),             # count word in corpus
    v = as.numeric(v),             # count word per part
    N = max(i),                    # number of unique types in sample
    n = max(j),                    # number of parts in sample
    .I = rep.int(seq_len(N), f),   # sorted token index corpus

    # vectorized values
    s        = (sizes / sum(sizes))[j], # % part in corpus
    p        = v / sizes[j],          # % word in part
    v_rel    = v / f[i],
    f_mean   = f / n,

    # for distance-based measures
    diffs = c(sort_ids[-1L], l) - sort_ids,
    d = wrap_distances(sort_ids, diffs, f, l), # distance between tokens (wrapping)
    awt_sum  = sum_by(.I, N, d^2),
    mad = sum_by(.I, N, abs(d - l / f[.I])) / f,
    worst_mad = (l - f + 1 - l / f) / (f / 2),

    # shared sums
    p_sum    = sum_by(i, N, p),
    s_sum    = sum_by(i, N, s),
    f_sqrt   = sum_by(i, N, sqrt(v)),

    # measures from Gries 2019: Analyzing dispersion
    range    = tabulate(i, N),
    idf      = log2(n / range),
    maxmin   = max_min0(v, i, n, range),
    engvall  = range * f_mean,
    dp       = (1 - s_sum + sum_by(i, N, abs(v_rel - s))) / 2,
    dp_norm  = dp / (1 - min(s)),
    kld      = sum_by(i, N, v_rel * log2(v_rel / s)),
    kld_norm = 1 - 2^-kld,
    sd_pop   = sqrt((sum_by(i, N, (v - f_mean[i])^2) + (n - range) * f_mean^2) / n),
    cv_pop   = sd_pop / f_mean,
    D_eq     = 1 - cv_pop / sqrt(n - 1L),
    U_eq     = D_eq * f,
    dc       = (f_sqrt / n)^2 / f_mean,
    S        = sum_by(i, N, sqrt(v * s))^2 / f,
    S_eq     = f_R_eq / f,
    f_R_eq   = f_sqrt^2 / n,
    chisq    = (1L - s_sum) * f + sum_by(i, N, {
      f_exp <- s * f[i]
      (v - f_exp)^2 / f_exp
    }),
    D3 = 1 - chisq / (4 * f),
    D = {
      p_mean <- p_sum / n
      D_sum <- sum_by(i, N, (p - p_mean[i])^2)
      1 - sqrt((D_sum + (n - range) * p_mean^2) / n) / p_mean / sqrt(n - 1L)
    },
    U = D * f,
    D2 = {
      carrol_p <- p / p_sum[i]
      -sum_by(i, N, carrol_p * log2(carrol_p)) / log2(n)
    },
    Um = f * D2 + (1 - D2) * f_mean,
    Ur = sum_by(i, N, .digamma(v + 1) - .digamma(1)),

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
      wash_sum <- 1 / nearest_neighbor(diffs, v)
      sum_by(.I, N, wash_sum) / # distance between tokens per part (no wrapping)
        sum_by(i, N, v1) / (2 * f / l)
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
  mins <- suppressWarnings(vapply(groups[non_zero], min, 1, USE.NAMES = FALSE))
  maxs[non_zero] <- maxs[non_zero] - mins
  maxs
}

nearest_neighbor <- function(d, v) {
  last <- cumsum(v)
  d[last] <- Inf
  pmin.int(d, c(Inf, d[-length(d)]))
}

wrap_distances <- function(sort_ids, d, f, n) {
  last <- cumsum(f)
  first <- c(1L, last[-length(last)] + 1L)
  d[last] <- sort_ids[first] + n - sort_ids[last]
  d
}

dist_to_prev <- function(bool) {
  inds <- which(bool)
  ln <- length(bool)
  if (!length(inds)) return(rep_len(NA_integer_, ln))
  d <- c(inds, ln)[-1L] - inds
  c(rep_len(NA_integer_, inds[1L]), sequence.default(d))
}

dist_to_nearest <- function(bool) {
  .rev <- seq.int(length(bool), 1L)
  pmin.int(
    dist_to_prev(bool),
    dist_to_prev(bool[.rev])[.rev],
    na.rm = TRUE
  )
}

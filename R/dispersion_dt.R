annotate_vals <- function(dt, s = TRUE, p = TRUE) {
  if (s) dt[, s := sum(v), by = part][, s := s / sum(dt$v)]
  if (p) dt[, p := v / s][, `:=`(
    p_sum = sum(p), f = sum(v)), by = word]
  else
    dt[, f := sum(v), by = word]
}

# bquote hack solves scoping problems with shared variable n in dt[, eval(x)]
pre_sum_expr <- function(n) bquote(`:=`(
  p      = v / s,
  f_sqrt = sqrt(v),
  f.R    = sqrt(v * s),
  kld    = (v / f) * log2(v / f / s),
  sd.pop = (v - f / .(n))^2,
  dp     = abs(v / f - s),
  chisq  = (v - s * f)^2 / (s * f),
  D      = (p - p_sum / .(n))^2,
  D2     = (p / p_sum) * log2(p / p_sum),
  Ur     = digamma(v + 1) + -digamma(1)
))

grouped_expr <- function(n) bquote(list(
  f      = f[1],
  range  = .N,
  maxmin = max(v) - fifelse(.N < .(n), 0, min(v)),
  p_sum  = p_sum[1],
  f_sqrt = sum(f_sqrt),
  sd.pop = sum(sd.pop),
  f.R    = sum(f.R),
  kld    = sum(kld),
  D      = sum(D),
  dp     = sum(dp),
  s      = sum(s),
  D2     = sum(D2),
  chisq  = sum(chisq),
  Ur     = sum(Ur)
))

post_sum_expr <- function(n) bquote(`:=`(
  f.R      = f.R^2,
  f.R.eq   = f_sqrt^2 / .(n),
  idf      = log2(.(n) / range),
  dp       = (dp + 1 - s) / 2,
  chisq    = chisq + (1 - s) * f,
  D2       = -D2 / log2(.(n)),
  kld.norm = 1 - 2^-kld,
  sd.pop   = sqrt((sd.pop + ((.(n) - range) * (f / .(n))^2)) / .(n)),
  D        = sqrt((D + ((.(n) - range) * (p_sum / .(n))^2)) / .(n)),
  f_mean   = f / .(n)
))

norm_expr <- function(n) bquote(`:=`(
  dc      = (f_sqrt / .(n))^2 / f_mean,
  dp.norm = dp / (1 - min(s)),
  engvall = range * f_mean,
  Um      = f * D2 + (1 - D2) * f_mean,
  vc.pop  = sd.pop / f_mean,
  D_eq    = 1 - sd.pop / f_mean / sqrt(.(n) - 1),
  D       = 1 - D / (p_sum / .(n)) / sqrt(.(n) - 1),
  S       = f.R / f,
  S.eq    = f.R.eq / f,
  D3      = 1 - chisq / (4 * f),
  f_mean  = NULL, s = NULL, f_sqrt = NULL, p_sum = NULL
))

dispersion_dt <- function(x, n = uniqueN(x$part))
  annotate_vals(x, T, T
    )[, eval(pre_sum_expr(n))
    ][, eval(grouped_expr(n)), by = word
    ][, eval(post_sum_expr(n))
    ][, eval(norm_expr(n))
    ][, `:=`(U = D * f, U.eq = D_eq * f)
][]

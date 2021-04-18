# math helpers
max_min0 <- function(x, group, n, range)
  collapse::fmax.default(x, group) -
    ifelse(range < n, 0, collapse::fmin.default(x, group))

chisq0 <- function(v, s, f, group, s_sum) {
  f_exp <- s * f[group]
  ((1 - s_sum) * f) + sum_by(group, (v - f_exp)^2 / f_exp)
}

carroll_d2 <- function(p, p_sum, group, n) {
    x <- p / p_sum[group]
    -sum_by(group, x * log2(x)) / log2(n)
}

juilland_d <- function(p, n, range, p_sum, group) {
  p_mean <- p_sum / n
  1 - sd_pop(p, n, range, p_mean, group) / p_mean / sqrt(n - 1)
}

kromer <- function(x, group)
  sum_by(group, Rfast::Digamma(x + 1) - Rfast::Digamma(1))

sd_pop <- function(v, n, range, rel, group)
  sqrt((sum_by(group, (v - rel[group])^2) + ((n - range) * rel^2)) / n)

sd_inner <- function(tokens, v, rel, zeros)
  sum_by(tokens, (v - rel[tokens])^2) + zeros * rel^2

# meta
builtin_disp <- function() expression(
  ids = as_factor(parts), n = nlevels(ids), sizes = sum_by(ids, v),

  # vectorized values               # v = count in part
  f      = sum_by(words, v),        # count word in corpus
  s      = proportions(sizes)[ids], # % part in corpus
  p      = v / sizes[ids],          # % word in part
  v_rel  = v / f[words],
  f_mean = f / n,

  # shared values
  p_sum  = sum_by(words, p),
  s_sum  = sum_by(words, s),
  f_sqrt = sum_by(words, sqrt(v)),
  range  = tabulate(words),

  # measures from Gries 2019: Analyzing dispersion
  maxmin   = max_min0(v, words, n, range),
  engvall  = range * f_mean,
  Ur       = kromer(v, words),
  idf      = log2(n / range),
  dc       = (f_sqrt / n)^2, # BUG: something is off here
  sd.pop   = sd_pop(v, n, range, f_mean, words),
  vc.pop   = sd.pop / f_mean,
  D.eq     = 1 - vc.pop / sqrt(n - 1),
  U.eq     = D.eq * f,
  kld      = sum_by(words, v_rel * log2(v_rel / s)),
  kld.norm = 1 - 2^-kld,
  f.R      = sum_by(words, sqrt(v * s))^2,
  S        = f.R / f,
  f.R.eq   = f_sqrt^2 / n,
  S.eq     = f.R.eq / f,
  D        = juilland_d(p, n, range, p_sum, words),
  U        = D * f,
  D2       = carroll_d2(p, p_sum, words, n),
  Um       = (f * D2) + (1 - D2) * f_mean,
  dp       = (1 - s_sum + sum_by(words, abs(v_rel - s))) / 2,
  dp.norm  = dp / (1 - min(s)),
  chisq    = chisq0(v, s, f, words, s_sum),
  D3       = 1 - chisq / (4 * f)
)

# meta-helpers
recurse_vars <- function(funs, exprs)
  if (identical(funs, out <- union(all.vars(exprs[funs]), funs)))
    return(out) else recurse_vars(out, exprs)

calculate_disp <- function(start_vals, funs = all_measures(), exprs = builtin_disp()) {
  funs <- intersect(recurse_vars(funs, exprs), names(exprs))
  exprs <- exprs[funs]
  start_vals[funs] <- 0L
  for (i in names(exprs))
    start_vals[[i]] <- eval(exprs[[i]], start_vals)
  start_vals
}

# core helpers
sum_by <- function(f, x) collapse::fsum.default(x, f, use.g.names = FALSE)

as_factor <- function(x) {
  if (is.factor(x)) return(x)
  uniqx <- collapse::funique(x)
  factorcpp(x, uniqx)
}

#' @export
dispersion <- function(freqs, tokens, parts, funs = "dp.norm") {
  stopifnot(identical(length(freqs), length(tokens), length(parts)))

  tokens <- as_factor(tokens)
  input <- list(v = freqs, words = tokens, parts = parts)

  if (anyNA(input, recursive = TRUE)) warning("NA values in input.")

  exprs <- builtin_disp()

  if (is.character(funs) &
      length(mismatch <- funs[!funs %in% names(exprs)]) > 0L)
    stop("No built-in measure named: ", mismatch,
         "; see available_measures(\"disp\")")

  ans <- calculate_disp(input, funs, exprs)[c("f", funs)]

  if (!is.null(names(funs))) names(ans) <- names(funs)
  data.frame(types = levels(tokens), ans,
             check.names = FALSE, fix.empty.names = FALSE)
}

# @export
all_measures <- function() names(builtin_disp())[- (1:12)]


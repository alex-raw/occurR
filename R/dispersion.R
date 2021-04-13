sum_by <- function(group, x)
  as.numeric(rowsum.default(x, group))

sd_pop <- function(v, n, range, rel, words)
  sqrt((sum_by(words, (v - rel[words])^2) + ((n - range) * rel^2)) / n)

builtin_disp <- function() expression(
    n = nlevels(parts),

    # vectorized values
    sizes    = sum_by(parts, v),
    s        = proportions(sizes)[parts],  # % part in corpus
    p        = v / sizes[parts],           # % word in part
    v_rel    = v / f[words],               # % word in part
    f_mean   = f / n,                      # % word in corpus
    sum_prop = sum_by(words, p),
    p_mean   = sum_prop / n,               # % part in corpus
    f_sqrt   = sum_by(words, sqrt(v))^2,   # squared sum of roots

    # Gries 2019: Analyzing dispersion
    range    = tabulate(words),
    maxmin   = tapply(v, words, max) - ifelse(range < n, 0, tapply(v, words, min)), # TODO: slow!
    engvall  = range * f_mean,
    idf      = log2(n / range),
    dc       = f_sqrt / (n * f),
    Ur       = sum_by(words, digamma(v + 1) - digamma(1)),
    kld      = sum_by(words, v_rel * log2(v_rel / s)),
    f.R      = sum_by(words, sqrt(v * s))^2,
    f.R.eq   = f_sqrt / n,
    S        = f.R / f,
    S.eq     = f.R.eq / f,
    sd.pop   = sd_pop(v, n, range, f_mean, words),
    vc.pop   = sd.pop / f_mean,
    D_eq     = 1 - vc.pop / sqrt(n - 1),
    D        = 1 - sd_pop(p, n, range, p_mean, words) / p_mean / sqrt(n - 1),
    U.eq     = D_eq * f,
    U        = D * f,
    dp       = {
      x <- rowsum(cbind(s, abs(v_rel - s)), words)
      (1 - x[, 1L] + x[, 2L]) / 2
    },
    D2       = {
      x <- p / sum_prop[words]
      -sum_by(words, x * log2(x)) / log2(n)
    },
    chisq    = {
      f_exp <- s * f[words]
      x <- rowsum(cbind(s, (v - f_exp)^2 / f_exp), words)
      ((1 - x[, 1L]) * f) + x[, 2L]
    },
    Um       = (f * D2) + (1 - D2) * f_mean,
    kld.norm = 1 - 2^-kld,
    dp.norm  = dp / (1 - min(s)),
    D3       = 1 - chisq / (4 * f)
)

recurse_vars <- function(funs, exprs)
  if (identical(funs, out <- union(all.vars(exprs[funs]), funs)))
    return(out) else recurse_vars(out, exprs)

calculate_disp <- function(funs, start_vals, exprs = builtin_disp()) {
  funs <- intersect(recurse_vars(funs, exprs), names(exprs))
  exprs <- exprs[funs]
  start_vals[funs] <- 0L
  for (i in names(exprs))
    start_vals[[i]] <- eval(exprs[[i]], start_vals)
  start_vals
}

dispersion <- function(v, words, parts, funs = "dp.norm") {
  stopifnot(is.numeric(v), identical(length(v), length(words), length(parts)))

  input <- list(
    v = as.numeric(v),
    words = as.factor(words),
    parts = as.factor(parts),
    f = sum_by(words, v)
  )

  if (anyNA(input, recursive = TRUE)) warning("NA values in input.")

  exprs <- builtin_disp()

  if (is.character(funs) &
      length(mismatch <- funs[!funs %in% names(exprs)]) > 0L)
    stop("No built-in measure named: ", mismatch,
         "; see available_measures(\"disp\")")

  out <- calculate_disp(funs, input, exprs)[funs]
  if (!is.null(names(funs))) names(out) <- names(funs)

  data.frame(
    word = levels(words),
    freq = input$f,
    out,
    row.names = NULL
  )
}

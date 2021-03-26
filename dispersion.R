# Gries 2019: Analyzing dispersion
sum_by <- function(group, x) rowsum(x, group)[, 1]

build_args <- function(funs, v, f, words, parts) {
# calculate values to use in FUN; returns list to use in call
  arg_names <- unique(unlist(lapply(funs, formalArgs)))
  n <- nlevels(parts)
  sapply(arg_names, function(x) {
    switch(x, words = words,
      v      = v,              # frequency of word in each part
      f      = f,              # overall frequency of word in corpus
      l      = sum(v),         # corpus size in tokens
      n      = n,              # number of parts
      n_w    = table(words),   # number of parts word occurs in
      # TODO: speed improv. with tabulate()?
      rel    = f / n,
      f_sqrt = sum_by(words, sqrt(v)),
      # percentages word makes up of each corpus part (vectorized)
      p = v / sum_by(parts, v)[parts],
      # percentages of the n corpus part sizes (vectorized)
      s = proportions(sum_by(parts, v))[parts],
      stop(paste0("No built-in way to calculate `", arg, "`.")))
  }, simplify = FALSE)
}

dispersion <- function(v, words, parts, fun) {
  f <- sum_by(words, v)
  if (!is.list(fun)) fun <- list(fun)
  input_vals <- build_args(fun, v, f, words, parts)
  disp <- sapply(fun, function(x) {
    do.call(x, input_vals[names(formals(x))])
    })
  data.frame(words = rownames(disp), freq = f, disp)
}

part_range <- function(n_w)      n_w
idf        <- function(n, n_w)   log2(n / n_w)
engvall    <- function(rel, n_w) n_w * rel
dc   <- function(f_sqrt, n, rel) (f_sqrt / n)^2 / rel
kromer_ur  <- function(v, words) sum_by(words, digamma(v + 1) - digamma(1))

rosengren_f_eq   <- function(f_sqrt, n)      f_sqrt^2 / n
rosengren_f_uneq <- function(v, s, words)    sum_by(words, sqrt(v * s))^2
rosengren_s_eq   <- function(f_sqrt, n, f)   rosengren_f_eq(f_sqrt, n) / f
rosengren_s_uneq <- function(v, s, f, words) rosengren_f_uneq(v, s, words) / f

dp <- function(v, s, f, words) {
  ans <- rowsum(cbind(s, abs(v / f[words] - s)), words)
  (1 - ans[, 1L] + ans[, 2L]) / 2
}

dp_norm <- function(v, s, f, words) dp(v, s, f, words) / (1 - min(s))

kld <- function(v, s, f, words) {
  x <- v / f[words]
  sum_by(words, x * log2(x / s))
}

kld_norm  <- function(v, s, f) 1 - 2^-kld(v, s, f)

carroll_d2 <- function(p, n, words) {
  x <- p / sum_by(words, p)[words]
  -sum_by(words, x * log2(x)) / log2(n)
}

carroll_um <- function(p, n, f, words) {
  d2 <- carroll_d2(p, n, words)
  (f * d2) + (1 - d2) * (f / n)
}

maxmin <- function(v, n_w, n, words) { # TODO: slow!
  tapply(v, words, max) - ifelse(n_w < n, 0, tapply(v, words, min))
}

# standard deviation-based ----------------------------------------------------
sd_pop <- function(v, n, n_w, rel, words)
  sqrt((sum_by(words, (v - rel[words])^2) + ((n - n_w) * rel^2)) / n)

vc <- function(v, n, n_w, rel, words)
  sd_pop(v, n, n_w, rel, words) / rel

juilland_d_eq <- function(v, n, n_w, f, words)
  1 - (vc(v, n, n_w, f, words) / sqrt(n - 1))

juilland_d_uneq <- function(p, n, n_w, words)
  juilland_d_eq(p, n, n_w, sum_by(words, p), words)

juilland_u_eq <- function(v, n, n_w, f, words)
  juilland_d_eq(v, n, n_w, f, words) * f

juilland_u_uneq <- function(p, n, n_w, f, words)
  juilland_d_eq(p, n, n_w, sum_by(words, p), words) * f

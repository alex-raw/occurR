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
      # TODO: speed improv. with tabulate(); handle names
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

# -----------------------------------------------------------------------------

# TODO: haven't found non-trivial way to get s for which v = 0 per word
# chi_squared <- function(v, s, f) {
#   # ??? s_not -> s for which word doesn't appear
#   sum_by_word(((v - s * f[words])^2) / (s * f[words]))
#   sum_by_word(s_not * f)
# }
# lyne_d3 <- function(v, s, f)
#   1 - chi_squared(v, s, f) / (4 * f)

# # distance-based measures from SavickÃ½ and HlavÃ¡covÃ¡
# wheres.which <- which(wheres)
# distances <- diff(c(0, wheres.which + (length(corpus) - max(wheres.which))))
# segments <- rep(l / f, f)

# f_arf <- function(l, f, distances, segments)
#   sum(pmin(distances, segments)) / (l / f)

# awt <- function(l, distances)
#   0.5 * (1 + (1 / l * sum(distances^2)))

# f_awt <- function(l, distances)
#   l / ((2 * awt(l, distances)) - 1) # l ^ 2 / sum(distances^2)

# ald <- function(l, distances)
#   sum(distances * log10(distances)) / l

# f_ald <- function(l, distances)
#   l * 10 ^ (-ald(l, distances)) # exp(-sum(distances / l * log(distances / l)))

# # Washtell
# # table(words) == 1

# denominator <- (2 * f) / l
# candidates <- rownames(frequencies.of.element.in.corpus.parts.df)[v > 1]
# if (length(candidates) > 0) {
#   g <- sum(v[v > 1])
#   reduced.corpus <- corpus[corpus.parts %in% candidates]
#   reduced.parts <- corpus.parts[corpus.parts %in% candidates]
#   within.part.distances <- tapply(reduced.corpus, reduced.parts,
#                                   function(x) which(x == element))
#   min.dists <- unlist(sapply(within.part.distances, min.distance.finder))
#   numerator <- (1 / g) * sum(1 / min.dists)
#   "Washtell's Self Dispersion" <- numerator / denominator
# } else {
#   "Washtell's Self Dispersion" <- NA
# }

# dp <- function(v, s, f) sum(abs((v / f) - s)) / 2
# dp_norm <- function(v, s, f) dp(v, s, f) / (1 - min(s))
# kromer_ur <- function(v) sum(digamma(v + 1) - digamma(1))
# part_range <- function(v) sum(v > 0)
# rosengren_s_eq <- function(v, n, f) ((sum(sqrt(v))^2) / n) / f
# rosengren_s_uneq <- function(v, s, f) sum(sqrt(v * s))^2 / f
# rosengren_adj_freq_eq <- function(v, n) (sum(sqrt(v))^2) / n
# rosengren_adj_freq_uneq <- function(v, s) sum(sqrt(v * s))^2
# dc <- function(v, n) ((sum(sqrt(v)) / n)^2) / mean(v)
# idf <- function(v, n) log2(n / sum(v > 0))
# engvall <- function(v, n, f) f * (sum(v > 0) / n)
# carroll_d2 <- function(p, n) {
#   ans <- p / sum(p)
#   ans <- ans[ans > 0]
#   -sum(ans * log2(ans)) / log2(n)
# }
# carroll_um <- function(p, n, f) {
#   d2 <- carroll_d2(p, n)
#   (f * d2) + (1 - d2) * (f / n)
# }
# kld <- function(v, s, f) {
#   post_true <- v / f
#   logs <- log2(post_true / s)
#   logs[logs == -Inf] <- 0
#   sum(post_true * logs) # could be normalized as 1-2^(-kld)
# }
# sd_pop <- function(v)
#   sd(v) * sqrt((length(v) - 1) / length(v))

# vc <- function(v)
#   sd_pop(v) / mean(v)

# juilland_d_eq <- function(v, n)
#   1 - (vc(v) / sqrt(n - 1))

# juilland_u_eq <- function(v, n, f)
#   f * juilland_d_eq(v, n)

# juilland_d_uneq <- function(v, n, p)
#   1 - ((sd_pop(v) / mean(p)) / sqrt(n - 1))

# juilland_u_uneq <- function(v, n, p)
#   f * juilland_d_uneq(v, n, p)

# chi_squared <- function(v, s, f)
#   sum(((v - (s * f))^2) / (s * f))

# lyne_d3 <- function(v, s, f)
#   1 - chi_squared(v, s, f) / (4 * f)

# x <- with(na.omit(df), Matrix::sparseMatrix(
#   as.numeric(words),
#   as.numeric(parts),
#   x = Freq,
#   dimnames = list(levels(words),
#                   levels(parts)), repr = "C"))
# dp.matrix <- function(x) {
#   # for a cross tabulated matrix: rows = words, cols = parts
#   # matrices produced are too big for, e.g. BNC
#   dp <- colSums(abs(t(x / rowSums(x)) - (
#   s <- proportions(colSums(x))))) / 2
#   dp / (1 - min(s))
# }

# rauhut <- function(words, v, parts) {
#    f <- rowsum(v, words)[words, ]
#    s <- proportions(rowsum(v, parts))[parts, ]
#    m <- cbind(s, abs(v / f - s))
#    m <- rowsum(m, words, reorder = FALSE)
#    m[, 1L] <- 1 - m[, 1L]
#    (rowSums(m) / 2) / (1 - min(s))
# }

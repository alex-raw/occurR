# library(data.table)
# # load_all()

# rauhut <- function(words, v, parts) {
#    f <- rowsum(v, words)[words, ]
#    s <- proportions(rowsum(v, parts))[parts, ]
#    m <- cbind(s, abs(v / f - s))
#    m <- rowsum(m, words, reorder = FALSE)
#    m[, 1L] <- 1 - m[, 1L]
#    (rowSums(m) / 2) / (1 - min(s))
# }
# # cwb-decode:      1m27s + 18s fread and .N by word/part
# # cwb-scan-corpus: 0m46s +

# microbenchmark::microbenchmark(
# lel <- fread("bnc_word_id_full",
#     select = 1:2,
#     col.names = c("word", "part"),
#     sep = "\t",
#     quote = "",
#     na.string = NULL,
#     fill = TRUE)[,
#     .N, by = .(word, part)]
# ,
# times = 1L
# )

# microbenchmark::microbenchmark(
# lal <- fread("bnc_word_id",
#   col.names = c("freq", "word", "part"),
#   key = c("word", "part"),
#   sep = "\t",
#   quote = NULL,
#   na.string = "",
#   # stringsAsFactors = TRUE,
#   fill = TRUE)
# , times = 1L
# )

# lal[word == "FACTSHEET", ]
# lol[word == "FACTSHEET", ]

# # -----------------------------------------------------------------------------

# # TODO: haven't found non-trivial way to get s for which v = 0 per word
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

# get_dispersion_vars <- function(x) {
#   switch(x, words = words, v = v, f = f, n = n,
#     s = proportions(sum_by(parts, v))[parts], # % part in corpus (vectorized)
#     p = v / sum_by(parts, v)[parts],          # % word in part   (vectorized)
#     rel = f / n,                              # % word in corpus
#     n_w = table(words),                       # number of parts with word
#     f_sqrt = sum_by(words, sqrt(v)),          # sums of square roots per part
#     stop(paste0("No built-in way to calculate `", arg, "`."))
#   )
# }

# formulae <- expression(
#   part_range  = sum(v > 0),
#   dp          = sum(abs((v / f) - s)) / 2,
#   dp_norm     = dp(v, s, f) / (1 - min(s)),
#   vc          = sd_pop(v) / mean(v),
#   dc          = ((sum(sqrt(v)) / n)^2) / mean(v),
#   idf         = log2(n / sum(v > 0)),
#   sd_pop      = sd(v) * sqrt((length(v) - 1) / length(v)),
#   engvall     = f * (sum(v > 0) / n),
#   kromer_ur   = sum(digamma(v + 1) - digamma(1)),
#   j_d_eq      = 1 - (vc(v) / sqrt(n - 1)),
#   j_u_eq      = f * juilland_d_eq(v, n),
#   j_d_uneq    = 1 - ((sd_pop(v) / mean(p)) / sqrt(n - 1)),
#   j_u_uneq    = f * juilland_d_uneq(v, n, p),
#   r_s_eq      = ((sum(sqrt(v))^2) / n) / f,
#   r_s_uneq    = sum(sqrt(v * s))^2 / f,
#   r_adj_eq    = (sum(sqrt(v))^2) / n,
#   r_adj_uneq  = sum(sqrt(v * s))^2,
#   chi_squared = sum(((v - (s * f))^2) / (s * f)),
#   lyne_d3     = 1 - chi_squared(v, s, f) / (4 * f),
#   carroll_d2  = {
#     ans <- p / sum(p)
#     ans <- ans[ans > 0]
#     -sum(ans * log2(ans)) / log2(n)
#   },
#   carroll_um  = {
#     d2 <- carroll_d2(p, n)
#     (f * d2) + (1 - d2) * (f / n)
#   },
#   kld = {
#     post_true <- v / f
#     logs <- log2(post_true / s)
#     logs[logs == -Inf] <- 0
#     sum(post_true * logs) # could be normalized as 1-2^(-kld)
#   }
# )

# f <- c(1, 2, 3)
# n <- 10
# v <- 10000
# test_fun <- c("engvall", "kromer_ur")

# get_vars <- function(input, funs, val_fun)
# expr <- build_args(formulae[test_fun], get_dispersion_vars)
# get_formulae()[test_fun]

# with(args_, lapply(expr, eval))

# build_args <- function(funs, val_fun) {
#   fun_args <- unlist(lapply(funs, all.vars))
#   sapply(unique(fun_args), val_fun, simplify = FALSE)
# }

# freq_list <- read.delim("../blog/test_data/brown_word_id",
#   quote = "",
#   na.strings = "",
#   header = FALSE,
#   fill = TRUE,
#   colClasses = c("numeric", "factor", "factor"),
#   col.names = c("Freq", "words", "parts")
# )

# lol <- with(freq_list, dispersion(Freq, words, parts, c("dp", "dp_norm")))
# lol <- with(freq_list, dispersion(Freq, words, parts, names(get_formulae())
# ))

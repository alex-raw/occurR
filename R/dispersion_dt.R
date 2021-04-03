# library(data.table)

# rauhut <- function(words, v, parts) {
#    f <- rowsum(v, words)[words, ]
#    s <- proportions(rowsum(v, parts))[parts, ]
#    m <- cbind(s, abs(v / f - s))
#    m <- rowsum(m, words, reorder = FALSE)
#    m[, 1L] <- 1 - m[, 1L]
#    (rowSums(m) / 2) / (1 - min(s))
# }

# lal <- fread("../blog/test_data/bnc_word_id",
#   col.names = c("freq", "word", "part"),
#   key = c("word", "part"),
#   sep = "\t",
#   quote = "",
#   na.string = "",
#   # stringsAsFactors = TRUE,
#   fill = TRUE
# )

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

# # # old ones from Gries
# # formulae <- expression(
# #   part_range  = sum(v > 0),
# #   dp          = sum(abs((v / f) - s)) / 2,
# #   dp_norm     = dp(v, s, f) / (1 - min(s)),
# #   vc          = sd_pop(v) / mean(v),
# #   dc          = ((sum(sqrt(v)) / n)^2) / mean(v),
# #   idf         = log2(n / sum(v > 0)),
# #   sd_pop      = sd(v) * sqrt((length(v) - 1) / length(v)),
# #   engvall     = f * (sum(v > 0) / n),
# #   kromer_ur   = sum(digamma(v + 1) - digamma(1)),
# #   j_d_eq      = 1 - (vc(v) / sqrt(n - 1)),
# #   j_u_eq      = f * juilland_d_eq(v, n),
# #   j_d_uneq    = 1 - ((sd_pop(v) / mean(p)) / sqrt(n - 1)),
# #   j_u_uneq    = f * juilland_d_uneq(v, n, p),
# #   r_s_eq      = ((sum(sqrt(v))^2) / n) / f,
# #   r_s_uneq    = sum(sqrt(v * s))^2 / f,
# #   r_adj_eq    = (sum(sqrt(v))^2) / n,
# #   r_adj_uneq  = sum(sqrt(v * s))^2,
# #   chi_squared = sum(((v - (s * f))^2) / (s * f)),
# #   lyne_d3     = 1 - chi_squared(v, s, f) / (4 * f),
# #   carroll_d2  = {
# #     ans <- p / sum(p)
# #     ans <- ans[ans > 0]
# #     -sum(ans * log2(ans)) / log2(n)
# #   },
# #   carroll_um  = {
# #     d2 <- carroll_d2(p, n)
# #     (f * d2) + (1 - d2) * (f / n)
# #   },
# #   kld = {
# #     post_true <- v / f
# #     logs <- log2(post_true / s)
# #     logs[logs == -Inf] <- 0
# #     sum(post_true * logs) # could be normalized as 1-2^(-kld)
# #   }
# # )


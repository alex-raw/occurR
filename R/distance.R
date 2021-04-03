# library(data.table)

# dat <- fread("bnc_word_id_full",
#       quote = "",
#       sep = "\t",
#       stringsAsFactors = TRUE,
#       select = c(1, 2),
#       col.names =  c("token", "part")
# )

# dat[, part := NULL]
# test <- copy(dat)

# system.time({
# # test[, dist_emp := .I - shift(.I, fill = 0L), by = token]
# size <- nrow(test)
# test[, cpos := .I]
# setkey(test, token)

# test <- test[,
#   dist_emp := cpos - shift(cpos, fill = 0L), by = token
#   ][,
#   freq = .N,
#   # deviation of distances
#   dd := dist_emp - (size / .N), by = token
#   ][,
#   # mean absolute differences, and frequency
#   .(freq = .N,
#     mad = sum(abs(dd)) / .N), by = token
#   ][,
#   # worst mad
#   worst_mad := (((size - freq) + 1) - (size / freq)) / (.5 * freq)
#   ][,
#   dwg := mad / worst_mad,
#   ]
# })

# dat[token == "DING", , .N, by = token][]

# sum(abs(c(1, 2, -3, 100))) / 4

# old <- copy(test)
# hist(old[, dwg], breaks = 100)
# hist(test[, dwg], breaks = 100)

# old["DING"]
# old["DING"]
# old[dwg == 1]

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

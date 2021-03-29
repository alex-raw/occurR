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

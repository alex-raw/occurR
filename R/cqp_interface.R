query_cqp <- function(corpus, query, filename) {
  system(paste0(
    "echo '", corpus, "; ", query,
    '; cat Last > "', filename, "\";' | cqp -c"
  ))
}

# }}}
# # {{{ Examples ------------------------------------------------------------
# corpus <- "BROWN"
# cqp_query <- '
# "the"
# '
# filename <- "lol"

# query_cqp("BROWN", "the", "lol")

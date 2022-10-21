# data set on sentential adjective co-occurrence, cf. Justeson & Katz 1991

count_pairs_per_sentence <- function(words, s_id) {
  # find all combinations of 2 adjectives within a sentence
  combos <- tapply(words, s_id, function(x) {
    if (length(x) > 1)
      unique(combn(x, 2, sort, simplify = FALSE)) # ab = ba and count only once
  }) |>
    unlist(recursive = FALSE) # get a list of pairs

  uniques <- unique(combos)
  data.frame(
    matrix(unlist(uniques), ncol = 2, byrow = TRUE),
    as.vector(table(match(combos, uniques)))
  )
}

get_adjectives <- function(corpus, tag) {
  res <- subset(corpus, grepl(tag, pos), c(word, sentence))
  res$word <- as.factor(tolower(res$word))
  res
}

main <- function(corpus) {
  adjectives <- get_adjectives(corpus, "jj")

  counts <- table(adjectives$word)
  .pairs <- count_pairs_per_sentence(adjectives$word, adjectives$sentence)

  cbind(.pairs,
    as.vector(counts[.pairs[, 1]]),
    as.vector(counts[.pairs[, 2]])
  ) |>
    structure(
      names = c("word", "collocate", "o11", "f1", "f2"),
      corpus_size = nrow(corpus),
      unique_jj = length(counts)
    )
}

data(brown)

adjective_cooccurrence <- main(brown)
usethis::use_data(adjective_cooccurrence, overwrite = TRUE)

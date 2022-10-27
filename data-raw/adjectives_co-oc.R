# data set on sentential adjective co-occurrence, cf. Justeson & Katz 1991

# find and count all combinations of 2 adjectives within a sentence
count_pairs_per_sentence <- function(words, s_id) {
  combos <- fastmatch::ctapply(words, s_id, function(x) {
    if (length(x) > 1) {
      unique(combn(x, 2, sort.int, simplify = FALSE))
    }
  }) # get a list of pairs; sort so ab = ba and counted only once
  uniques <- unique(combos)

  data.frame(
    do.call(rbind, uniques),
    as.vector(table(fastmatch::fmatch(combos, uniques)))
  )
}

get_pos <- function(corpus, tag) {
  res <- subset(corpus, grepl(tag, pos), c(word, sentence))
  res$word <- tolower(res$word)
  res
}

get_cooccurrence <- function(corpus, tag) {
  adjectives <- get_pos(corpus, tag)
  counts <- table(adjectives$word)
  .pairs <- count_pairs_per_sentence(adjectives$word, adjectives$sentence)

  # join counts with word and collocate
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

adjective_cooccurrence <- get_cooccurrence(brown, tag = "^jj")
usethis::use_data(adjective_cooccurrence, overwrite = TRUE)

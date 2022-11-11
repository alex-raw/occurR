# data set on sentential adjective co-occurrence, cf. Justeson & Katz 1991

# find and count all combinations of 2 adjectives within a sentence
get_pairs <- function(matches) {
  if (length(matches) > 1) {
    unique(combn(matches, 2, sort.int, simplify = FALSE))
  } # get a list of pairs; sort |> unique so a b and b a only appear once
}

get_cooccurrence <- function(corpus, p_attr, s_attr, tag, tag_column = "pos") {
  tag_ids <- grep(tag, corpus[, tag_column])
  tokens <- corpus[tag_ids, p_attr] |> tolower()
  s_ids  <- corpus[tag_ids, s_attr]

  .pairs <- fastmatch::ctapply(tokens, s_ids, get_pairs)
  unique_pairs <- unique(.pairs)
  word      <- sapply(unique_pairs, "[", 1)
  collocate <- sapply(unique_pairs, "[", 2)

  pair_counts <- table(fastmatch::fmatch(.pairs, unique_pairs))
  word_counts <- table(tokens)

  structure(
    data.frame(
      word      = word,
      collocate = collocate,
      o11       = as.vector(pair_counts),
      f1        = as.vector(word_counts[word]),
      f2        = as.vector(word_counts[collocate])
    ),
    corpus_size = nrow(corpus),
    unique_jj = length(word_counts)
  )
}

data(brown)

adjective_cooccurrence <- get_cooccurrence(
  corpus = brown,
  p_attr = "word",
  s_attr = "doc_id",
  tag = "^jj"
)

usethis::use_data(adjective_cooccurrence, overwrite = TRUE)

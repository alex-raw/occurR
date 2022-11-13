# Reference implementation by Stephan Gries
# does rm(list = ls(all = TRUE)), so better encapsulate just to be safe
gries_env <- new.env()
source("http://www.stgries.info/research/dispersion/dispersions.r", gries_env)
gries_dispersion2 <- gries_env$dispersions2
gries_dispersion1 <- gries_env$dispersions1

.write_table <- \(...) utils::write.table(..., sep = "\t", na = "",
                                          quote = FALSE, row.names = FALSE)

get_gries_dispersion <- function(tokens, doc_id) {
   s <- table(doc_id) / length(doc_id)
   v <- table(tokens, doc_id)
   res <- apply(v, 1, gries_dispersion2, s) |>
    # distance; produces a matrix of named lists...
    sapply(unlist) |> t() |> data.frame()

  res <- cbind(rownames(res), res)
  colnames(res) <- c(
    "types", "f", "range", "maxmin", "sd_pop", "cv_pop", "chisq", "D_eq", "D",
    "S_eq", "S", "dc", "D2", "idf", "D3", "dp", "dp_norm", "kld", "U_eq",
    "U", "f_R_eq", "f_R", "Um", "engvall", "Ur"
  )

  res[order(res$types), ]
}

data(brown)
get_gries_dispersion(brown[, "word"], brown[, "doc_id"]) |>
  .write_table("../tests/testthat/test_data_dispersion.tsv")


# very ineffecient implementation, takes a while
get_gries_distances <- function(tokens, doc_id) {
  res <- sapply(
    unique(tokens),
    gries_dispersion1,
    corpus = tokens,
    corpus.parts = doc_id,
    with.distance.measures = TRUE
  ) |>
    apply(1, unlist)

  # first and last 6, last 6 are distance-based measures
  res <- data.frame(unique(tokens), res[, c(1, ncol(res) - 0:5)])
  colnames(res) <- c("types", "f", "washtell", "f_ald", "ald", "f_awt", "awt", "arf")
  res
}

get_gries_distances(brown$word, brown$doc_id) |>
  .write_table("../tests/testthat/test_data_distance.tsv")

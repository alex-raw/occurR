# Reference implementation by Stephan Gries
# does rm(list = ls(all = TRUE)), so better encapsulate just to be safe
gries_env <- new.env()
source("http://www.stgries.info/research/dispersion/dispersions.r", gries_env)
gries_dispersion2 <- gries_env$dispersions2
gries_dispersion1 <- gries_env$dispersions1

.write_table <- \(...) utils::write.table(..., sep = "\t", na = "",
                                          quote = FALSE, row.names = FALSE)

get_gries_dispersion <- function(corpus) {
  res <- with(corpus, {
    s <- table(id) / length(id)
    v <- table(word, id)
    apply(v, 1, gries_dispersion2, s)
  }) |> # distance; produces a matrix of named lists...
    sapply(unlist) |> t() |> data.frame()

  res <- cbind(rownames(res), res)
  colnames(res) <- c(
    "types", "f", "range", "maxmin", "sd_pop", "cv_pop", "chisq", "D_eq", "D",
    "S_eq", "S", "dc", "D2", "idf", "D3", "dp", "dp_norm", "kld", "U_eq",
    "U", "f_R_eq", "f_R", "Um", "engvall", "Ur"
  )

  res[order(res$types), ]
}

brown <- read.table("brown.tsv", quote = "", na.strings = "", header = TRUE,
           colClasses = c("factor", "NULL", "factor", "NULL"))
get_gries_dispersion(brown) |>
  .write_table("../tests/testthat/test_data_dispersion.tsv")


# very ineffecient implementation, takes a while
get_gries_distances <- function(corpus) {
  res <- with(corpus,
    table(word) |> sort() |> names() |> sapply(
      gries_dispersion1,
      corpus = word,
      corpus.parts = id,
      with.distance.measures = TRUE
    )) |>
    apply(1, unlist)

  # first and last 6, last 6 are distance-based measures
  res <- data.frame(rownames(res), res[, c(1, ncol(res) - 0:5)])
  colnames(res) <- c("types", "f", "washtell", "f.ald", "ald", "f.awt", "awt", "arf")
  res
}

get_gries_distances(brown) |>
  .write_table("../tests/testthat/test_data_distance.tsv")

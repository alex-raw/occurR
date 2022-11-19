disps <- setdiff(available_measures("disp"), "kld_norm")
dists <- setdiff(available_measures("dist"), c("dwg", "dwg_norm"))

data(brown)
x_table <- table(brown[, c("doc_id", "word")])

result_table <- dispersion(x_table, tokens = "word", parts = "doc_id", fun = disps)

.df <- as.data.frame(x_table, responseName = "v", stringsAsFactors = TRUE)
result_df <- dispersion(.df, tokens = word, parts = doc_id, freq = v, fun = disps)
result_raw <- dispersion(brown, tokens = word, parts = doc_id, fun = disps, type = "raw")

test_that("table and data.frame methods are consistent", {
  expect_equal(result_table, result_df, ignore_attr = TRUE)
  expect_equal(result_raw, result_df, ignore_attr = TRUE)
})

test_that("errors with incomplete corpus on distance measures", {
  expect_error(dispersion(result_df, tokens = word, parts = doc_id, freq = v, fun = dists))
})

ref <- .read_table("test_data_dispersion.tsv")[, colnames(result_df)]
ref <- ref[order(ref$types), ][1:100, ]
ref_dist <- .read_table("test_data_distance.tsv")
result_dist <- dispersion(brown, tokens = word, parts = doc_id, fun = dists, type = "raw")
x_disp <- result_df[order(result_df$types), ][1:100, ]

ref_cols_dist <- union(c("types", "f"), dists)
x_dist <- result_dist[order(result_dist$types), ][1:100, ]
ref_dist <- ref_dist[order(ref_dist$types), ][1:100, ref_cols_dist]

test_that("data sets have all required columns", {
  expect_identical(colnames(x_disp), c("types", "f", disps))
  expect_identical(colnames(x_dist), c("types", "f", dists))
})

test_that("values are consistent with Gries", {
  expect_equal(x_disp, ref, ignore_attr = TRUE)
  expect_equal(x_dist[, ref_cols_dist], ref_dist, ignore_attr = TRUE)
})

test_that("minimal dp == dispersion(..., fun = \"dp\")", {
  n <- 50
  v <- sample(1:100, n, replace = TRUE)
  tokens <- sample(letters, n, replace = TRUE)
  parts <- sample(LETTERS[1:3], n, replace = TRUE)

  big <- disp(tokens, parts, v, fun = "dp")
  big <- big[order(big$types), ]
  big <- setNames(big$dp, big$types)
  mini <- dp(tokens, parts, v, norm = FALSE)
  mini <- mini[order(names(mini))]
  expect_equal(mini, big)

  big <- disp(tokens, parts, v, fun = "dp_norm")
  big <- big[order(big$types), ]
  big <- setNames(big$dp, big$types)
  mini <- dp(tokens, parts, v, norm = TRUE)
  mini <- mini[order(names(mini))]
  expect_equal(mini, big)
})

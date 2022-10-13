# currently no 3rd party data for kld.norm
all_measures <- Filter(\(x) x != "kld.norm", available_measures("disp"))

read_test_data <- function(path, ...) {
  utils::read.table(path, sep = "\t", na = "", quote = "", ...)
}

create_mock <- function() {
  mock <- "bamnibeupbasatbewqnbcagabestabaghabeaatbahaabeaxat" |>
    strsplit("") |> unlist() |>
    table(tokens = _, parts = rep(1:5, c(9, 10, 10, 10, 11))) |>
    as.data.frame(responseName = "v")

  mock[mock$v != 0, ]
}

mock <- create_mock()

ans1 <- dispersion(v, tokens, parts, all_measures) |>
  with(mock, expr = _)

ans1_fact <- dispersion(v, as.factor(tokens), as.factor(parts), all_measures) |>
  with(mock, expr = _)

mock_reference <- read_test_data("mock_reference.tsv", header = TRUE)
ans1 <- ans1[, colnames(mock_reference)]
ans1_fact <- ans1_fact[, colnames(mock_reference)]

# existing brown values
brown <- read_test_data("brown_word_id",
  colClasses = c("numeric", "character", "character"),
  col.names  = c("v", "tokens", "parts")
)

ans2 <- dispersion(v, tokens, parts, all_measures) |>
  with(brown, expr = _)

ans2_fact <- dispersion(v, as.factor(tokens), as.factor(parts), all_measures) |>
  with(brown, expr = _)

ans2 <- ans2[order(ans2$types), ]
ans2_fact <- ans2_fact[order(ans2_fact$types), ]
rownames(ans2) <- NULL

get_brown_reference <- function() {
  res <- read_test_data("brown_reference.tsv", header = TRUE)
  res <- res[order(res$types), ]
  rownames(res) <- NULL
  res
}

brown_reference <- get_brown_reference()

get_collostructions_pkg <- function(.url, path) {
  dest <- file.path(path, basename(.url))
  utils::download.file(.url, dest)
  utils::untar(dest, exdir = path)
  path
}

get_reference_values <- function(.data) {
  more_measures <- c(
    chisq = "chisq",
    dice = "dice",
    gmean = "gmean",
    jaccard = "jaccard",
    liddell = "liddell",
    mi = "mi",
    mi3 = "mi3",
    min_sens = "ms",
    odds_ratio = "odds",
    poisson_sterling = "pois",
    t_score = "t",
    zscore = "z",
    zscore_cor = "z.cor",
    random = "random",
    cramer = "cramersV",
    fye = "fye",
    fye_ln = "fye.ln"
  )

  init <- collostructions::collex(.data, am = "logl", delta.p = TRUE,
                                  decimals = 99)

  lapply(more_measures, function(x) {
    collostructions::collex(.data, am = x, decimals = 999)[c(1, 6)]
  }) |> # list: data.frames
    Reduce(f = merge, init = init[c(1:4, 6, 8, 9)]) |>
    stats::setNames(c("type", "f1", "o11", "e11", "ll", "delta_p1",
                      "delta_p2", names(more_measures)))
}

main <- function() {
  get_collostructions_pkg(
    "https://sfla.ch/wp-content/uploads/2021/02/collostructions_0.2.0.tar.gz",
    tempdir()
  ) |> # char: path
    file.path("collostructions") |> # char: path
    devtools::load_all()

  data(goVerb)
  get_reference_values(goVerb) # nolint
}

res <- main() |> suppressMessages()
write.table(res, "../tests/testthat/test_data_collex.tsv",
            sep = "\t", na = "", quote = FALSE, row.names = FALSE)

# Gries data
# last accessed 2022-10-28 18:26
# download.file("https://www.stgries.info/teaching/groningen/coll.analysis.r")

cols <- list(
  type = "character",
  o11 = "numeric",
  other = NULL,
  relation = NULL,
  ll = "numeric",
  pearsonsid = "numeric",
  odds = "numeric",
  mi_base2 = "numeric",
  delta_p2 = "numeric",
  delta_p1 = "numeric",
  fye_mpfr = "numeric"
)

gries_output <- "https://www.stgries.info/teaching/groningen/1_out.csv" |>
  read.table(header = TRUE, col.names = names(cols), colClasses = cols)

ref_data_gries <- "https://www.stgries.info/teaching/groningen/1.csv" |>
  read.table(header = TRUE, col.names = c("type", "f1", "o11")) |>
  merge(gries_output)

write.table(ref_data_gries, "../tests/testthat/test_data_gries4.tsv",
            sep = "\t", na = "", quote = FALSE, row.names = FALSE)

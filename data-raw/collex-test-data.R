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
    fisher_pv = "fye",
    fisher_pv_ln = "fye.ln"
  )

  init <- collostructions::collex(goVerb, am = "logl", delta.p = TRUE,
                                  decimals = 99)

  lapply(more_measures, function(x) {
    collostructions::collex(.data, am = x, decimals = 999)[c(1, 6)]
  }) |>
    Reduce(f = merge, init = init[c(1:4, 6, 8, 9)]) |>
    stats::setNames(c("type", "f1", "o11", "e11", "ll", "delta_p1",
                      "delta_p2", names(more_measures)))
}

main <- function() {
  get_collostructions_pkg(
    "https://sfla.ch/wp-content/uploads/2021/02/collostructions_0.2.0.tar.gz",
    tempdir()
  ) |>
    file.path("collostructions") |>
    devtools::load_all()

  data(goVerb)
  get_reference_values(goVerb)
}

res <- main() |> suppressMessages()
write.table(res, "../tests/testthat/test_data_collex.tsv",
            sep = "\t", na = "", quote = FALSE, row.names = FALSE)

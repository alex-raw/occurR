#' Builtin measures
#'
#' Display names of built-in measures to be used in
#'
#' @param stat character, currently either "assoc" or "disp"
#'
#' @export
available_measures <- function(stat) {
  assoc <- names(builtin_assoc())
  disp  <- names(builtin_disp())
  # Values before range are intermediate results
  i <- seq(which(disp == "range"), length(disp))
  switch(stat, assoc = assoc, disp = disp[i])
}

count <- function(x) if (is.factor(x)) nlevels(x) else
  collapse::fNdistinct.default(x, na.rm = FALSE)

as_factor <- function(x, lex = NULL) {
  if (is.factor(x)) return(x)
  if (is.null(lex)) lex <- collapse::funique(x)
  factorcpp(x, lex)
}

make_one_sided <- function(assoc, o11, e11) {
  repulsed <- o11 < e11
  assoc[repulsed] <- -assoc[repulsed]
  assoc
}

zscore <- function(o11, e11) (o11 - e11) / sqrt(e11)

# maths helper functions, cf. UCS/R
reg_gamma_inv <- function(a, y, lower = TRUE, log = FALSE) {
  if (log == TRUE) y <- y * log(10)
  stats::qgamma(y, shape = a, scale = 1, lower.tail = lower, log = log)
}

reg_gamma <- function(a, x, lower = TRUE, log = FALSE) {
  ans <- stats::pgamma(x, shape = a, scale = 1, lower.tail = lower, log = log)
  if (log == TRUE) ans / log(10) else ans
}

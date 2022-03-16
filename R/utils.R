#' Builtin measures
#'
#' Display names of built-in measures
#'
#' @param stat character, currently either "assoc" or "disp"
#'
#' @export
available_measures <- function(stat = "") {
  assoc <- names(builtin_assoc())
  disp <- names(builtin_disp())
  # Values before range are intermediate results
  i <- seq(which(disp == "range"), length(disp))
  switch(stat,
    assoc = assoc,
    disp = disp[i],
    list(assoc = assoc, disp = disp[i])
  )
}

check_funs <- function(fun, exprs) {
  if (any(mismatch <- !fun %in% names(exprs))) {
    stop(
      "No built-in measure named: `",
      fun[mismatch],
      "`; see ?available_measures"
    )
  }
}

as_factor <- function(x, lex = NULL) {
  if (is.factor(x)) {
    return(x)
  }
  if (is.null(lex)) lex <- kit::funique(x)
  factorcpp(x, lex)
}

flip_negative_assoc <- function(x, o11, e11, funs, flip) {
  two_sided <- colnames(x) %in% flip
  repulsed <- o11 < e11
  x[repulsed, two_sided] <- -x[repulsed, two_sided]
  x
}

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
  # Values before range/ll are intermediate results
  i_assoc <- seq(which(assoc == "ll"), length(assoc))
  i_disp <- seq(which(disp == "range"), length(disp))
  switch(stat,
    assoc = assoc,
    disp = disp[i_disp],
    list(assoc = assoc[i_assoc], disp = disp[i_disp])
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
  if (is.factor(x)) return(x)
  if (is.null(lex)) lex <- kit::funique(x)
  factorcpp(x, lex)
}

# recursively get all variable names necessary to calculate intermediate and
# final values from list of expressions
gather_vars <- function(fun, exprs) {
  out <- union(all.vars(exprs[fun]), fun)
  if (identical(fun, out)) {
    return(out)
  } else {
    gather_vars(out, exprs)
  }
}

eval_exprs <- function(x, fun, exprs) {
  if (is.expression(fun)) fun <- names(fun)
  fun <- intersect(gather_vars(fun, exprs), names(exprs))
  exprs <- exprs[fun]
  for (i in names(exprs)) x[[i]] <- eval(exprs[[i]], x)
  x
}

flip_negative_assoc <- function(x, o11, e11, flip) {
  two_sided <- colnames(x) %in% flip
  repulsed <- o11 < e11
  x[repulsed, two_sided] <- -x[repulsed, two_sided]
  x
}

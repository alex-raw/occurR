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
  if (is.character(fun) && any(mismatch <- !fun %in% names(exprs))) {
    stop(
      "No built-in measure named: `",
      fun[mismatch],
      "`; see ?available_measures"
    )
  }
}

get_assoc_fun <- function(fun) {
  check_funs(fun, builtin_assoc())
  switch(class(fun),
    "character"  = builtin_assoc()[fun],
    "function"   = as.expression(body(fun)),
    "call"       = as.expression(fun),
    "expression" = fun,
    stop("invalid type of `fun`: ", class(fun))
  )
}

count <- function(x) {
  if (is.factor(x)) {
    return(nlevels(x))
  }
  collapse::fndistinct.default(x, na.rm = FALSE)
}

as_factor <- function(x, lex = NULL) {
  if (is.factor(x)) {
    return(x)
  }
  if (is.null(lex)) lex <- collapse::funique(x)
  factorcpp(x, lex)
}

make_one_sided <- function(assoc, o11, e11) {
  repulsed <- o11 < e11
  assoc[repulsed] <- -assoc[repulsed]
  assoc
}

# maths helper functions, cf. UCS/R
reg_gamma_inv <- function(a, y, lower = TRUE, log = FALSE) {
  if (log == TRUE) y <- y * log(10)
  stats::qgamma(y, shape = a, scale = 1, lower.tail = lower, log = log)
}

reg_gamma <- function(a, x, lower = TRUE, log = FALSE) {
  ans <- stats::pgamma(x, shape = a, scale = 1, lower.tail = lower, log = log)
  if (log == TRUE) ans / log(10) else ans
}

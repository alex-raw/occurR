get_obs <- function(f1, f2, o11, n) {
  cbind(o11,
        o12 = f2 - o11,
        o21 = f1 - o11,
        o22 = n - f1 - f2 + o11)
}

get_exp <- function(f1, f2, o11, n) {
  cbind(e11 = f1 * f2 / n,
        e12 = (n - f1) * f2 / n,
        e21 = (n - f2) * f1 / n,
        e22 = (n - f1) * (n - f2) / n)
}

get_vars <- function(arg, input) {
  # get vectors of values in a contingency table, cf. Evert (2004)
  with(input, switch(arg, n = n, f1 = f1, f2 = f2, o11 = o11,
    r1  = f1,
    c1  = f2,
    r2  = n - f1,
    c2  = n - f2,
    o12 = f2 - o11,
    o21 = f1 - o11,
    o22 = n - f1 - f2 + o11,
    e11 = f1 * f2 / n,
    e12 = (n - f1) * f2 / n,
    e21 = (n - f2) * f1 / n,
    e22 = (n - f1) * (n - f2) / n,
    o   = get_obs(f1, f2, o11, n),
    e   = get_exp(f1, f2, o11, n),
    alpha = 2,
    stop(paste0("No built-in way to calculate `", arg, "`."))
  ))
}

builtin_assoc <- function() {
  # cf. Evert (2004) & http://www.collocations.de
  expression(
    ll          = 2 * rowSums(o * log(o / e), na.rm = TRUE),
    mi          = log10(o11 / e11),
    mi_squared  = log10(o11^2 / e11),
    mi_conf     = log10(reg_gamma_inv(o11, -alpha - log10(2), log = TRUE) / e11),
    fisher_pv   = -log10(stats::phyper(o11 - 1, c1, c2, r1, lower.tail = FALSE)),
    poisson_pv  = -reg_gamma(o11, e11, log = TRUE),
    t_score     = (o11 - e11) / sqrt(o11),
    rel_risk    = log10((o11 * c2) / (o12 * c1)),
    lidell      = (n * (o11 - e11)) / (c1 * c2),
    gmean       = o11 / sqrt(n * e11),
    dice        = (2 * o11) / (r1 + c1),
    jaccard     = o11 / (o11 + o12 + o21),
    z_score     = (o11 - e11) / sqrt(e11),
    z_score_cor = z_score(ifelse(o11 > e11, o11 - 0.5, o11 + 0.5), e11),
    chisq       = (n * ((o11 - e11)^2)) / (e11 * e22),
    chisq_i     = rowSums(((o - e)^2) / e, na.rm = TRUE),
    chisq_h     = (n * (o11 * o22 - o12 * o21)^2) / (r1 * r2 * c1 * c2),
    chisq_corr  = (n * (abs(o11 * o22 - o12 * o21) - n / 2)^2) / (r1 * r2 * c1 * c2),
    min_sens    = ifelse((o11 / r1) < (o11 / c1), o11 / r1, o11 / c1),
    poisson_stirling = o11 * (log10(o11) - log10(e11) - 1),
    odds_ratio  = {
      o <- o + 0.5
      log10((o[, 1] * o[, 4]) / (o[, 2] * o[, 3]))
    }
  )
}

#' Association Measures
#'
#' Display names of built-in association measures to be used in
#' \code{v_assoc}
#'
#' @export
available_measures <- function() names(builtin_assoc())

#' Vectorized calculation of association measures
#'
#' Calculates association measures from vectors of frequencies and
#' joint frequencies.
#'
#' @param f1 numeric
#' @param o11 numeric joint frequencies
#' @param f2 numeric, if not provided, sum of o11 is used
#' @param n numeric, if not provided, sum of f1 is used
#' @param fun character vector for built-in measures (see Details).
#' alternatively a custom function, expression, or call. if names are supplied,
#' they are used in the output
#'
#' @return matrix
#' @details coming soon ...
#'
#' @export
v_assoc <- function(f1, o11, f2 = NULL, n = NULL, fun = "ll") {
  if (is.null(f2)) f2 <- sum(o11)
  if (is.null(n)) n <- sum(f1)
  stopifnot(is.numeric(f1), is.numeric(o11), is.numeric(n), is.numeric(f2))

  if (is.character(fun)) {
    mismatch <- fun[!fun %in% names(builtin_assoc())]
    if (length(mismatch) > 0L) {
      stop("No built-in association measure named: ",
           mismatch, "; see available_measures()")
    }
  }

  exprs <- switch(class(fun),
    "expression" = fun,
    "character"  = builtin_assoc()[fun],
    "function"   = as.expression(body(fun)),
    "call"       = as.expression(fun),
    stop("invalid type of `fun`: ", class(fun))
  )

  input <- list(f1 = f1, o11 = o11, f2 = f2, n = n)
  vars <- sapply(all.vars(exprs), get_vars, input, simplify = FALSE)
  out <- vapply(exprs, eval, numeric(length(f1)), vars)

  if (is.function(fun)) colnames(out) <- deparse(substitute(fun))
  if (!is.null(names(fun))) colnames(out) <- names(fun)
  out
}

coll <- function(x, o11 = NULL, n = NULL, f2 = NULL,
                 fun = "ll", decreasing = TRUE, one_sided = FALSE) {
  # generic function coming here for data.frames, data.tables and matrix input
}

#' Builtin measures
#'
#' Display names of built-in measures
#'
#' @param stat character, currently either "assoc" or "disp"
#'
#' @details
#' The following collocation and association measures are currently available:
#'
#' \describe{
#' \item{\code{ll}}{...}
#' \item{\code{mi}}{...}
#' \item{\code{mi2}}{...}
#' \item{\code{mi3}}{...}
#' \item{\code{fye}}{...}
#' \item{\code{poisson_pv}}{...}
#' \item{\code{rel_risk}}{...}
#' \item{\code{liddell}}{...}
#' \item{\code{gmean}}{...}
#' \item{\code{dice}}{...}
#' \item{\code{jaccard}}{...}
#' \item{\code{zscore}}{...}
#' \item{\code{zscore_cor}}{...}
#' \item{\code{chisq}}{...}
#' \item{\code{chisq_i}}{...}
#' \item{\code{chisq_h}}{...}
#' \item{\code{chisq_corr}}{...}
#' \item{\code{pearsonsid}}{...}
#' \item{\code{t_score}}{...}
#' \item{\code{cramer}}{...}
#' \item{\code{min_sens}}{...}
#' \item{\code{poisson_stirling}}{...}
#' \item{\code{odds_ratio}}{...}
#' \item{\code{delta_p1}}{...}
#' \item{\code{delta_p2}}{...}
#' }
#'
#' The following dispersion measures are currently available:
#'
#' \describe{
#' \item{\code{range}}{...}
#' \item{\code{maxmin}}{...}
#' \item{\code{engvall}}{...}
#' \item{\code{idf}}{...}
#' \item{\code{D}}{...}
#' \item{\code{U}}{...}
#' \item{\code{sd.pop}}{...}
#' \item{\code{cv.pop}}{...}
#' \item{\code{D.eq}}{...}
#' \item{\code{U.eq}}{...}
#' \item{\code{D2}}{...}
#' \item{\code{Um}}{...}
#' \item{\code{f.R}}{...}
#' \item{\code{S}}{...}
#' \item{\code{dc}}{...}
#' \item{\code{f.R.eq}}{...}
#' \item{\code{S.eq}}{...}
#' \item{\code{kld}}{...}
#' \item{\code{kld.norm}}{...}
#' \item{\code{dp}}{...}
#' \item{\code{dp.norm}}{...}
#' \item{\code{chisq}}{...}
#' \item{\code{D3}}{...}
#' \item{\code{Ur}}{...}
#' }
#'
#' The following distance-based dispersion measures are currently available:
#'
#' \describe{
#' \item{\code{arf}}{...}
#' \item{\code{awt}}{...}
#' \item{\code{f_awt}}{...}
#' \item{\code{ald}}{...}
#' \item{\code{f_ald}}{...}
#' \item{\code{washtell}}{...}
#' \item{\code{dwg}}{...}
#' \item{\code{dwg_norm}}{...}
#' }
#'
#' @export
available_measures <- function(stat = "") {
  assoc <- names(builtin_assoc())
  disp <- names(builtin_disp())

  # Values before range/ll are intermediate results
  i_assoc <- seq(which(assoc == "ll"), length(assoc))
  i_disp <- seq(which(disp == "range"), which(disp == "Ur"))
  i_dist <- seq(which(disp == "arf"), length(disp))

  res <- list(
    assoc = assoc[i_assoc],
    disp = disp[i_disp],
    dist = disp[i_dist]
  )

  if (identical(stat, "")) return(res)
  unlist(res[stat], use.names = FALSE)
}

check_funs <- function(fun, exprs) {
  mismatch <- !fun %in% names(exprs)
  if (any(mismatch)) {
    stop(
      "No built-in measure named: `",
      fun[mismatch],
      "`; see ?available_measures"
    )
  }
}

as_factor <- function(x) {
  if (is.factor(x)) return(x)
  if (is.numeric(x)) return(as.factor(x))
  kit::charToFact(x, nThread = parallel::detectCores())
}

sum_by <- function(f, n, g) {
  stopifnot(length(f) > 0, length(n) > 0, length(g) > 0)
  groupsum(g, n, f)
}

# recursively get all variable names necessary to calculate intermediate and
# final values from list of expressions; essentially traverse the AST
gather_vars <- function(exprs, .labels) {
  out <- exprs[.labels] |>
    all.vars() |>
    union(.labels)

  if (identical(.labels, out)) {
    return(exprs[out])
  }

  gather_vars(exprs, out)
}

get_occur <- function(fun, type, ...) {
  .builtins <- switch(type, assoc = builtin_assoc(), disp = builtin_disp())

  .labels <- switch(class(fun),
    `character` = fun,
    `function` = deparse1(substitute(fun)),
    names(fun) # list or expression
  )

  if (is.null(.labels) || any(!nzchar(.labels))) {
    stop("If `fun` is a list, all elements have to be named")
  }

  exprs <- lapply(fun, \(x) switch(class(x),
    `character` = {
      check_funs(x, .builtins)
      .builtins[[x]]
    },
    `function` = body(x),
    x
  )) |>
    c(.builtins) |>
    gather_vars(.labels)

  if (!is.list(fun) && !is.character(fun)) fun <- list(fun)

  evalapply(list(...), exprs)[.labels]
}

evalapply <- function(x, exprs) {
  for (i in names(exprs)) {
    x[[i]] <- eval(exprs[[i]], x)
  }

  x
}

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
  mismatch <- !fun %in% names(exprs)
  if (any(mismatch)) {
    stop(
      "No built-in measure named: `",
      fun[mismatch],
      "`; see ?available_measures"
    )
  }
}

as_factor <- function(x, lex = NULL) {
  if (is.factor(x)) return(x)
  if (is.numeric(x)) return(as.factor(x))
  kit::charToFact(x, nThread = parallel::detectCores())
}

# recursively get all variable names necessary to calculate intermediate and
# final values from list of expressions; essentially traverse the AST
gather_vars <- function(labels, exprs) {
  out <- exprs[labels] |>
    all.vars() |>
    union(labels)

  if (identical(labels, out)) {
    return(exprs[out])
  }

  gather_vars(out, exprs)
}

extract_vars <- function(fun, exprs) {
  if (is.character(fun)) check_funs(fun, exprs)

  if (is.expression(fun)) {
    exprs <- c(exprs, fun)
    fun <- names(fun)
  }

  out <- gather_vars(fun, exprs)
  attr(out, "labels") <- fun
  out
}

eval_exprs <- function(x, exprs) {
  for (i in names(exprs)) {
    x[[i]] <- eval(exprs[[i]], x)
  }

  x
}

flip_negative_assoc <- function(x, o11, e11, flip) {
  stopifnot(is.character(flip))
  two_sided <- colnames(x) %in% flip
  repulsed <- o11 < e11
  x[repulsed, two_sided] <- -x[repulsed, two_sided]
  x
}

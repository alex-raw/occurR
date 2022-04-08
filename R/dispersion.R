#' Calculate word dispersion measures
#'
#' @param v integer. counts
#' @param tokens character or factor. token
#' @param parts character or factor.
#' part id where a part is usually text or text region.
#' @param fun (named) character or list.
#' @param lexicon character. optional list of words to match. can be used
#' for clean up and speed up
#'
#' @export
dispersion <- function(v, tokens, parts, fun = "dp.norm", lexicon = NULL) {
  check_funs(fun, builtin_disp())
  stopifnot(
    is.numeric(v),
    is.character(tokens) || is.factor(tokens) || is.numeric(tokens),
    is.character(parts) || is.factor(parts) || is.numeric(parts),
    identical(length(v), length(tokens), length(parts))
  )

  # TODO: implement expression or function input like in assoc.R

  # TODO: check what happens if v = 0. might get nonsensical results

  v <- stats::na.fail(as.numeric(v))
  tokens <- as_factor(tokens, lexicon)
  x <- list(parts = parts, i = tokens, v = v)
  ans <- calculate_disp(x, fun = fun)[c("f", fun)]

  if (!is.null(names(fun))) names(ans) <- names(fun)
  data.frame(types = levels(tokens), ans)
}

gather_vars <- function(fun, exprs) {
  out <- union(all.vars(exprs[fun]), fun)
  if (identical(fun, out)) {
    return(out)
  } else {
    gather_vars(out, exprs)
  }
}

calculate_disp <- function(x, fun, exprs = builtin_disp()) {
  fun <- intersect(gather_vars(fun, exprs), names(exprs))
  exprs <- exprs[fun]
  for (i in names(exprs)) x[[i]] <- eval(exprs[[i]], x)
  x
}

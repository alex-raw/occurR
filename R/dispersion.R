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
  stopifnot(
    is.numeric(v),
    is.character(tokens) || is.factor(tokens) || is.numeric(tokens),
    is.character(parts) || is.factor(parts) || is.numeric(parts),
    identical(length(v), length(tokens), length(parts))
  )

  # TODO: check what happens if v = 0. might get nonsensical results

  exprs <- builtin_disp()
  if (is.character(fun)) check_funs(fun, exprs)
  if (is.expression(fun)) {
    exprs <- c(exprs, fun)
    fun <- names(fun)
  }

  if (anyNA(v)) stop("missing values in `v`")

  tokens <- as_factor(tokens, lexicon)
  x <- list(parts = parts, i = tokens, v = as.numeric(v))
  ans <- eval_exprs(x, fun, exprs)[c("f", fun)]

  if (!is.null(names(fun))) names(ans) <- names(fun)
  data.frame(types = levels(tokens), ans)
}

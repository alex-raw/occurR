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
    class(tokens) %in% c("character", "factor", "numeric"),
    class(parts) %in% c("character", "factor", "numeric"),
    identical(length(v), length(tokens), length(parts)),
    "missing values in `v`" = !anyNA(v)
  )

  # TODO: check what happens if v = 0. might get nonsensical results

  vars <- extract_vars(fun, builtin_disp())
  tokens <- as_factor(tokens, lexicon)
  ans <- list(parts = parts, i = tokens, v = as.numeric(v)) |>
    eval_exprs(vars)
  ans <- ans[c("f", fun)]

  if (!is.null(names(fun))) names(ans) <- names(fun)
  data.frame(types = levels(tokens), ans)
}

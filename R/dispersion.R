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
dispersion <- \(.x, ...) UseMethod("dispersion")

#' @export
dispersion.data.frame <- function(.x, ...) {
  # TODO:
  dispersion.default(v, tokens, parts, fun, lexicon)
}

#' @export
dispersion.data.table <- function(.x, ...) {
  # TODO:
  dispersion.default(v, tokens, parts, fun, lexicon)
}

#' @export
dispersion.table <- function(.x, ...) {
  # TODO:
  dispersion.default(v, tokens, parts, fun, lexicon)
}

#' @export
dispersion.default <- function(v, tokens, parts, fun = "dp.norm", lexicon = NULL) {
  stopifnot(
    is.numeric(v),
    class(tokens) %in% c("character", "factor", "numeric"),
    class(parts) %in% c("character", "factor", "numeric"),
    identical(length(v), length(tokens), length(parts)),
    "missing values in `v`" = !anyNA(v)
  )

  # TODO: check what happens if v = 0. might get nonsensical results
  # TODO: return length 0 data with length 0 input

  vars <- extract_vars(fun, builtin_disp())
  tokens <- as_factor(tokens, lexicon)
  ans <- list(parts = parts, i = tokens, v = as.numeric(v)) |>
    eval_exprs(vars)
  ans <- ans[c("f", fun)]

  if (!is.null(names(fun))) names(ans) <- names(fun)
  data.frame(types = levels(tokens), ans)
}

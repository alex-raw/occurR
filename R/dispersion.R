#' Calculate word dispersion measures
#'
#' @param .x data.frame or list containing data
#' @param v integer. counts
#' @param tokens character or factor. token
#' @param parts character or factor.
#' part id where a part is usually text or text region.
#' @param fun (named) character or list.
#' @param lexicon character. optional list of words to match. can be used
#' for clean up and speed up
#' @param ... further arguments to be passed to or from other methods
#'
#' @export
dispersion <- \(.x, ...) UseMethod("dispersion")

#' @rdname dispersion
#' @export
dispersion.data.frame <- function(.x, tokens, parts, v, fun, ...) {
  disp(
    tokens = eval(substitute(tokens), .x),
    parts = eval(substitute(parts), .x),
    v = eval(substitute(v), .x),
    fun = fun,
    ...
  )
}

#' @rdname dispersion
#' @export
dispersion.data.table <- function(.x, tokens, parts, v, fun, ...) {
  # TODO:
  # disp(tokens, parts, v, fun, lexicon)
}

#' @rdname dispersion
#' @export
dispersion.table <- function(.x, ...) {
  # TODO:
  # disp(tokens, parts, v, fun, lexicon)
}

#' @rdname dispersion
#' @export
dispersion.default <- function(.x, tokens, parts, v, fun, lexicon, ...) {
  disp(tokens = tokens, parts = parts, v = v, fun = fun, lexicon = lexicon)
}

disp <- function(tokens, parts, v, fun = "dp.norm", lexicon = NULL) {
  stopifnot(
    is.numeric(v),
    class(tokens) %in% c("character", "factor", "numeric"),
    class(parts) %in% c("character", "factor", "numeric"),
    identical(length(v), length(tokens), length(parts)),
    "missing values in `v`" = !anyNA(v)
  )

  # TODO: return length 0 data with length 0 input

  non_zero <- v != 0
  vars <- extract_vars(fun, builtin_disp())
  tokens <- as_factor(tokens, lexicon)
  ans <- data.frame(parts = parts, i = tokens, v = as.numeric(v))[non_zero, ] |>
    as.list() |>
    eval_exprs(vars)
  ans <- ans[c("f", fun)]

  if (!is.null(names(fun))) names(ans) <- names(fun)
  data.frame(types = levels(tokens), ans)
}

utils::globalVariables(c("tokens", "parts", "v", "lexicon"))

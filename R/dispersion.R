#' Calculate word dispersion measures
#'
#' @param .x data.frame or list containing data
#' @param v integer. counts
#' @param tokens character or factor; for table method, character specifying
#' dimname; for data.frame method, expression specifying the column
#' @param parts character or factor; for table method, character specifying
#' dimname; for data.frame method, expression specifying the column
#' part id where a part is usually text or text region.
#' @param fun (named) character or list.
#' @param ... further arguments to be passed to or from other methods
#'
#' @export
dispersion <- \(.x, ...) UseMethod("dispersion")

#' @rdname dispersion
#' @export
dispersion.data.frame <- function(.x, tokens, parts, v = NULL, fun = "dp_norm", ...) {
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
dispersion.table <- function(.x, tokens, parts, ...) {
  .df <- as.data.frame(.x, responseName = "v")
  disp(tokens = .df[, tokens], parts = .df[, parts], v = .df[, "v"], ...)
}

#' @rdname dispersion
#' @export
dispersion.default <- function(.x, tokens, parts, v, fun, ...) {
  disp(tokens = tokens, parts = parts, v = v, fun = fun)
}

disp <- function(tokens, parts, v = NULL, fun = "dp_norm") {
  stopifnot(
    is.numeric(v) || is.null(v),
    class(tokens) %in% c("character", "factor", "numeric"),
    class(parts) %in% c("character", "factor", "numeric"),
    "missing values in `v`" = !anyNA(v),
    is.list(fun) || is.character(fun) || is.expression(fun) || is.function(fun)
  )

  if (is.null(v)) {
    x <- as.data.frame(table(parts, tokens))
    parts <- x[, 1]
    tokens <- x[, 2]
    v <- x[, 3]
  }

  stopifnot(identical(length(v), length(tokens), length(parts)))

  if (!length(v)) {
    return(numeric(0))
  }

  non_zero <- v != 0
  get_occur(
    fun = c("types", "f", fun),
    type = "disp",
    parts = as_factor(parts[non_zero]),
    v = as.numeric(v[non_zero]),
    i = as_factor(tokens[non_zero])
  ) |>
    data.frame()
}

utils::globalVariables(c("tokens", "parts", "v"))

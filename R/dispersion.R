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
#' @param cutoff integer minimum frequency for each type
#' @param ... further arguments to be passed to or from other methods
#'
#' @export
dispersion <- \(.x, ...) UseMethod("dispersion")

#' @rdname dispersion
#' @export
dispersion.data.frame <- function(.x, tokens, parts, v = NULL, fun = "dp_norm", cutoff = 0L, ...) {
  disp(
    tokens = eval(substitute(tokens), .x),
    parts = eval(substitute(parts), .x),
    v = eval(substitute(v), .x),
    fun = fun,
    cutoff = cutoff,
    ...
  )
}

#' @rdname dispersion
#' @export
dispersion.table <- function(.x, tokens, parts, fun = "dp_norm", cutoff = 0L, ...) {
  .df <- as.data.frame(.x, responseName = "v")
  disp(tokens = .df[, tokens], parts = .df[, parts], v = .df[, "v"], fun = fun, cutoff = cutoff, ...)
}

#' @rdname dispersion
#' @export
dispersion.default <- function(.x = NULL, tokens, parts, v = NULL, fun, cutoff = 0L, ...) {
  disp(tokens = tokens, parts = parts, v = v, fun = fun, cutoff = cutoff)
}

disp <- function(tokens, parts, v = NULL, fun = "dp_norm", cutoff = 0L) {
  stopifnot(
    is.numeric(v) || is.null(v),
    class(tokens) %in% c("character", "factor", "numeric"),
    class(parts) %in% c("character", "factor", "numeric"),
    "missing values in `v`" = !anyNA(v),
    is.list(fun) || is.character(fun) || is.expression(fun) || is.function(fun)
  )

  pnearest <- d <- l <- d_sum_squ <- d_sum_log10 <- d_pmin <- d_sum_abs <- NULL

  with_dist <- any(fun %in% available_measures("dist"))
  if (with_dist) {
    if (!is.null(v)) stop("complete corpus required for distance-based measures")
    l <- length(tokens)
    cpos <- seq_along(tokens)
  }

  tokens <- as_factor(tokens)
  parts <- as_factor(parts)
  N <- nlevels(tokens)

  f <- if (is.null(v)) tabulate(tokens) else sum_by(tokens, N, v)

  if (cutoff > 0L) {
    f_keep <- f > cutoff
    f <- f[f_keep]
    keep <- !is.na(fastmatch::fmatch(as.integer(tokens), which(f_keep)))
    tokens <- fdroplevels(tokens[keep])
    parts <- fdroplevels(parts[keep])
  }

  if (is.null(v)) {
    tp <- as.integer(tokens) * nlevels(parts) + as.integer(parts)
    uxid <- !kit::fduplicated(tp)
    p <- parts[uxid]
    i <- tokens[uxid]
    v <- tabulate(fastmatch::fmatch(tp, tp[uxid]))
  } else {
    non_zero <- v > 0L
    p <- parts[non_zero]
    i <- tokens[non_zero]
    v <- v[non_zero]
  }

  if (with_dist) {
    if (cutoff > 0L) cpos <- cpos[keep]

    groups <- split.default(cpos, tokens)

    d_sum_squ <- d_sum_log10 <- d_pmin <- d_sum_abs <- numeric(length(groups))
    for (j in seq_along(groups)) {
      group <- groups[[j]]
      d <- c(0L, group + l - max(group)) # wrapping around
      d <- d[-1L] - d[-length(d)] # diff
      d_sum_squ[j] <- sum(d^2)
      d_sum_log10[j] <- sum(d * log10(d))
      d_pmin[j] <- sum(pmin.int(d, l / length(d)))
      d_sum_abs[j] <- sum(abs(d - l / length(d)))
    }

    if ("washtell" %in% fun) pnearest <- get_pnearest(cpos, tp)
  }

  if (!length(v) || identical(v, 0)) {
    return(numeric(0))
  }

  stopifnot(identical(length(v), length(i), length(p)))

  get_occur(fun = c("types", "f", fun), type = "disp",
    f = f, l = l, v = v, parts = p, pnearest = pnearest, i = i,
    d_sum_squ = d_sum_squ, d_sum_log10 = d_sum_log10, d_pmin = d_pmin,
    d_sum_abs = d_sum_abs
  ) |>
    data.frame()
}

get_pnearest <- function(cpos, tp) {
  groups <- split.default(cpos, as_factor(tp)) # split calls base::as.factor
  pnearest <- numeric(length(groups))
  for (j in which(lengths(groups) > 1L)) {
    x <- groups[[j]]
    x <- x[-1L] - x[-length(x)] # diff
    pnearest[j] <- sum(1 / c(x[1L], pmin.int(x, c(x[-1L], Inf))))
  }
  pnearest
}

utils::globalVariables(c("tokens", "parts", "v"))

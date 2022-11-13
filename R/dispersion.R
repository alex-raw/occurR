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

  pnearest <- d <- l <-  NULL
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
  f_keep <- f > cutoff
  keep <- !is.na(fastmatch::fmatch(as.integer(tokens), which(f_keep)))

  tokens <- tokens[keep, drop = TRUE]
  parts <- parts[keep, drop = TRUE]
  f <- f[f_keep]

  if (is.null(v)) {
    x <- as.data.frame(table(parts, tokens))
    non_zero <- x[, "Freq"] > 0L
    p <- x[non_zero, 1L]
    i <- x[non_zero, 2L]
    v <- x[non_zero, 3L]
  } else {
    non_zero <- v > 0L
    p <- parts[non_zero]
    i <- tokens[non_zero]
    v <- v[non_zero]
  }

  d_sum_squ <- d_sum_log10 <- d_pmin <- NULL
  if (with_dist) {
    cpos <- cpos[keep]
    groups <- split.default(cpos, tokens)

    d_sum_squ <- d_sum_log10 <- d_pmin <- numeric(length(groups))
    for (j in seq_along(groups)) {
      group <- groups[[j]]
      d <- c(0L, group + l - max(group))
      d <- d[-1L] - d[-length(d)] # diff
      d_sum_squ[j] <- sum(d^2)
      d_sum_log10[j] <- sum(d * log10(d))
      d_pmin[j] <- sum(pmin.int(d, l / length(d)))
    }

    if ("washtell" %in% fun) pnearest <- get_pnearest(tokens, parts, cpos)
  }

  stopifnot(identical(length(v), length(i), length(p)))

  if (!length(v)) {
    return(numeric(0))
  }

  get_occur(
    fun = c("types", "f", fun),
    type = "disp",
    l = l,
    d = d,
    pnearest = pnearest,
    v = as.numeric(v),
    parts = p,
    i = i,
    d_sum_squ = d_sum_squ,
    d_sum_log10 = d_sum_log10,
    d_pmin = d_pmin
  ) |>
    data.frame()
}

get_distances <- function(tokens, cpos, l) {
}

get_pnearest <- function(tokens, parts, cpos) {
  tp <- as.integer(tokens) * nlevels(parts) + as.integer(parts) # interaction
  groups <- split.default(cpos, tp)
  pnearest <- numeric(length(groups))
  for (j in which(lengths(groups) > 1)) {
    x <- groups[[j]]
    x <- x[-1L] - x[-length(x)] # diff
    pnearest[j] <- sum(1 / c(x[1L], pmin.int(x, c(x[-1L], Inf))))
  }
  pnearest
}

utils::globalVariables(c("tokens", "parts", "v"))

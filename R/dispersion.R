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
  disp(tokens = .df[, tokens], parts = .df[, parts], v = .df[, "v"], fun = fun,
       cutoff = cutoff, ...)
}

#' @rdname dispersion
#' @export
dispersion.default <- function(.x = NULL, tokens, parts, v = NULL, fun, cutoff = 0L, ...) {
  disp(tokens = tokens, parts = parts, v = v, fun = fun, cutoff = cutoff)
}

disp <- function(tokens, parts, v = NULL, fun = "dp_norm", cutoff = 0L) {
  stopifnot(
    is.numeric(v) || is.null(v),
    class(tokens) %in% c("character", "factor", "numeric", "integer"),
    class(parts) %in% c("character", "factor", "numeric", "integer"),
    "missing values in `v`" = !anyNA(v),
    is.list(fun) || is.character(fun) || is.expression(fun) || is.function(fun)
  )

  if (any(fun %in% available_measures("dist"))) {
    if (!is.null(v)) stop("complete corpus required for distance-based measures")
    cpos <- seq_along(tokens)
  }

  # TODO: no longer needed for factors?
  l <- length(tokens)
  types <- if (is.factor(tokens)) levels(tokens) else kit::funique(tokens)
  tokens <- fastmatch::fmatch(tokens, types)
  N <- length(types)

  parts_types <- if (is.factor(parts)) levels(parts) else kit::funique(parts)
  parts <- fastmatch::fmatch(parts, parts_types)
  n <- length(parts_types)

  f <- if (is.null(v)) tabulate(tokens) else sum_by(tokens, N, v)

  # if (cutoff > 0L) {
  #   f_keep <- f > cutoff
  #   f <- f[f_keep]
  #   keep <- !is.na(fastmatch::fmatch(as.integer(tokens), which(f_keep)))
  #   tokens <- fdroplevels(tokens[keep])
  #   parts <- parts[keep]
  #   N <- N - number_of_dropped_itmes
  # }

  if (is.null(v)) {
    tp <- tokens * n + parts # interaction
    itp <- !kit::fduplicated(tp)
    ip <- parts[itp]
    i <- tokens[itp]
    v <- tabulate(fastmatch::fmatch(tp, tp[itp]))
  } else {
    non_zero <- v > 0L
    ip <- parts[non_zero]
    i <- tokens[non_zero]
    v <- v[non_zero]
  }

  stopifnot(stats::var(c(length(v), length(i), length(ip))) == 0)

  # if (cutoff > 0L) cpos <- cpos[keep]
  d <- if (any(fun %in% setdiff(available_measures("dist"), "washtell")))
    group_distances(tokens, cpos, l)

  d_per_part <- if ("washtell" %in% fun)
    group_distances(tp, cpos, per_part = TRUE)

  if (!length(v) || identical(v, 0)) {
    return(numeric(0))
  }

  out <- get_occur(fun = fun, type = "disp",
    tokens = tokens, parts = parts, N = N, types = types,
    n = n, f = f, l = l, v = v, ip = ip, i = i, d = d,
    d_per_part = d_per_part
  ) |> data.frame()
  cbind(types, f, out)
}

# add one dummy token for each unique token;
# diff to dummy == diff to corpus end or NA
# first index of unique tokens == diff to first occurrence
group_distances <- function(g, cpos, l = NULL, per_part = FALSE) {
  is_unique <- !kit::fduplicated(g, fromLast = per_part)
  uniques <- g[is_unique]
  sort_id <- order(c(g, uniques))

  dummies <- if (per_part) {
    rep.int(Inf, length(uniques))
  } else {
    which(is_unique) + l
  }

  i <- c(cpos, dummies)[sort_id]
  d <- c(i[-1L], 0L) - i # diff
  # sort back to line up with types and remove dummies
  d[order(sort_id)[seq_along(g)]]
}

utils::globalVariables(c("tokens", "parts", "v"))

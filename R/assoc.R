#' Association measures for collocation and collostruction analyses
#'
#' Calculates common association measures used to perform collocation or
#' collostruction analysis for typical count data.
#'
#' @param .x data.frame or list containing data
#' @param o11 numeric: joint frequencies
#' @param f1 numeric: corpus frequencies of the word
#' @param f2 numeric of length 1 or equal to o11: corpus frequencies of
#' co-occurring structure; if omitted, sum of o11 is used
#' @param n numeric of length 1 or equal to o11: corpus or sample size; if
#' omitted, `sum(f1 + f2)` is used; this might be undesired in the case of
#' collostruction analysis, where corpus size should always be explicitely
#' passed
#' @param fun character vector or named list containing character, function or
#' expression elements: for built-in measures (see Details).
#' @param flip character: names of measures for which to flip the sign for cases
#' with negative association, intended for two-sided measures
#' @param ... further arguments to be passed to or from other methods
#'
#' @return an object similar to .x with one result per column for the
#' association measures specified in `fun`; row names in matrices and character
#' or factor columns in data.frames are preserved
#'
#' @details
#' For collocation analysis, f1 and f2 typically represent the corpus frequencies of
#' the word and the collocate, respectively, i.e. frequencies of co-occurrence
#' included. For collostruction analysis, f1 represents the corpus frequencies
#' of the word, and f2 the construction frequency. In a contingency table, they
#' represent marginal sums.
#' Both the construction frequency `f2` and the corpus size `n` can be provided
#' as vectors, which allows for efficient calculations over data from multiple
#' constructions/corpora.
#'
#' For data.frame input, the values for "o11", "f1", "f2", "n" can either be
#' provided explicitly as expression or character argument or implicitly by
#' column name. It is recommended to pass the columns explicitly.
#'
#' Matrix input currently requires column names "o11", "f1", "f2", "n"
#'
#' @examples
#'
#' data(adjective_cooccurrence)
#' .x <- subset(adjective_cooccurrence, word != collocate)
#' n <- attr(adjective_cooccurrence, "corpus_size")
#' res <- coll_analysis(.x, o11, f1, f2, n, fun = "ll")
#' res[order(res$ll, decreasing = TRUE), ] |> head()
#'
#' # if arguments match column names, they can be used explicitely
#' c("o11", "f1", "f2") %in% names(.x) # TRUE
#' coll_analysis(.x, n = n, fun = "ll") |>
#'   head()
#'
#' # control names of output columns by using a named list
#' coll_analysis(.x, o11, f1, f2, n, fun = list(logl = "ll")) |>
#'   head()
#'
#' # using custom function
#' mi_base2 <- \(o11, e11) log2(o11 / e11)
#' coll_analysis(.x, o11, f1, f2, n, fun = mi_base2) |>
#'   head()
#'
#' # mix built-in measures with custom functions
#' coll_analysis(.x, n = n, fun = list(builtin = "ll", custom = mi_base2)) |>
#'   head()
#'
#' @export
coll_analysis <- \(.x, ...) UseMethod("coll_analysis")

#' @rdname coll_analysis
#' @export
coll_analysis.data.frame <- function(.x, o11 = NULL, f1 = NULL, f2 = NULL,
                                     n = NULL, fun = "ll", flip = NULL, ...) {
  o11 <- if (missing(o11)) .x$o11 else eval(substitute(o11), .x)
  f1 <- if (missing(f1)) .x$f1 else eval(substitute(f1), .x)
  f2 <- if (missing(f2)) .x$f2 else eval(substitute(f2), .x)
  n <- if (missing(n)) .x$n else eval(substitute(n), .x)

  if (is.function(fun)) {
    fun_name <- deparse1(substitute(fun))
    fun <- list(fun)
    names(fun) <- fun_name
  }

  if (is.null(o11) || is.null(f1)) {
    stop("missing columns: at least o11 and f1 need to be present in the ",
         "data.frame or specified explicitely")
  }

  res <- assoc(o11 = o11, f1 = f1, f2 = f2, n = n, fun = fun, flip = flip)
  cbind(Filter(\(x) is.character(x) || is.factor(x), .x), res)
}


# TODO: version for true contingency tables? where o12 and o21 are given?
#' @rdname coll_analysis
#' @export
coll_analysis.matrix <- function(.x, f2 = NULL, n = NULL, fun = "ll",
                                 flip = NULL, ...) {
  stopifnot(is.numeric(.x))
  storage.mode(.x) <- "numeric"
  .names <- colnames(.x)

  stopifnot(all(c("o11", "f1") %in% .names))

  o11 <- .x[, "o11"]
  f1 <- .x[, "f1"]
  if (is.null(f2) && "f2" %in% .names) {
    f2 <- .x[, "f2"]
  }
  if (is.null(n) && "n" %in% .names) {
    n <- .x[, "n"]
  }

  res <- assoc(o11 = o11, f1 = f1, f2 = f2, n = n, fun = fun, flip = flip)
  rownames(res) <- rownames(.x)
  res
}

#' @rdname coll_analysis
#' @export
coll_analysis.default <- function(.x, o11, f1, f2 = NULL, n = NULL, fun = "ll",
                                  flip = NULL, ...) {
  stop("currently only implemented for data.frame and matix input")
}

assoc <- function(o11, f1, f2 = NULL, n = NULL, fun = "ll", flip = NULL, ...) {
  if (is.null(f2)) f2 <- sum(o11)
  if (is.null(n)) n <- sum(f1 + f2)

  stopifnot(
    is.character(flip) || is.null(flip),
    is.numeric(o11),
    is.numeric(f1),
    is.numeric(f2),
    is.numeric(n),
    length(n)  == 1 || identical(length(f1), length(n)),
    length(f2) == 1 || identical(length(f1), length(f2)),
    identical(length(f1), length(o11)),
    all(o11 <= f1),
    all(o11 <= f2),
    all(o11 <= n),
    is.list(fun) || is.character(fun) || is.expression(fun) || is.function(fun)
  )

  .labels <- switch(class(fun),
    `character` = fun,
    `function` = deparse1(substitute(fun)),
    names(fun) # list or expression
  )

  if (is.null(.labels) || any(!nzchar(.labels))) {
    stop("If `fun` is a list, all elements have to be named")
  }

  .builtins <- builtin_assoc()
  if (!is.list(fun) && !is.character(fun)) fun <- list(fun)
  exprs <- lapply(fun, \(x) switch(class(x),
    `character` = {
      check_funs(x, .builtins)
      .builtins[[x]]
    },
    `function` = body(x),
    x
  ))

  names(exprs) <- .labels
  exprs <- c(.builtins, unlist(exprs))
  vars <- gather_vars(.labels, exprs)

  if (!length(f1)) return(numeric(0))

  ans <- eval_exprs(list(
    f1 = as.numeric(f1),
    o11 = as.numeric(o11),
    f2 = as.numeric(f2),
    n = as.numeric(n)
  ), vars)

  ans <- do.call(cbind, ans[.labels])

  if (is.null(flip)) return(ans)

  repulsed <- o11 < f1 * f2 / n
  two_sided <- colnames(ans) %in% flip
  ans[repulsed, two_sided] <- -ans[repulsed, two_sided]
  ans
}

utils::globalVariables(c("f1", "f2", "o11", "n"))

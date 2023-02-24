#' Calculate word dispersion measures
#'
#' @param .x data.frame or list containing data, or `corpus` constructed with
#' `create_corpus`
#' @param tokens character, factor, or expression specifying the data.frame
#' column or table dimnames
#' @param parts character, factor, or expression specifying the data.frame
#' column or table dimnames; the part ids have to be contiguous
#' @param freq integer counts
#' @param fun character or named list.
#' @param type input type, see details
#' @param cutoff integer minimum frequency for each type
#' @param ... further arguments to be passed to or from other methods
#'
#' @export
dispersion <- \(.x, ...) UseMethod("dispersion")

#' @rdname dispersion
#' @export
dispersion.data.frame <- function(
  .x,
  tokens,
  parts,
  freq = NULL,
  fun = "dp_norm",
  type = c("per_part", "raw", "corpus"),
  cutoff = 0L,
  ...
) {
  dispersion.default(
    tokens = eval(substitute(tokens), .x),
    parts = eval(substitute(parts), .x),
    freq = eval(substitute(freq), .x),
    fun = fun,
    cutoff = cutoff,
    type = type,
    ...
  )
}

#' @rdname dispersion
#' @export
dispersion.table <- function(
  .x,
  tokens = NULL,
  parts = NULL,
  fun = "dp_norm",
  cutoff = 0L,
  ...
) {
  .df <- as.data.frame(.x, responseName = "v")
  dispersion.default(
    tokens = .df[, if (is.null(tokens)) 1L else tokens],
    parts = .df[, if (is.null(parts)) 2L else parts],
    freq = .df[, "v"],
    fun = fun,
    cutoff = cutoff,
    type = "per_part",
    ...
  )
}

#' @rdname dispersion
#' @export
dispersion.corpus <- function(
  .x,
  tokens = NULL,
  parts = NULL,
  freq = NULL,
  fun = "dp.norm",
  cutoff = 0L,
  ...
) {
  disp(.x, fun = fun, type = "corpus", cutoff = cutoff)
}

#' @rdname dispersion
#' @export
dispersion.default <- function(
  .x = NULL,
  tokens,
  parts,
  freq = NULL,
  fun = "dp.norm",
  type = c("per_part", "raw"),
  cutoff = 0L,
  ...
) {
  stopifnot(
    is.numeric(freq) || is.null(freq),
    class(tokens) %in% c("character", "factor", "numeric", "integer"),
    class(parts) %in% c("character", "factor", "numeric", "integer"),
    "missing values in `v`" = !anyNA(freq),
    is.list(fun) || is.character(fun) || is.expression(fun) || is.function(fun)
  )
  if (!is.null(freq) && !all(keep <- freq > 0L)) {
    tokens <- tokens[keep]
    parts <- parts[keep]
    freq <- freq[keep]
  }
  disp(
    .x = tokens,
    parts = parts,
    freq = freq,
    fun = fun,
    type = type,
    cutoff = cutoff
  )
}

disp <- function(
  .x,
  parts = NULL,
  freq = NULL,
  vocab = NULL,
  doc_ids = NULL,
  fun = "dp_norm",
  type = c("per_part", "raw", "corpus"),
  cutoff = 0L
) {
  type <- match.arg(type)

  with_distance <- any(available_measures("dist") %in% fun)
  if (type == "per_part" && with_distance) {
    stop("complete corpus required for distance-based measures")
  }

  corpus <- if (type == "corpus") .x else
    create_corpus(.x, parts, freq, vocab, doc_ids, type, cutoff, with_distance)

  if (!length(corpus$v) || identical(corpus$v, 0)) {
    return(numeric(0))
  }

  data.frame(types = corpus$vocab, f = corpus$f, get_occur(fun, "disp", corpus))
}

#' Prepare corpus data for dispersion calculations
#'
#' From raw corpus or part-frequency list, create a list object containing
#' descriptive stats and indexed tokens.
#'
#' @param tokens character vector with tokens
#' @param parts character vector
#' @param freq integer optional vector with counts
#' @param vocab character or factor optional vector with unique tokens
#' @param doc_ids character or factor optional vector with part ids
#' @param type input type, either "per_part" or "raw"
#' @param cutoff integer minimum frequency for each type
#' @param with_distance logical whether or not to calculate distances required
#' for distance measures
#' @param no_match character, "fail" (default): throws an error if tokens
#' contain NAs after creating an index. Typically, this happens when `vocab` is
#' given and doesn't contain all types in the corpus; "remove": NAs are removed,
#' "keep": treat NAs as separate type of token
#' error
#'
#' @returns list of type "corpus"
#'
#' @details
#' \describe{
#' \item{\code{iparts}}{integer index of parts}
#' \item{\code{l}}{number of tokens in the input}
#' \item{\code{f}}{frequency per unique tokens}
#' \item{\code{i}}{integer index of tokens per parts}
#' \item{\code{j}}{integer index of parts per tokens}
#' \item{\code{v}}{frequency of tokens per part}
#' \item{\code{vocab}}{unique tokens}
#' \item{\code{sort_ids}}{sorting permutation of tokens for use in distance
#'                        based measures}
#' \item{\code{sizes}}{sizes of parts}
#' }
#'
#' @export
create_corpus <- function(
  tokens,
  parts,
  freq = NULL,
  vocab = NULL,
  doc_ids = NULL,
  type = c("per_part", "raw"),
  cutoff = 0L,
  with_distance = TRUE,
  no_match = c("fail", "remove", "keep")
) {
  type <- match.arg(type)
  no_match <- match.arg(no_match)

  vocab <- unique_if_null(vocab, tokens)
  doc_ids <- unique_if_null(doc_ids, parts)
  itokens <- to_index(tokens, vocab)
  iparts  <- to_index(parts, doc_ids)
  sort_ids <- if (with_distance) order(itokens)
  l <- length(tokens)

  if (anyNA(tokens)) {
    if (no_match == "fail") {
      stop("`tokens` contain NAs, make sure `vocab` contains all possible unique
           tokens, and/or that text is imported with `na.string` conversion
           disabled, alternatively set `no_match` to a different value")
    }
    if (no_match == "remove") {
      nas <- is.na(itokens)
      itokens <- itokens[!nas]
      iparts <- iparts[!nas]
      if (with_distance) sort_ids <- sort_ids[!nas]
    }
    if (no_match == "keep") {
      nas <- is.na(itokens)
      itokens[nas] <- max(itokens) + 1L
    }
  }

  sizes <- switch(type,
    raw = tabulate(iparts, length(doc_ids)),
    per_part = sum_by(iparts, length(doc_ids), freq)
  )

  f <- switch(type,
    raw = if (!is.null(freq)) freq else tabulate(itokens, length(vocab)),
    per_part = sum_by(itokens, length(vocab), freq)
  )

  if (cutoff > 0L) {
    vkeep <- f > cutoff
    f <- f[vkeep]
    vocab <- vocab[vkeep]

    itokens <- match(itokens, which(vkeep))
    ckeep <- !is.na(itokens)
    itokens <- itokens[ckeep]
    iparts <- iparts[ckeep]
    sort_ids <- sort_ids[ckeep]
  }

  if (type == "per_part") {
    i <- itokens
    j <- iparts
    v <- freq
  } else {
    m <- quick_table(iparts, itokens)
    i <- m$j # this is reversed due to some sorting issue later on
    j <- m$i
    v <- m$x
  }

  structure(list(
    iparts = iparts,
    l = l,
    f = f,
    i = i,
    j = j,
    v = v,
    vocab = vocab,
    sort_ids = sort_ids,
    sizes = sizes
  ), class = "corpus")
}

to_index <- function(obj, .table) {
  switch(class(obj),
    character = fastmatch::fmatch(obj, .table),
    numeric = if (all(ans <- as.integer(obj) == obj)) ans
              else match(obj, .table),
    as.integer(obj)
  )
}

unique_if_null <- function(x, y) {
  if (!is.null(x)) return(x)
  if (is.factor(y)) levels(y) else .unique(y)
}

# coercion to CsparseMatrix is magically much faster than manually calculating
# interaction indices and tabulating them; maybe should figure out how it's
# done internally to remove this hack
quick_table <- function(i, j) {
  Matrix::mat2triplet(
    methods::as(
      methods::new("dgTMatrix",
        x = rep_len(1, length(i)),
        Dim = c(max(i), max(j)),
        i = as.integer(i) - 1L,
        j = as.integer(j) - 1L
      ), "CsparseMatrix"
    )
  )
}

utils::globalVariables(c("tokens", "parts", "v"))

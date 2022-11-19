#' Calculate word dispersion measures
#'
#' @param .x data.frame or list containing data
#' @param tokens character, factor, or expression specifying the data.frame
#' column or table dim
#' @param parts character, factor, or expression specifying the data.frame
#' column or table dim
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
  tokens,
  parts,
  fun = "dp_norm",
  cutoff = 0L,
  ...
) {
  .df <- as.data.frame(.x, responseName = "v")
  dispersion.default(
    tokens = .df[, tokens],
    parts = .df[, parts],
    freq = .df[, "v"],
    fun = fun,
    cutoff = cutoff, # TODO: NULL to switch off?
    type = "per_part",
    ...
  )
}

#' @rdname dispersion
#' @export
dispersion.default <- function(
  .x = NULL,
  tokens,
  parts,
  freq = NULL,
  fun = "dp.norm",
  type = c("per_part", "raw", "corpus"),
  cutoff = 0L,
  ...
) {
  if (!is.null(freq) && !all(keep <- freq > 0L)) {
    tokens <- tokens[keep]
    parts <- parts[keep]
    freq <- freq[keep]
  }
  disp(
    tokens = tokens,
    parts = parts,
    freq = freq,
    fun = fun,
    type = type,
    cutoff = cutoff
  )
}

disp <- function(
  tokens,
  parts,
  freq = NULL,
  vocab = NULL,
  doc_ids = NULL,
  fun = "dp_norm",
  type = c("per_part", "raw", "corpus"),
  cutoff = 0L
) {
  stopifnot(
    is.numeric(freq) || is.null(freq),
    class(tokens) %in% c("character", "factor", "numeric", "integer"),
    class(parts) %in% c("character", "factor", "numeric", "integer"),
    "missing values in `v`" = !anyNA(freq),
    is.list(fun) || is.character(fun) || is.expression(fun) || is.function(fun)
  )

  type <- match.arg(type)

  dist_measures <- available_measures("dist") %in% fun
  if (type == "per_part" && any(dist_measures)) {
    stop("complete corpus required for distance-based measures")
  }

  corpus <- create_corpus(tokens, parts, vocab, doc_ids)

  f <- switch(type,
    per_part = sum_by(corpus$tokens$itokens, corpus$ntypes, freq),
    raw = tabulate(corpus$tokens$itokens),
    corpus = corpus$lexicon$f,
  )

  corpus$lexicon$f <- f[f != 0L]
  if (cutoff > 0L) {
    keep <- f > cutoff
    corpus$lexicon <- corpus$lexicon[keep, ]
    keep_ids <- !is.na(fastmatch::fmatch(corpus$tokens$itokens, which(keep)))
    corpus$tokens <- corpus$tokens[keep_ids, ] # TODO: wtf that's exensive
  }

  corpus$per_part <- get_per_part(corpus, freq)

  # TODO: gather everything in a big ass matrix and call with grouping
  # vector rep(i, number_of_funs)

  d <- if (any(fun %in% setdiff(available_measures("dist"), "washtell")))
    group_distances(corpus$tokens$itokens, corpus$tokens$cpos, corpus$ntokens)

  d_per_part <- if ("washtell" %in% fun)
    group_distances(corpus$tokens$ijoint, corpus$tokens$cpos, per_part = TRUE)

  if (!length(corpus$per_part$v) || identical(corpus$per_part$v, 0)) {
    return(numeric(0))
  }

  out <- get_occur(
    fun = fun,
    type = "disp",
    corpus = corpus,
    d = d,
    d_per_part = d_per_part
  ) |> data.frame()
  cbind(types = corpus$lexicon$vocab, f, out)
}

to_index <- function(obj, .table = NULL) {
  if (is.null(.table)) {
    .table <- kit::funique(obj)
  }
  switch(class(obj),
    character = fastmatch::fmatch(obj, .table),
    numeric = if ((ans <- as.integer(obj)) == obj) ans
              else fastmatch::fmatch(obj, .table),
    as.integer(obj)
  )
}

create_corpus <- function(tokens, parts, vocab = NULL, doc_ids = NULL) {
  first <- which(!kit::fduplicated(tokens))
  sort_id <- if (is.character(tokens))
    fastmatch::coalesce(tokens) else order(tokens)

  if (is.null(doc_ids))
    doc_ids <- if (is.factor(parts)) levels(parts) else kit::funique(parts)
  if (is.null(vocab))
    vocab <- if (is.factor(tokens)) levels(tokens) else tokens[first]

  itokens <- to_index(tokens, vocab)
  iparts  <- to_index(parts, doc_ids)
  ijoint <- itokens * as.numeric(length(iparts)) + iparts # interaction

  list(
    tokens = data.frame( # TODO: test if matrix is any better
      cpos    = seq_along(tokens), # TODO: this is technically in the rownames
      sort_id = sort_id,
      itokens = itokens,
      iparts  = iparts,
      ijoint  = ijoint
    ),
    lexicon = data.frame(
      vocab = vocab,
      first = first
    ),
    ntokens = length(tokens),
    ndocs   = length(doc_ids),
    ntypes  = length(vocab),
    doc_ids = doc_ids
  )
}

get_per_part <- function(corpus, freq = NULL) {
  if (!is.null(freq)) {
    corpus$tokens$v <- freq
    return(corpus$tokens[, c("itokens", "iparts", "v")])
  }
  i <- corpus$tokens$ijoint
  itp <- !kit::fduplicated(i)
  v <- tabulate(fastmatch::fmatch(i, i[itp]))
  res <- corpus$tokens[itp, c("itokens", "iparts")]
  res$v <- v
  res
}

# add one dummy token for each unique token;
# diff to dummy == diff to corpus end or NA
# first index of unique tokens == diff to first occurrence
group_distances <- function(g, cpos, l = NULL, per_part = FALSE) {
  is_unique <- !kit::fduplicated(g)
  uniques <- g[is_unique]
  sort_id <- order(c(g, uniques))

  dummies <- if (per_part) {
    rep.int(Inf, length(uniques))
  } else {
    which(is_unique) + l
  }

  i <- c(cpos, dummies)[sort_id]
  d <- c(i[-1L], 0L) - i # diff
  # sort back to line up with vocab and remove dummies
  d[order(sort_id)[seq_along(g)]]
}

utils::globalVariables(c("tokens", "parts", "v"))

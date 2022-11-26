#' Calculate word dispersion measures
#'
#' @param .x data.frame or list containing data
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
  type <- match.arg(type)

  if (type == "per_part" && any(available_measures("dist") %in% fun)) {
    stop("complete corpus required for distance-based measures")
  }

  vocab <- unique_if_null(vocab, tokens)
  doc_ids <- unique_if_null(doc_ids, parts)
  itokens <- to_index(tokens, vocab)
  iparts  <- to_index(parts, doc_ids)
  corpus <- list()

  if (type == "raw") {
    corpus$itokens <- itokens
    corpus$l <- length(tokens)
  }

  f <- switch(type,
    raw = tabulate(itokens, length(vocab)),
    per_part = sum_by(itokens, length(vocab), freq),
    corpus = corpus$lexicon$f
  )

  corpus$f <- f[f != 0L]

  # if (cutoff > 0L) {
  #   keep <- f > cutoff
  #   corpus$lexicon <- corpus$lexicon[keep, ]
  #   keep_ids <- !is.na(match(itokens, which(keep)))
  #   corpus$tokens <- corpus$tokens[keep_ids, ] # TODO: wtf that's expensive
  # }

  if (type == "per_part") {
    corpus$i <- itokens
    corpus$j <- iparts
    corpus$v <- freq
  } else {
    m <- quick_table(iparts, itokens)
    corpus$i <- m$j # this is reversed due to some sorting issue later on
    corpus$j <- m$i
    corpus$v <- m$x
  }

  if (!length(corpus$v) || identical(corpus$v, 0)) {
    return(numeric(0))
  }

  data.frame(
    types = vocab,
    f = f,
    get_occur(
      fun = fun,
      type = "disp",
      corpus = corpus
    )
  )
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
  if (is.factor(y)) levels(y) else kit::funique(y)
}

# coercion to CsparseMatrix is magically much faster than manually calculating
# interaction indeces and tabulating them; maybe should figure out how it's
# done internally to remove this hack
quick_table <- function(i, j) {
  methods::new("dgTMatrix",
    x = rep_len(1, length(i)),
    Dim = c(max(i), max(j)),
    i = as.integer(i) - 1L,
    j = as.integer(j) - 1L
  ) |>
    methods::as("CsparseMatrix") |>
    Matrix::mat2triplet()
}

utils::globalVariables(c("tokens", "parts", "v"))

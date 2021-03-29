build_args <- function(funs, val_fun, input) {
  fun_args <- unlist(lapply(funs, formalArgs))
  sapply(unique(fun_args), val_fun, input, simplify = FALSE)
}

get_obs <- function(f1, f2, o11, n) {
# returns matrix with contingency table of observed frequencies
  cbind(o11,
        o12 = f2 - o11,
        o21 = f1 - o11,
        o22 = n - f1 - f2 + o11)
}

get_exp <- function(f1, f2, o11, n) {
# returns matrix with contingency table of expected frequencies
  cbind(e11 = f1 * f2 / n,
        e12 = (n - f1) * f2 / n,
        e21 = (n - f2) * f1 / n,
        e22 = (n - f1) * (n - f2) / n)
}

get_assoc_vars <- function(x, input) {
  with(input, switch(x, n = n, f1 = f1, f2 = f2, o11 = o11,
    o12 = f2 - o11,
    o21 = f1 - o11,
    o22 = n - f1 - f2 + o11,
    e11 = f1 * f2 / n,
    e12 = (n - f1) * f2 / n,
    e21 = (n - f2) * f1 / n,
    e22 = (n - f1) * (n - f2) / n,
    r1  = f1,
    c1  = f2,
    r2  = n - f1,
    c2  = n - f2,
    o   = get_obs(f1, f2, o11, n),
    e   = get_exp(f1, f2, o11, n),
    stop(paste0("No built-in way to calculate `", arg, "`."))
  ))
}

coll_vec <- function(f1, o11, n, f2, fun = "ll", one_sided = FALSE) {
# calculate normalization or association for frequency lists of co-occurrences
  if (!is.list(fun)) fun <- list(fun)

  input_vals <- build_args(fun, get_assoc_vars, list(
    f1 = f1, o11 = o11, n = n, f2 = f2
  ))

  sapply(fun, function(x) {
    do.call(x, input_vals[names(formals(x))])
  })
}

make_one_sided <- function(assoc, o11, e11) {
# negative values for negative association
  repulsed <- o11 < e11
  assoc[repulsed] <- -assoc[repulsed]
  assoc
}

coll <- function(x, o11 = NULL, n = NULL, f2 = NULL,
                 fun = "ll", decreasing = TRUE, one_sided = FALSE) {
# convenience wrapper around coll_vec
  # word list
  if (is.data.frame(x)) {
    word_id <- grepl("character", sapply(x, class))
    words <- x[, word_id]
    if (length(words) == 0) words <- rownames(x)
    x <- x[, !word_id]
  } else if (is.matrix(x)) {
    words <- rownames(x)
  } else {
    words <- names(x)
  }

  if (length(words) == 0) {
    warning("No character vector found, only numeric values used")
  }

  # values
  if (is.data.frame(x) || is.matrix(x)) {
    one <- all(x[, 1] >= x[, 2])
    two <- all(x[, 1] <= x[, 2])
    if (!(one || two)) {
      stop("Some joint frequencies are greater than corresponding frequency of f1.")
    } else if (one == TRUE) {
      f1 <- x[, 1]
      o11 <- x[, 2]
    } else if (one == FALSE) {
      f1 <- x[, 2]
      o11 <- x[, 1]
    }
  } else {
    stopifnot(is.vector(x) && is.vector(o11))
    f1 <- x
  }

  if (is.null(n))   n <- sum(f1)
  if (is.null(f2)) f2 <- sum(o11)

  assoc <- coll_vec(f1, o11, n, f2, fun = fun, one_sided = one_sided)
  e11 <- f1 * f2 / n
  # TODO: repair
  if (one_sided == TRUE)
    assoc <- make_one_sided(assoc, o11, e11)

  out <- data.frame(words, f1, o11, e11, assoc)

  # TODO: take name of list element instead of 5
  if (!is.null(decreasing))
    out <- out[order(out[, 5L], decreasing = decreasing), ]

  # setNames(out, c("word", "f1", "observed", "expected", substitute(fun)))
  out
}

# {{{ helpers: association measures ---------------------------------
# cf. Evert (2004) & http://www.collocations.de

ll <- function(o, e)
  2 * rowSums(o * log10(o / e), na.rm = TRUE)

fisher_pv <- function(o11, c1, c2, r1)
  -log10(phyper(o11 - 1, c1, c2, r1, lower.tail = FALSE))

z_score <- function(o11, e11)
  (o11 - e11) / sqrt(e11)

z_score_yates <- function(o11, e11)
  z_score(ifelse(o11 > e11, o11 - 0.5, o11 + 0.5), e11)

t_score <- function(o11, e11)
  (o11 - e11) / sqrt(o11)

poisson_stirling <- function(o11, e11)
  o11 * (log10(o11) - log10(e11) - 1)

chi_squared <- function(o11, e11, e22, n)
  (n * ((o11 - e11)^2)) / (e11 * e22)

chi_squared_i <- function(o, e)
  rowSums(((o - e)^2) / e, na.rm = TRUE)

chi_squared_h <- function(o11, o22, o12, o21, n, r1, r2, c1, c2)
  (n * (o11 * o22 - o12 * o21)^2) / (r1 * r2 * c1 * c2)

chi_squared_corr <- function(o11, o22, o12, o21, n, r1, r2, c1, c2)
  (n * (abs(o11 * o22 - o12 * o21) - n / 2)^2) / (r1 * r2 * c1 * c2)

mi <- function(o11, e11)
  log10(o11 / e11)

mi_squared <- function(o11, e11)
  log10(o11^2 / e11)

odds_ratio_plain <- function(o)
  log10((o[, 1] * o[, 4]) / (o[, 2] * o[, 3]))

odds_ratio <- function(o)
  odds_ratio_plain(o + 0.5)

rel_risk <- function(o11, o12, c1, c2)
  log10((o11 * c2) / (o12 * c1))

lidell <- function(n, o11, e11, c1, c2)
  (n * (o11 - e11)) / (c1 * c2)

min_sens <- function(o11, r1, c1) {
  one <- o11 / r1
  two <- o11 / c1
  ifelse(one < two, one, two)
}

gmean <- function(o11, n, e11)
  o11 / sqrt(n * e11)

dice <- function(o11, r1, c1)
  (2 * o11) / (r1 + c1)

jaccard <- function(o11, o12, o21, o22)
  o11 / (o11 + o12 + o21)

mi_conf <- function(o11, e11, alpha = 2)
  log10(reg_gamma_inv(o11, -alpha - log10(2), log = TRUE) / e11)

poisson_pv <- function(o11, e11)
  -reg_gamma(o11, e11, log = TRUE)

# maths helper functions, cf. UCS/R ------------------------------------
reg_gamma_inv <- function(a, y, lower = TRUE, log = FALSE) {
  if (log == TRUE) y <- y * log(10)
  qgamma(y, shape = a, scale = 1, lower.tail = lower, log = log)
}

reg_gamma <- function(a, x, lower = TRUE, log = FALSE) {
  ans <- pgamma(x, shape = a, scale = 1, lower.tail = lower, log = log)
  if (log == TRUE) ans / log(10) else ans
}

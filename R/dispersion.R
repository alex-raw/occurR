# Gries 2019: Analyzing dispersion
disp_vars <- function(char) expression(
    words = words, v = v, f = f, n = n, ans = NULL,
    s = proportions(sum_by(parts, v))[parts], # % part to corpus (vectorized)
    p = v / sum_by(parts, v)[parts],          # % word to part   (vectorized)
    v_rel = v / f[words],                     # % word per part (vectorized)
    f_sqrt = sum_by(words, sqrt(v)),          # summed square roots (vectorized)
    part_range = tabulate(words),                    # number of parts with word
    f_mean = f / n                            # % word in corpus
  )[char]

builtin_disp <- function(char) expression(
    # maxmin     = tapply(v, words, max) - ifelse(part_range < n, 0, tapply(v, words, min)), # TODO: slow!
    part_range = part_range,  # TODO: if I can merge this, I can merge both lists. juilland doesn't like it
    idf        = log2(n / part_range),
    engvall    = part_range * f_mean,
    dc         = (f_sqrt / n)^2 / f_mean,
    kromer_ur  = sum_by(words, digamma(v + 1) - digamma(1)),
    kld        = sum_by(words, v_rel * log2(v_rel / s)),
    kld_norm   = 1 - 2^-kld,
    r_f_eq     = f_sqrt^2 / n,
    r_f_uneq   = sum_by(words, sqrt(v * s))^2,
    r_s_eq     = r_f_eq / f,
    r_s_uneq   = r_f_uneq / f,
    sd_pop     = sd_pop(v, n, part_range, f_mean, words),
    vc         = sd_pop / f_mean,
    j_d_eq     = 1 - vc / sqrt(n - 1),
    j_d_uneq   = {
      p_rel <- sum_by(words, p) / n
      part_range = part_range
      1 - sd_pop(p, n, part_range, p_rel, words) / p_rel / sqrt(n - 1)
    },
    j_u_eq     = j_d_eq * f,
    j_u_uneq   = j_d_uneq * f,
    dp         = {
      x <- with_zero_s(s, abs(v_rel - s), words)
      (1 - x[, 1L] + x[, 2L]) / 2
    },
    dp_norm    = dp / (1 - min(s)),
    carroll_d2 = {
      x <- p / sum_by(words, p)[words]
      -sum_by(words, x * log2(x)) / log2(n)
    },
    carroll_um = (f * carroll_d2) + (1 - carroll_d2) * (f / n),
    chi_squared = {
      f_exp <- s * f[words]
      x <- with_zero_s(s, ((v - f_exp)^2) / f_exp, words)
      ((1 - x[, 1L]) * f) + x[, 2L]
    },
    lyne_d3 = 1 - chi_squared / (4 * f)
  )[char]

with_zero_s <- function(s, expr, group)
  rowsum(cbind(s, expr), group)

get_pre <- function(x)
  Find(function(y) !y %in% names(disp_vars()), x)

calculate_disp <- function(fun, input) {
  expr <- builtin_disp(fun)
  vars <- all.vars(expr)

  # if any term is not in the list, start over with it instead
  if (!is.null(pre <- get_pre(vars))) {
    pre <- calculate_disp(pre, input)
  }

  # skip further calculations on already calculated values
  vars <- vars[!vars %in% names(pre)]
  input_vals <- lapply(disp_vars(vars), eval, input)

  # calculate using input plus results of recursive calls
  out <- list(eval(expr, c(input_vals, pre)))
  names(out) <- fun
  c(out, pre)
}

dispersion <- function(v, words, parts, fun = "dp_norm") {
  stopifnot(is.numeric(v), identical(length(v), length(words), length(parts)))

  if (!is.factor(words)) words <- as.factor(words)
  if (!is.factor(parts)) parts <- as.factor(parts)

  input <- list(v = v, words = words, parts = parts)
  # if (anyNA(input, recursive = TRUE))
  #   stop("NA values in input.")

  if (is.character(fun)) {
    mismatch <- fun[!fun %in% names(builtin_disp())]
    if (length(mismatch) > 0L)
      stop("No built-in association measure named: ",
           mismatch, "; see available_measures(\"disp\")")
  }

  unique_words <- levels(words)
  input$f <- sum_by(words, v)    # frequency of word in corpus
  input$n <- nlevels(parts)      # number of parts word occurs in

  # order funs to avoid duplicated calculations
  order_ind <- match(fun, names(builtin_disp()))
  fun_op <- fun[order(order_ind, decreasing = TRUE)]

  out <- vector(length(fun_op), mode = "list")
  names(out) <- fun_op
  for (i in fun_op) if (is.null(out[[i]])) {
    ans <- calculate_disp(i, input)
    out[names(ans)] <- ans
  }

  out <- out[fun]
  if (!is.null(names(fun))) names(out) <- names(fun)

  data.frame(
    word = unique_words,
    freq = input$f,
    out,
    row.names = NULL
  )
}

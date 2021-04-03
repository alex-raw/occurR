sum_by <- function(group, x) rowsum(x, group)[, 1]

# Gries 2019: Analyzing dispersion
get_disp_vars <- function(x, input) {
  with(input, switch(x, words = words, v = v, f = f, n = n, ans = NULL,
    s = proportions(sum_by(parts, v))[parts], # % part to corpus (vectorized)
    p = v / sum_by(parts, v)[parts],          # % word to part   (vectorized)
    rel = f / n,                              # % word in corpus
    n_w = table(words),                       # number of parts with word
    v_rel = v / f[words],                     # % word per part (vectorized)
    f_sqrt = sum_by(words, sqrt(v)),          # sums of square roots per part
    stop(paste0("No built-in way to calculate `", x, "`."))
  ))
}

dispersion <- function(v, words, parts, fun = "dp_norm") {
  input <- list(v = v, words = words, parts = parts,
    f = sum_by(words, v),  # frequency of word in corpus
    n = nlevels(parts)    # number of parts word occurs in
  )

  pre <- c(
    dp_norm = "dp", vc = "sd_pop", kld_norm  = "kld", carroll_um = "carroll_d2",
    r_s_eq = "r_f_eq", r_s_uneq = "r_f_uneq", j_u_eq = "j_d_eq",
    j_u_uneq = "j_d_uneq"
  )

  fun_tmp <- fun
  for (id in names(pre)) {
    fun_tmp[fun == id] <- pre[id]
  }

  expr <- get_formulae()[fun_tmp]
  input_vals <- sapply(all.vars(expr), get_disp_vars, input, simplify = FALSE)
  disp_1 <- lapply(expr, eval, input_vals)

  expr <- get_formulae()[setdiff(fun, names(disp_1))]
  disp_2 <- lapply(expr, eval, c(input_vals, disp_1))

  disp <- simplify2array(c(disp_1, disp_2)[fun])
  data.frame(words = rownames(disp), freq = input[["f"]], disp, row.names = NULL)
}

get_formulae <- function() {
  expression(
    part_range = n_w,
    idf        = log2(n / n_w),
    engvall    = n_w * rel,
    dc         = (f_sqrt / n)^2 / rel,
    kromer_ur  = sum_by(words, digamma(v + 1) - digamma(1)),
    r_f_eq     = f_sqrt^2 / n,
    r_f_uneq   = sum_by(words, sqrt(v * s))^2,
    r_s_eq     = r_f_eq / f,
    r_s_uneq   = r_f_uneq / f,
    kld        = sum_by(words, v_rel * log2(v_rel / s)),
    kld_norm   = 1 - 2^-kld,
    sd_pop     = sd_pop(v, n, n_w, rel, words),
    vc         = sd_pop / rel,
    j_d_eq     = 1 - (vc(v, n, n_w, f, words) / sqrt(n - 1)),
    j_u_eq     = j_d_eq * f,
    j_d_uneq   = 1 - (vc(v, n, n_w, sum_by(words, p), words) / sqrt(n - 1)), # FIXME: broken
    j_u_uneq   = j_d_uneq * f, # FIXME: broken
    # maxmin     = tapply(v, words, max) - ifelse(n_w < n, 0, tapply(v, words, min)), # TODO: slow!
    dp         = {
      ans <- rowsum(cbind(s, abs(v_rel - s)), words)
      (1 - ans[, 1L] + ans[, 2L]) / 2
    },
    dp_norm    = dp / (1 - min(s)),
    carroll_d2 = {
      ans <- p / sum_by(words, p)[words]
      -sum_by(words, ans * log2(ans)) / log2(n)
    },
    carroll_um = (f * carroll_d2) + (1 - carroll_d2) * (f / n)
  )
}

sd_pop <- function(v, n, n_w, rel, words)
  sqrt((sum_by(words, (v - rel[words])^2) + ((n - n_w) * rel^2)) / n)

vc <- function(v, n, n_w, rel, words)
  sd_pop(v, n, n_w, rel, words) / rel

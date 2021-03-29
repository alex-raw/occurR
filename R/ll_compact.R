vlogl <- function(f1, o11, n = sum(f1), f2 = sum(o11)) {
  o <- cbind(o11,                 o12 = f2 - o11,
             o21 = f1 - o11,      o22 = n - f1 - f2 + o11)
  e <- cbind(e11 = f1 * f2,       e12 = (n - f1) * f2,
             e21 = f1 * (n - f2), e22 = (n - f1) * (n - f2)) / n
  assoc <- 2 * rowSums(o * log(o / e), na.rm = TRUE)
  ifelse(o[, 1] < e[, 1], -assoc, assoc)
}

logl_dt <- function(x, n = sum(x$f1), f2 = sum(x$o11),
                     one_sided = TRUE, sorted = TRUE) {
  x[, `:=`(o12 = f2 - o11,
           o21 = f1 - o11,          o22 = n - f1 - f2 + o11,
           e11 = f1 * f2 / n,       e12 = (n - f1) * f2 / n,
           e21 = (n - f2) * f1 / n, e22 = (n - f2) * (n - f1) / n)
  ][, assoc :=
        2 * Reduce("+", Map(function(o, e) fifelse(o == 0, 0, o * log(o / e)),
                            list(o11, o12, o21, o22), list(e11, e12, e21, e22)))
  ][, c("o12", "o21", "o22", "e12", "e21", "e22") := NULL]
  if (one_sided == TRUE) x[o11 < e11, assoc := -assoc]
  if (sorted == TRUE) setorder(x, -assoc)
}

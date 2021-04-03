#' Calculate G² (log-likelihood test) from vectors
#'
#' Two functions that calculate loglikelihood association measures
#' for vectors of counts. Mostly for practice/demonstration purposes
#'
#' @param o11 numeric or integer with joint frequency
#' @param f1 numeric or integer with frequencies of token
#' @param f2 numeric or integer with frequencies of co-occurring token or construction
#' if not provided, sum of joint frequencies (o11) is taken
#' @param n numeric or integer with overall frequencies if not provided,
#' sum of f1 is taken
#' @param x data.table with named columns o11 and f1, optionally f2 and n
#' @param one_sided logical. should repulsion be indicated by a negative sign?
#' @param sorted logical. should output be sorted by association strength?
#'
#' @return numeric
#' @examples
#' lol_test <- ll_mini(c(thes = 41, is = 32, corpus = 41, data = 12), c(12, 1, 0, 3))
#'
#' @export
ll_mini <- function(f1, o11, n = sum(f1), f2 = sum(o11)) {
  o <- cbind(o11,                 o12 = f2 - o11,
             o21 = f1 - o11,      o22 = n - f1 - f2 + o11)
  e <- cbind(e11 = f1 * f2,       e12 = (n - f1) * f2,
             e21 = f1 * (n - f2), e22 = (n - f1) * (n - f2)) / n
  assoc <- 2 * rowSums(o * log(o / e), na.rm = TRUE)
  ifelse(o[, 1] < e[, 1], -assoc, assoc)
}

#' @rdname ll_mini
#' @import data.table
#' @export
ll_dt <- function(x, n = sum(x$f1), f2 = sum(x$o11), fun = "ll",
                  one_sided = TRUE, sorted = TRUE) {

  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package \"data.table\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  datatable.aware = TRUE

  # TODO: matrix output doesn't work
  x[, (fun) := data.frame(v_assoc(f1, o11, f2, n, fun))]
  if (isTRUE(one_sided)) x[o11 < e11, `:=`(assoc, -assoc)]
  if (isTRUE(sorted)) setorder(x, -assoc)
}

# due to NSE notes in R CMD check
o11 <- f1 <- assoc <- e11 <- NULL

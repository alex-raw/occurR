#' Calculate word dispersion measures
#'
#' @param v integer. frequencies
#' @param tokens character or factor. token
#' @param parts character or factor.
#' part id where a part is usually text or text region.
#' @param fun (named) character or list.
#' @param lexicon character. optional list of words to match. can be used
#' for clean up and speed up
#'
#' @export
dispersion <- function(v, tokens, parts, fun = "dp.norm", lexicon = NULL) {
  stopifnot(identical(length(v), length(tokens), length(parts)))
  if (anyNA(v)) stop("NA values need to be removed or set to 0.")

  tokens <- as_factor(tokens, lexicon)
  input <- list(v = v, i = tokens, parts = parts, N = nlevels(tokens))

  exprs <- builtin_disp()
  # TODO: implement expression or function input as in assoc.R

  if (is.character(fun) &
      length(mismatch <- fun[!fun %in% names(exprs)]) > 0L)
    stop("No built-in measure named: ", mismatch,
         "; see available_measures(\"disp\")")

  ans <- calculate_disp(input, fun, exprs)[c("f", fun)]

  if (!is.null(names(fun))) names(ans) <- names(fun)
  data.frame(types = levels(tokens), ans,
             check.names = FALSE, fix.empty.names = FALSE, row.names = NULL)
}

# meta-helpers
recurse_vars <- function(fun, exprs)
  if (identical(fun, out <- union(all.vars(exprs[fun]), fun)))
    return(out) else recurse_vars(out, exprs)

calculate_disp <- function(start_vals, fun, exprs = builtin_disp()) {
  fun <- intersect(recurse_vars(fun, exprs), names(exprs))
  exprs <- exprs[fun]
  start_vals[fun] <- 0L
  for (i in names(exprs))
    start_vals[[i]] <- eval(exprs[[i]], start_vals)
  start_vals
}

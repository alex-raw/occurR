distance <- function(cpos, f, size, corr = TRUE) {
  # TODO: only word growth dispersion for now
  deltas <- cpos - data.table::shift(cpos, fill = 0L)
  dd <- deltas - size / f
  mad <- sum(abs(dd)) / f
  worst_mad <- (size - f + 1 - size / f) / (f / 2)
  dwg <- mad / worst_mad
  if (isFALSE(corr)) return(dwg)
  dwg / (2 * atan(worst_mad) / atan(mad))
}

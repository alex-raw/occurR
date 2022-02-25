dwg <- function(cpos, f, size) { # word growth dispersion
    dd <- cpos - data.table::shift(cpos, fill = 0L) - (size / f)
    mad <- sum(abs(dd)) / f
    worst_mad <- (size - f + 1 - size / f) / (f / 2)
    dwg <- mad / worst_mad
    dwg / (2 * atan(worst_mad) / atan(mad))
}

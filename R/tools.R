#' Prepare input for collocation or collostruction analysis
#'
#' This is a convenience function to create valid input for `coll_analysis`
#' from raw frequency lists
#'
#' @param joint data.frame joint frequencies of collocate 1 or word in
#' construction
#' @param frequencies data.frame corpus/sample frequencies of collocate 1 or
#' word
#' @param type character either of "collocation", "simple", "distinctive",
#' "co-varying", see details
#'
#' @return data.frame valid for coll_analysis
#'
#' @details TODO
#'
#' @examples # TODO
#'
#' @export
prepare_coll_analysis <- function(
  joint,
  frequencies,
  type = c("collocation", "simple", "distinctive", "co-varying")
) {
  check_frequency_list(joint)
  check_frequency_list(frequencies)

  type <- match.arg(type) # TODO: use this

  key_joint <- Position(\(x) is.character(x) | is.factor(x), joint)
  key_freqs <- Position(\(x) is.character(x) | is.factor(x), frequencies)

  out <- merge(
    joint,
    frequencies,
    by.x = key_joint,
    by.y = key_freqs,
    all = TRUE
  )

  names(out) <- c("type", "o11", "f1")
  out
}

#' Check frequency lists for common problems
#'
#' Check frequency lists for duplicates caused by whitespace or case
#' sensitivity. Also warn if any fields are empty. This is a convenience
#' function. Do not rely on this feature during your data cleaup.
#' More checks might be added in future versions.
#'
#' @param .x frequency list with a character/factor column with types and a
#' numeric column with counts
#' @param case_sensitive logical whether or not the frequency list is case
#' sensitive
#'
#' @return NULL; fails if expected columns are ambiguous or missing, warns if
#' duplicate types or empty types are found
#'
#' @examples # TODO
#'
#' @export
check_frequency_list <- function(.x, case_sensitive = FALSE) {
  stopifnot(is.data.frame(.x))

  id_word <- vapply(.x, \(x) is.character(x) | is.factor(x), TRUE)
  id_count <- vapply(.x, is.numeric, TRUE)

  if (sum(id_word) == 0)
    stop("missing character or factor column to be identified as type column")

  if (sum(id_count) == 0)
    stop("missing numeric column to be identified as frequency column")

  if (sum(id_word) > 1)
    stop("ambiguous columns: multiple factor or character columns")

  if (sum(id_count) > 1)
    stop("ambiguous columns: multiple numeric columns")

  word_original <- .x[, id_word]
  word <- trimws(word_original)

  if (sum(is_empty <- !nzchar(word)) > 1)
    warning("found empty types in rows: ", paste(which(is_empty), collapse = ","))

  if (!case_sensitive) word <- tolower(word)

  if (sum(is_duplicated <- duplicated(word)) > 1) {
    warning("found potential duplicates:\n", paste0(
      which(is_duplicated),
      ": ",
      word_original[is_duplicated],
      "\n"
    ))
  }

  return()
}

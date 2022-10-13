#' Brown corpus
#'
#' The Brown corpus in tabular format tokenized and pos-tagged as distributed on
#' \url{https://www.nltk.org/nltk_data/}. Headings and sentence boundaries are
#' currently not preserved.
#'
#' For documentation, see \url{http://korpus.uib.no/icame/brown/bcm.html}.
#' The the raw README and CONTENTS files are also included as attributes.
#'
#' @format A data frame with four factor variables: `text_id`, `genre`,
#' `token`, `pos`; and two string attributes: `contents` and `readme`
#'
#' @examples
#'
#' head(brown)
#'
#' class(attr(brown, "README"))
"brown"

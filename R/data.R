#' Brown corpus
#'
#' The Brown corpus in tabular format tokenized and pos-tagged as distributed on
#' \url{https://www.nltk.org/nltk_data/}. Headings and sentence boundaries are
#' currently not preserved.
#'
#' For documentation, see \url{http://korpus.uib.no/icame/brown/bcm.html}.
#' The the raw README and CONTENTS files are also included as attributes.
#'
#' @format A data frame with five variables: `genre_id`, `doc_id`,
#' `sentence_id`, `word`, `pos`; and two string attributes: `contents` and
#' `readme`
#'
#' @examples
#'
#' data(brown)
#' head(brown)
#'
#' class(attr(brown, "README"))
"brown"


#' Adjective co-occurrence
#'
#' This data set includes counts for sentential co-occurrence of adjectives in
#' the Brown corpus, based on Justeson & Katz (1990). The data is extracted
#' from the same data set available in `?brown`. The tags used to extract
#' adjectives match "^jj.*".
#' @seealso [brown]
#'
#' @references Justeson, John S., and Slava M. Katz. "Co-occurrences of antonymous adjectives and their contexts." Computational linguistics 17.1 (1991): 1-20.
#'
#' @format A data frame with five variables, and two attributes:
#' \describe{
#' \item{\code{word}}{first adjective}
#' \item{\code{collocate}}{second adjective}
#' \item{\code{o11}}{frequency of co-occurrence of `word` and `collocate`}
#' \item{\code{f1}}{frequency of `word`}
#' \item{\code{f2}}{frequency of `collocate`}
#' \item{\code{attr(*, "corpus_size")}}{number of tokens in the whole corpus}
#' \item{\code{attr(*, "unique_jj")}}{number of tokens tagged as adjectives
#' (jj.*) in the whole corpus}
#' }
#'
#' @examples
#'
#' data(adjective_cooccurrence)
#' str(adjective_cooccurrence)
#'
#' attributes(adjective_cooccurrence)$unique_jj
"adjective_cooccurrence"

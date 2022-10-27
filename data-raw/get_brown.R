# ------- Archive structure as of 2022-10-12:
# ca01, ca02, ca03, ..., cats.txt, README, CONTENTS
# brown/cats.txt contains ids equivalent to their file name next to their genre
# (space delimited)
# the files `CONTENTS` and `README` are currently attached as attributes
# text files contain tokens (" "), sentences ("\n\n"),
# everything but headings is indented with one leading "\t"
# tokens consist of word/pos-tags
# literal "/" are not escaped
# "/" does not occur in tag, therefore last "/" in pair can be replaced with " "
# -------

# wrapper to partially apply arguments that ensure all characters are read as is
.text_as_is <- function(fun, ...) {
  function(...) {
    fun(...,
      na.strings = "",
      quote = "",
      comment.char = "",
      allowEscapes = FALSE,
      strip.white = TRUE,
      blank.lines.skip = TRUE
    )
  }
}

.scan <- \(...) .text_as_is(scan)(..., sep = "\n", what = "", quiet = TRUE)
.read_table <- \(...) .text_as_is(utils::read.table)(..., stringsAsFactors = TRUE)


# process text files based on index and add genre and file ids as columns
process_texts <- function(index_path) {
  index <- .read_table(index_path)
  files <- file.path(dirname(index_path), index[, 1])

  .lines <- lapply(files, .scan)          # split on \n to get sentences
  nlines <- lengths(.lines)

  tokens <- strsplit(unlist(.lines), " ") # split on white to get tokens
  ntokens <- lengths(tokens)

  tokens <- unlist(tokens) |>
    gsub("/([^/]*)$", " \\1", x = _) |>   # replace last "/" with white
    .read_table(text = _)                 # split again on white, get word/pos

  text_ids <- rep(index[, 1], nlines) |> rep(ntokens)
  genre_ids <- rep(index[, 2], nlines) |> rep(ntokens)
  sentence_ids <- seq_len(sum(nlines)) |> rep(ntokens)

  cbind(genre_ids, text_ids, sentence_ids, tokens)
}

get_brown <- function(.url, archive_path) {
  utils::download.file(.url, archive_path, quiet = TRUE)
  utils::unzip(archive_path, exdir = dirname(archive_path), junkpaths = TRUE)
}

main <- function() {
  temp_path <- tempdir()
  in_temp <- \(path) file.path(temp_path, path)

  # data as linked on https://www.nltk.org/nltk_data/
  .url <- "https://raw.githubusercontent.com/nltk/nltk_data/gh-pages/packages/corpora/brown.zip"  # nolint
  get_brown(.url, archive_path = in_temp(basename(.url)))

  structure(
    process_texts(index_path = in_temp("cats.txt")),
    names = c("genre_id", "doc_id", "sentence_id", "word", "pos"),
    contents = readLines(in_temp("CONTENTS")),
    readme = readLines(in_temp("README"))
  )
}

brown <- main()

write.table(brown, "brown.tsv",
  sep = "\t", na = "", quote = FALSE, row.names = FALSE)

usethis::use_data(brown, overwrite = TRUE)

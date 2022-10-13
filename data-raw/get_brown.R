# ------- Archive structure as of 2022-10-12:
# ca01, ca02, ca03, ..., cats.txt, README, CONTENTS
# brown/cats.txt contains ids equivalent to their file name next to their genre
# (space delimited)
# the files `CONTENTS` and `README` are currently attached as attributes
# text files contain tokens (" "), sentences ("\n\n"),
# everything but headings is indented with one leading "\t"
# tokens consist of word/pos-tags
# literal "/" are not escaped
# "/" does not occur in tag, therefore last "/" in pair can be replaced with "\t"
# -------

# wrapper to partially apply arguments that ensure all characters are read as is
.text_as_is <- function(fun, ...) {
  function(...) {
    fun(...,
      na.strings = "",
      quote = "\"",
      comment.char = "",
      allowEscapes = FALSE,
      strip.white = TRUE,
      blank.lines.skip = TRUE
    )
  }
}

.gsub_last_slash <- \(x) gsub("/([^/]*)$", " \\1", x)
.scan <- \(...) .text_as_is(scan)(..., what = "", quiet = TRUE)
.read_txt <- \(...) .text_as_is(utils::read.table)(..., stringsAsFactors = TRUE)
.read_char <- \(.f) readChar(.f, file.info(.f)$size)


parse_brown_file <- function(file, id, genre) {  # nolint
  .scan(as.character(file)) |>  # split on whitespace, get vector
    .gsub_last_slash() |>       # replace delimiter
    .read_txt(text = _) |>      # split again, get data.frame
    cbind(id, genre, cols = _)  # recycling here
}


# process text files based on index and add genre and file ids as columns
process_texts <- function(index_path, .fun) {
  index <- .read_txt(index_path)
  files <- file.path(dirname(index_path), index[, 1])
  Map(.fun, files, index[, 1], index[, 2], USE.NAMES = FALSE) |>
    do.call(rbind, args = _)
}


# download the Brown corpus and extract
get_brown <- function(url, archive_path) {
  utils::download.file(url, archive_path, quiet = TRUE)
  utils::unzip(archive_path, exdir = dirname(archive_path), junkpaths = TRUE)
}


main <- function() {
  temp_path <- tempdir()
  at_temp <- \(path) file.path(temp_path, path)

  # data as linked on https://www.nltk.org/nltk_data/
  url <- "https://raw.githubusercontent.com/nltk/nltk_data/gh-pages/packages/corpora/brown.zip"  # nolint
  get_brown(url, archive_path = at_temp(basename(url)))

  structure(
    process_texts(
      index_path = at_temp("cats.txt"),
      .fun = parse_brown_file
    ),
    names = c("id", "genre", "word", "pos"),
    contents = .read_char(at_temp("CONTENTS")),
    readme = .read_char(at_temp("README"))
  )
}

brown <- main()

usethis::use_data(brown, overwrite = TRUE)

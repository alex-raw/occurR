get_data_grid <- function(parameters) {
  # create parameter combinations from list
  data.table::data.table(
    expand.grid(parameters, stringsAsFactors = FALSE)
    )[
    corpus %in% c("DTA", "DTA2017", "BASE") &
    s_attr == "text_id", s_attr := "file_id"
    ][
    corpus %in% c("BNC", "BNC-BABY") &
    p_attr == "lemma", p_attr := "hw"
  ][]
}

cwb_scan <- Vectorize(
  function(dir_name, corpus, p_attr, s_attr, constraint = NULL) {
    # TODO: test constraint
    # call cwb-scan-corpus and save result to file in directory
    if (!dir.exists(dir_name)) dir.create(dir_name)
    filename <- paste0(dir_name, corpus, ".", p_attr, ".", s_attr)
    system(paste(
      "cwb-scan-corpus -o", filename, corpus, p_attr, s_attr, constraint)
    )
}, vectorize.args = c("corpus", "p_attr", "s_attr", "constraint"))

freq_list_to_dt <- function(path) {
  data.table::fread(path,
    sep = "\t",
    quote = "",
    na.string = "",
    header = FALSE,
    fill = TRUE,
    strip.white = TRUE,
    stringsAsFactors = TRUE,
    col.names = c("count", "p_attr", "s_attr")
  )
}

import_from_dir <- function(path, col_names) {
  # import scanned files; create columns with corpus and attributes
  files <- dir(path, full.names = TRUE)
  data.table::rbindlist(idcol = "corpus",
    sapply(files, freq_list_to_dt, simplify = FALSE)
  )[, (col_names) := lapply(FUN = as.factor,
        data.table::tstrsplit(split = ".", fixed = TRUE,
        gsub(paste0(path, "/"), "", corpus)
  ))]
}

# }}}
# {{{ Wrappers ------------------------------------------------------------

call_scan <- function(dir_name, parameters) invisible(
  # scan and create file;
  with(get_data_grid(parameters), cwb_scan(dir_name, corpus, p_attr, s_attr))
)

scan_import <- function(dir_name, parameters, col_names = names(parameters)) {
  # scan and create file; read them into data.table
  call_scan(dir_name, parameters)
  import_from_dir(dir_name, col_names)
}

# }}}
# {{{ Examples ------------------------------------------------------------
# lol <- list(
#   corpus = c("BASE"),
#   p_attr = c("word"),
#   s_attr = c("text_id")
# )

# full <- scan_import("data/", lol)
# full <- import_from_dir("data/", names(lol))
# call_scan("data/", lol)

# format(object.size(full), unit = "MB")

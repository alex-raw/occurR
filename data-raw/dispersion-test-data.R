# Reference implementation by Stephan Gries
# does rm(list = ls(all = TRUE)), so better encapsulate just to be safe
gries_env <- new.env()
source("http://www.stgries.info/research/dispersion/dispersions.r", gries_env)
gries_dispersion <- gries_env$dispersions2

raw <- data.frame(
  words = c("b", "a", "m", "n", "i", "b", "e", "u", "p", "b", "a", "s", "a", "t", "b", "e", "w", "q", "n", "b", "c", "a", "g", "a", "b", "e", "s", "t", "a", "b", "a", "g", "h", "a", "b", "e", "a", "a", "t", "b", "a", "h", "a", "a", "b", "e", "a", "x", "a", "t"),
  parts = paste0("p", rep(1:5, c(9, 10, 10, 10, 11)))
)
freq_list <- data.frame(table(raw))
freq_list <- freq_list[freq_list$Freq != 0, ]

raw <- read.table("/home/alexraw/Performance/occurR/blog/test_data/brown_word_id_full",
  quote = "", fill = TRUE, na.strings = "", header = FALSE,
  colClasses = c("factor", "factor"),
  col.names = c("words", "parts")
)

s <- with(raw, table(parts) / length(parts))
v <- with(raw, table(words, parts))

ans_gries <- apply(v, 1, gries_dispersion, s)
ans_gries <- data.frame(t(sapply(ans_gries, unlist)))
ans_gries <- data.frame(rownames(ans_gries), ans_gries)
rownames(ans_gries) <- NULL
colnames(ans_gries) <- c(
  "types", "f", "range", "maxmin", "sd.pop", "cv.pop", "chisq", "D.eq", "D",
  "S.eq", "S", "dc", "D2", "idf", "D3", "dp", "dp.norm", "kld", "U.eq",
  "U", "f.R.eq", "f.R", "Um", "engvall", "Ur"
)

ans_gries <- ans_gries[order(ans_gries$types), ]

write.table(ans_gries,
  file = "~/Performance/occurR/tests/testthat/toy_reference.tsv",
  sep = "\t",
  na = "",
  quote = FALSE,
  row.names = FALSE
)

freq_list <- data.frame(table(raw))
freq_list <- freq_list[freq_list$Freq != 0, ]

every <- occurR:::available_measures("disp")
comp <- occurR::dispersion(freq_list$Freq, freq_list$words, freq_list$parts, every)
colorder <- colnames(comp)
colorder <- Filter(function(x) x %in% colnames(ans_gries), colorder)
ans_gries <- ans_gries[, colorder]

write.table(ans_gries,
  file = "~/Performance/occurR/tests/testthat/toy_reference.tsv",
  sep = "\t",
  na = "",
  quote = FALSE,
  row.names = FALSE
)

hm <- read.table("toy_brown.tsv",
  header = TRUE,
  sep = "\t",
  na = "",
  quote = ""
)

# distance; produces a matrix of lists...
distances <- sapply(
  names(sort(table(raw$words))),
  dispersions1,
  corpus = raw$words,
  corpus.parts = raw$parts,
  # corpus.size.in.parts = 5,
  # corpus.size.in.units = 50,
  # corpus.part.sizes.in.perc = s,
  with.distance.measures = TRUE
)

distances <- apply(distances, 1, unlist)
last_six <- seq(nrow(distances) - 5, nrow(distances))
distances <- data.frame(distances[, last_six])
distances <- cbind(rownames(distances), distances)
colnames(distances) <- c("token", "arf", "awt", "f_awt", "ald", "f_ald", "washtell")
rownames(distances) <- NULL

utils::write.table(distances,
  file = "~/Performance/occurR/tests/testthat/toy_distances.tsv",
  sep = "\t",
  na = "",
  quote = FALSE,
  row.names = FALSE
)

# Toy corpus
path <- system.file("brown_reference.tsv", package = "occurR")
brown_reference <- read.table(path,
           header = TRUE,
           sep = "\t",
           na = "",
           quote = ""
)

raw <- data.frame(
  tokens = c("b", "a", "m", "n", "i", "b", "e", "u", "p", "b", "a", "s", "a", "t", "b", "e", "w", "q", "n", "b", "c", "a", "g", "a", "b", "e", "s", "t", "a", "b", "a", "g", "h", "a", "b", "e", "a", "a", "t", "b", "a", "h", "a", "a", "b", "e", "a", "x", "a", "t"),
  parts = paste0("p", rep(1:5, c(9, 10, 10, 10, 11)))
)

freq_list <- data.frame(table(raw))
freq_list <- freq_list[freq_list$Freq != 0, ]

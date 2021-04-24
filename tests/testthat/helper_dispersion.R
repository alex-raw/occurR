every <- Filter(function(x) x != "kld.norm", available_measures("disp"))

#---------------------------------------------------------- toy reference
toy_reference <- read.table("toy_reference.tsv", header = TRUE,
           sep = "\t", na = "", quote = "")
# toy calculation
toy <- data.frame(table(
  tokens = c("b", "a", "m", "n", "i", "b", "e", "u", "p", "b", "a", "s", "a", "t", "b", "e", "w", "q", "n", "b", "c", "a", "g", "a", "b", "e", "s", "t", "a", "b", "a", "g", "h", "a", "b", "e", "a", "a", "t", "b", "a", "h", "a", "a", "b", "e", "a", "x", "a", "t"),
  parts = paste0("p", rep(1:5, c(9, 10, 10, 10, 11)))))
toy <- toy[toy$Freq != 0, ]

v <- toy$Freq
tokens <- toy$tokens
parts <- toy$parts
ans1 <- dispersion(v, tokens, parts, every)

#---------------------------------------------------------- brown reference
brown_reference <- read.table("brown_reference.tsv", header = TRUE,
                              sep = "\t", na = "", quote = "")
brown_reference <- brown_reference[order(brown_reference$types), ]
rownames(brown_reference) <- NULL

# brown calculation
brown <- read.table("brown_word_id",
  quote = "", fill = TRUE, na.strings = "", header = FALSE,
  colClasses = c("numeric", "character", "character"),
  col.names = c("Freq", "tokens", "parts"))
v2 <- brown$Freq
tokens2 <- brown$tokens
parts2 <- brown$parts

ans2 <- dispersion(v2, tokens2, parts2, every)
ans2 <- ans2[order(ans2$types), ]
rownames(ans2) <- NULL


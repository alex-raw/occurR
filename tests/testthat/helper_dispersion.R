every <- Filter(function(x) x != "kld.norm", available_measures("disp"))
read_test_data <- function(path, ...)
  read.table(path, sep = "\t", na = "", quote = "", ...)

#---------------------------------------------------------- toy reference
toy_reference <- read_test_data("toy_reference.tsv", header = TRUE)
# toy calculation
toy <- data.frame(table(
  tokens = strsplit(c("bamnibeupbasatbewqnbcagabestabaghabeaatbahaabeaxat"), "")[[1]],
  parts = rep(1:5, c(9, 10, 10, 10, 11))
))
toy <- toy[toy$Freq != 0, ]

v <- toy$Freq
tokens <- toy$tokens
parts <- toy$parts
ans1 <- dispersion(v, tokens, parts, every)
ans1_fact <- dispersion(v, as.factor(tokens), as.factor(parts), every)
ans1 <- ans1[, colnames(toy_reference)]
ans1_fact <- ans1_fact[, colnames(toy_reference)]

#---------------------------------------------------------- brown reference
brown_reference <- read_test_data("brown_reference.tsv", header = TRUE)
brown_reference <- brown_reference[order(brown_reference$types), ]
rownames(brown_reference) <- NULL

# brown calculation
brown <- read_test_data("brown_word_id",
  colClasses = c("numeric", "character", "character"),
  col.names  = c("Freq", "tokens", "parts")
)
v2 <- brown$Freq
tokens2 <- brown$tokens
parts2 <- brown$parts

ans2 <- dispersion(v2, tokens2, parts2, every)
ans2 <- ans2[order(ans2$types), ]
ans2_fact <- dispersion(v2, as.factor(tokens2), as.factor(parts2), every)
ans2_fact <- ans2_fact[order(ans2_fact$types), ]
rownames(ans2) <- NULL

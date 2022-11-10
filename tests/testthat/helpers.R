.read_table <- \(...) {
  utils::read.table(..., sep = "\t", na = "", quote = "", header = TRUE)
}

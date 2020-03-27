input <- readLines("input")
tmp <- gsub(
  pattern = "#'.*$",
  replacement = "",
  x = input
)
empty_lines <- grepl(
  pattern = "^\\s*$",
  x = tmp
)
output <- tmp[!empty_lines]
writeLines(
  output,
  con = "output.txt"
)


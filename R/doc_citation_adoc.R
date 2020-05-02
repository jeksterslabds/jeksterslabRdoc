#' Convert Jekdoc Citation Tags to Asciidoctor Bibliography Tags
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams doc_retag
#' @importFrom gsubfn gsubfn
#' @export
#############################
# Add prefix and suffix later
#############################
doc_citation_adoc <- function(from_file = TRUE,
                              path,
                              jdoc) {
  if (from_file) {
    jdoc <- readLines(path)
  }
  foo <- function(tag,
                  jdoc) {
    gsubfn(
      pattern = paste0(
        tag,
        ":\\[.*?\\]"
      ),
      replacement = ~ gsub(
        pattern = ";[\\s]*",
        replacement = "]+[",
        x = x
      ),
      x = jdoc
    )

    # jdoc <- gsub(
    #  pattern = paste0(
    #    tag,
    #    ":\\[(.*?)\\]"
    #  ),
    #  replacement = "<\\1>",
    #  x = jdoc
    # )
    # jdoc <- gsub(
    #  pattern = "; (?=[^][{}]*})",
    #  replacement = "]+[",
    #  x = jdoc,
    #  perl = TRUE
    # )
    # gsub(
    #  pattern = "\\{(.*?)\\}",
    #  replacement = paste0(
    #    tag,
    #    ":[\\1]"
    #  ),
    #  x = jdoc
    # )
  }
  tag <- c(
    "citet",
    "citet\\*",
    "citealt",
    "citealt\\*",
    "citep",
    "citep\\*",
    "citealp",
    "citealp\\*",
    "citeauthor",
    "citeauthor\\*",
    "citeyear",
    "citeyearpar",
    "fullcite"
  )
  for (i in seq_along(tag)) {
    jdoc <- foo(
      tag = tag[i],
      jdoc = jdoc
    )
  }
  jdoc
}

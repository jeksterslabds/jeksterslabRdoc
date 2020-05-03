#' Convert Jekdoc Citation Tags to Biblatex/natbib Bibliography Tags
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param biblatex Logical.
#' If `TRUE`, convert to `Biblatex` tags.
#' If `FALSE`, convert to `natbib` tags.
#' @inheritParams doc_retag
#' @export
doc_citation_latex <- function(from_file = TRUE,
                               path,
                               jdoc,
                               biblatex = TRUE) {
  if (from_file) {
    jdoc <- readLines(path)
  }
  if (biblatex) {
    open <- c(
      "\\\\textcite{",
      "\\\\textcite*{",
      "\\\\cite{", # "\\\\citealt{", find biblatex equivalent
      "\\\\cite*{", # "\\\\citealt*{", find biblatex equivalent
      "\\\\parencite{",
      "\\\\parencite*{",
      "\\\\cite{",
      "\\\\cite*{",
      "\\\\citeauthor{",
      "\\\\citeauthor*{",
      "\\\\citeyear{",
      "\\\\citeyear{", # "\\\\citeyearpar{", find biblatex equivalent
      "\\\\fullcite{"
    )
  } else {
    open <- c(
      "\\\\citet{",
      "\\\\citet*{",
      "\\\\citealt",
      "\\\\citealt*{",
      "\\\\citep{",
      "\\\\citep*{",
      "\\\\citealp{",
      "\\\\citealp*{",
      "\\\\citeauthor{",
      "\\\\citeauthor*{",
      "\\\\citeyear{",
      "\\\\citeyearpar{",
      "\\\\fullcite{"
    )
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
  close <- rep(
    x = "}",
    times = length(tag)
  )
  doc_retag(
    tag = tag,
    open = open,
    close = close,
    jdoc = jdoc
  )
}

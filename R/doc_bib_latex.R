#' Bibliography.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param bib_path Character string.
#'   Path to Biblatex file.
#' @param style Character string.
#'   Bibliography style.
#' @export
doc_bib_latex <- function(bib_path,
                          style = "apa") {
  if (style == "apa") {
    language <- "\\DeclareLanguageMapping{american}{american-apa}"
  } else {
    language <- ""
  }
  paste(
    "\\usepackage[american]{babel}",
    "\\usepackage{csquotes}",
    paste0(
      "\\usepackage[style=",
      style,
      ",sortcites=true,sorting=nyt,backend=biber]{biblatex}"
    ),
    language,
    paste0(
      "\\addbibresource{",
      bib_path,
      "}"
    ),
    sep = "\n"
  )
}

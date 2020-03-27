#' Bibliography.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param bib_path Character string.
#'   Path to Biblatex file.
#' @inheritParams doc_attr_authorheader
#' @export
doc_bib <- function(bib_path,
                    format) {
  if (format == "adoc") {
    return(
      paste0(
        "// Bibliography",
        "\n",
        ":bibliography-database: ",
        bib_path,
        "\n",
        ":bibliography-style: apa"
      )
    )
  }
  if (format == "latex") {
    return(
      paste0(
        "\\usepackage[american]{babel}",
        "\n\n",
        "\\usepackage{csquotes}",
        "\n",
        "\\usepackage[style=apa,sortcites=true,sorting=nyt,backend=biber]{biblatex}",
        "\n",
        "\\DeclareLanguageMapping{american}{american-apa}",
        "\n",
        "\\addbibresource{",
        bib_path,
        "}"
      )
    )
  }
}

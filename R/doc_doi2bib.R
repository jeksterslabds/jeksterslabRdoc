#' Digital Object Identifiers to Bibtex using the Crossref API
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param doi Character vector.
#'   A vector of Digital Object Identifiers.
#' @inheritParams jeksterslabRutils::util_lapply
#' @importFrom jeksterslabRutils util_lapply
#' @importFrom curl curl
#' @export
doc_doi2bib <- function(doi,
                        par = TRUE,
                        ncores = NULL) {
  foo <- function(doi) {
    con <- paste0(
      "http://api.crossref.org/works/",
      doi,
      "/transform/application/x-bibtex"
    )
    paste0(
      readLines(
        curl(url = con)
      ),
      collapse = "\n"
    )
  }
  output <- util_lapply(
    FUN = foo,
    args = list(doi = doi),
    par = par,
    ncores = ncores
  )
  do.call(
    what = "rbind",
    args = output
  )
}

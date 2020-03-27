#' Asciidoc Preamble
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param jdoc_abs_path Character string.
#'   Jekdoc abstract input file path
#'   (e.g., jdoc_abs_path = "~/abstract.jdoc").
#' @param attr_path Character string.
#'   Path to attributes file.
#'   (e.g., attr_path = "~/attributes.adoc").
#' @param bib_path Character string.
#'   Path to Biblatex file.
#'   (e.g., bib_path = "~/references.bib").
#' @export
doc_preamble_adoc <- function(jdoc_abs_path,
                              attr_path,
                              bib_path) {
  format <- "adoc"
  # attr
  attr <- readLines(attr_path)
  keys <- c(
    "title"
  )
  attributes <- doc_attr(
    key = keys,
    attr_from_file = FALSE,
    attr = attr
  )
  names(attributes) <- keys
  # attr_title
  title <- attributes["title"]
  # attr_authorheader
  authorheader <- doc_attr_authorheader(
    attr_from_file = FALSE,
    attr = attr,
    format = format
  )
  # attr_authornote
  authornote <- doc_attr_authornote(
    attr_from_file = FALSE,
    attr = attr,
    format = format
  )
  # abstract
  abstract <- doc_abstract(
    jdoc_abs_from_file = TRUE,
    jdoc_abs_path = jdoc_abs_path,
    attr_from_file = FALSE,
    attr = attr,
    format = format
  )
  attr <- paste0(
    attr,
    collapse = "\n"
  )
  attr_bib <- doc_bib(
    bib_path = bib_path,
    format = format
  )
  paste0(
    "= ",
    title,
    "\n",
    authorheader,
    "\n",
    attr,
    "\n",
    attr_bib,
    "\n\n",
    authornote,
    "\n\n",
    abstract
  )
}

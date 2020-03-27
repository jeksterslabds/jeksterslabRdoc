#' LaTeX Preamble
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param mode Character string.
#'  APA7 LaTeX mode.
#'  The options are
#'  \code{"jou"} for journal,
#'  \code{"man"} for manuscript, and
#'  \code{"stu"} for student paper.
#' @param paper Character string.
#'   Paper size.
#'   The options are
#'   \code{"letter"} and
#'   \code{"a4paper"}.
#' @param packages Character vector.
#'   Additional packages to include in the preamble.
#' @param etc Character vector.
#'   Extra text to include in the preamble.
#' @inheritParams doc_attr
#' @inheritParams doc_abstract
#' @inheritParams doc_bib
#' @export
doc_preamble_latex <- function(mode = "man",
                               paper = "a4paper",
                               packages = c(
                                 "mathtools",
                                 "hyperref"
                               ),
                               etc = NULL,
                               jdoc_abs_path,
                               attr_from_file = FALSE,
                               attr_path,
                               attr,
                               bib_path) {
  if (is.null(etc)) {
    etc <- ""
  }
  if (attr_from_file) {
    attr <- readLines(attr_path)
  }
  key <- c(
    "title",
    "shorttitle"
  )
  attributes <- doc_attr(
    key = key,
    attr_from_file = FALSE,
    attr = attr
  )
  names(attributes) <- key
  title <- paste0(
    "\\title{",
    attributes["title"],
    "}"
  )
  shorttitle <- paste0(
    "\\shortitle{",
    attributes["shorttitle"],
    "}"
  )
  documentclass <- paste0(
    "\\documentclass[",
    mode,
    ",",
    paper,
    "]",
    "{apa7}"
  )
  packages <- paste0(
    "\\usepackage{",
    packages,
    "}",
    collapse = "\n"
  )
  bib <- doc_bib(
    bib_path = bib_path,
    format = "latex"
  )
  authorheader <- doc_attr_authorheader(
    attr_from_file = FALSE,
    attr = attr,
    format = "latex"
  )
  abstract <- doc_abstract(
    jdoc_abs_from_file = TRUE,
    jdoc_abs_path = jdoc_abs_path,
    attr_from_file = FALSE,
    attr = attr,
    format = "latex"
  )
  authornote <- doc_attr_authornote(
    attr_from_file = FALSE,
    attr = attr,
    format = "latex"
  )
  paste0(
    documentclass,
    "\n\n",
    packages,
    "\n\n",
    etc,
    "\n\n",
    bib,
    "\n\n",
    title,
    "\n",
    shorttitle,
    "\n\n",
    authorheader,
    "\n\n",
    abstract,
    "\n\n",
    authornote,
    "\n\n",
    collapse = "\n\n"
  )
}

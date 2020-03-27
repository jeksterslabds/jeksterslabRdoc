#' Replace Jekdoc Tags and Output Asciidoctor File
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams doc_retag
#' @export
doc_retag_adoc <- function(jdoc_from_file = TRUE,
                           jdoc_path,
                           jdoc) {
  if (jdoc_from_file) {
    jdoc <- readLines(jdoc_path)
  }
  tag <- c(
    # headings
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    # bold and italic
    "blditl",
    "bold",
    "italic",
    # quotation
    "doublequote",
    "singlequote",
    # subscript and superscript
    "subscript",
    "superscript",
    # monospace
    "monospace",
    # accents
    "grave",
    # reference
    "eqref",
    # table
    "table",
    # references
    "references",
    # appendix
    "appendix"
  )
  open <- c(
    # headings
    "\\=\\= ",
    "\\=\\=\\= ",
    "\\=\\=\\=\\= ",
    "\\=\\=\\=\\=\\= ",
    "\\=\\=\\=\\=\\=\\= ",
    # bold and italic
    "*_",
    "*",
    "_",
    # quotation
    "\"\`",
    "\'\`",
    # subscript and superscript
    "~",
    "^",
    # monospace
    "\`",
    # accents
    "\\&",
    # reference
    "\\\\eqref{",
    # table
    ",\\=\\=\\=",
    # references
    "\\=\\= References",
    # appendix
    "\\=\\= Appendix"
  )
  close <- c(
    # headings
    rep(x = "", times = 5),
    # bold and italic
    "_*",
    "*",
    "_",
    # quotation
    "\`\"",
    "\`\'",
    "~",
    "^",
    # monospace
    "\`",
    # accents
    "grave;",
    "}",
    # table
    "",
    # references
    "",
    # appendix
    ""
  )
  jdoc <- doc_retag(
    tag = tag,
    open = open,
    close = close,
    jdoc = jdoc
  )
  tag <- c(
    "noindent"
  )
  doc_rm_tag(
    tag = tag,
    jdoc = jdoc
  )
}

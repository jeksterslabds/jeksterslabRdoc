#' Replace Jekdoc Tags and Output Asciidoctor File
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams doc_retag
#' @export
doc_retag_adoc <- function(from_file = TRUE,
                           path,
                           jdoc) {
  if (from_file) {
    jdoc <- readLines(path)
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
    # references
    "references",
    # appendix
    "appendix",
    # comment
    "comment",
    # r inline
    "\\br",
    # r chunk
    "begin\\.rcode",
    "end\\.rcode"
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
    # references
    "\\=\\= References",
    # appendix
    "\\=\\= Appendix",
    # comment
    "// ",
    # r inline
    "+r ",
    # r chunk
    "// begin.rcode ",
    "// end.rcode"
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
    # subscript and superscript
    "~",
    "^",
    # monospace
    "\`",
    # accents
    "grave;",
    # reference
    "}",
    # references
    "",
    # appendix
    "",
    # comment
    "",
    # r inline
    " +",
    # r chunk
    "",
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

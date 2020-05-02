#' Replace Jekdoc Tags and Output Asciidoctor File
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams doc_retag
#' @export
doc_retag_latex <- function(from_file = TRUE,
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
    # noindent
    "noindent"
    # find the best way to convert the following from jdoc to latex
    # table
    # "table",
    # references
    # "references",
    # appendix
    # "appendix"
  )
  open <- c(
    # headings
    "\\\\section{",
    "\\\\subsection{",
    "\\\\subsubsection{",
    "\\\\paragraph{",
    "\\\\subparagraph{",
    # bold and italic
    "\\\\textbf{\\emph{",
    "\\\\textbf{",
    "\\\\emph{",
    # quotation
    "\\\\enquote{",
    "\\\\enquote*{",
    # subscript and superscript
    "\\\\textsuperscript{",
    "\\\\textsubscript{",
    # monospace
    "\\\\texttt{",
    # accents
    "\\\\`{",
    # reference
    "\\\\ref{",
    # noindent
    "\\\\noindent{"
    # table
    # create a function to make latex tables from jdoc
    # ",\\=\\=\\=",
    # references
    # "\\=\\= References",
    # appendix
    # "\\=\\= Appendix"
  )
  close <- c(
    # headings
    rep(x = "}", times = 5),
    # bold and italic
    "}}",
    "}",
    "}",
    # quotation
    "}",
    "}",
    # subscript and superscript
    "}",
    "}",
    # monospace
    "}",
    # accents
    "}",
    # reference
    "}",
    # noindent
    "}"
    # table
    # "",
    # references
    # "",
    # appendix
    # ""
  )
  latex <- doc_retag(
    tag = tag,
    open = open,
    close = close,
    jdoc = jdoc
  )
  # asciidoc comments
  latex <- strsplit(
    x = latex,
    split = "\n"
  )
  latex <- gsub(
    pattern = "^\\{2}[[:space:]]",
    replacement = "% ",
    x = latex
  )
  paste0(
    latex,
    collapse = "\n"
  )
}

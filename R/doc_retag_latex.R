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
    "noindent",
    # references
    "references",
    # appendix
    "appendix",
    # comment
    "comment",
    # math
    "stem",
    # r inline
    "\\br",
    # r chunk
    "begin.rcode",
    "end.rcode"
  )
  open <- c(
    # headings
    "\\\\section{",
    "\\\\subsection{",
    "\\\\subsubsection{",
    "\\\\paragraph{",
    "\\\\subparagraph{",
    # bold and italic
    "\\\\textbf{\\\\emph{",
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
    "\\\\noindent{",
    # references
    "",
    # appendix
    "\\\\appendix",
    # comment
    "%% ",
    # math
    "",
    # r inline
    "\\\\rinline{",
    # r chunk
    "%% begin.rcode ",
    "%% end.rcode"
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
    "}",
    # references
    "",
    # appendix
    "",
    # comment
    "",
    # math
    "",
    # r inline
    "}",
    # r chunk
    "",
    ""
  )
  latex <- doc_retag(
    tag = tag,
    open = open,
    close = close,
    jdoc = jdoc
  )
  # Bibliography
  latex <- gsub(
    pattern = "bibliography::\\[\\]",
    replacement = "\\\\printbibliography",
    x = latex
  )
  # Block math
  latex <- gsub(
    pattern = "\\b\\[stem\\]\\b",
    replacement = "",
    x = latex
  )
  gsub(
    pattern = "\\b\\+\\+\\+\\+\\b",
    replacement = "",
    x = latex
  )
}

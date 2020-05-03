#' Generate Manuscript
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param output_dir Character string.
#'   Output directory.
#' @param mode Character string.
#'  APA7 LaTeX mode.
#'  The options are
#'  `"jou"` for journal,
#'  `"man"` for manuscript, and
#'  `"stu"` for student paper.
#' @param paper Character string.
#'   Paper size.
#'   The options are
#'   `"letter"` and
#'   `"a4paper"`.
#' @param packages Character vector.
#'   Additional LaTeX packages to include in the preamble.
#' @param etc Character vector.
#'   Extra text to include in the preamble.
#' @inheritParams doc_attributes
#' @inheritParams doc_retag
#' @export
doc_manuscript <- function(jdoc,
                           attributes_yml,
                           output_dir = getwd(),
                           author_latex = NULL,
                           affiliation_latex = NULL,
                           add_authornote = NULL,
                           mode = "man",
                           paper = "a4paper",
                           packages = NULL,
                           etc = NULL) {
  output_dir <- normalizePath(output_dir)
  wd <- getwd()
  tmp_dir <- file.path(
    tempdir(),
    "manuscript"
  )
  dir.create(tmp_dir)
  tmp_output_adoc <- paste0(
    tempfile(),
    ".adoc"
  )
  tmp_output_tex <- paste0(
    tempfile(),
    ".tex"
  )
  tmp_output_html <- paste0(
    tempfile(),
    ".html"
  )
  tmp_output_pdf <- paste0(
    tempfile(),
    ".pdf"
  )
  on.exit(
    unlink(
      c(
        tmp_dir,
        tmp_output_adoc,
        tmp_output_tex,
        tmp_output_html,
        tmp_output_pdf
      ),
      recursive = TRUE
    )
  )
  setwd(tmp_dir)
  doctypes <- c(
    "adoc",
    "tex",
    "html",
    "pdf"
  )
  ext <- paste0(
    ".",
    doctypes
  )
  fn <- paste0(
    "manuscript",
    ext
  )
  fn <- file.path(
    output_dir,
    fn
  )
  names(fn) <- doctypes
  attributes <- doc_attributes(
    attributes_yml = attributes_yml,
    author_latex = author_latex,
    affiliation_latex = affiliation_latex,
    add_authornote = add_authornote
  )
  title_adoc <- attributes[["title"]]
  title_latex <- paste0("\\title{", title_adoc, "}")
  authornote_adoc <- attributes[["authorinfo"]][["authornote"]][["adoc"]]
  authornote_latex <- attributes[["authorinfo"]][["authornote"]][["latex"]]
  author_latex <- attributes[["authorinfo"]][["author_latex"]]
  affiliation_latex <- attributes[["authorinfo"]][["affiliation_latex"]]
  leftheader <- attributes[["authorinfo"]][["leftheader"]]
  abstract_adoc <- attributes[["abstract"]][["adoc"]][["block"]]
  abstract_latex <- attributes[["abstract"]][["latex"]][["block"]]
  attributes_adoc <- attributes[["attributes"]][["adoc"]]
  attributes_latex <- attributes[["attributes"]][["latex"]]
  bibliography <- attributes[["bibliography"]]
  knitr_preamble <- readLines(
    system.file(
      "extdata",
      "knitr_preamble",
      package = "jeksterslabRdoc",
      mustWork = TRUE
    )
  )
  knitr_preamble <- paste0(
    knitr_preamble,
    collapse = "\n"
  )
  if (!is.null(packages)) {
    packages <- paste0(
      "\\usepackage{",
      packages,
      "}",
      collapse = "\n"
    )
  } else {
    packages <- ""
  }
  doc_knit_adoc(
    input = jdoc,
    output = tmp_output_adoc
  )
  doc_knit_latex(
    input = jdoc,
    output = tmp_output_tex
  )
  body_adoc <- doc_retag_adoc(
    from_file = TRUE,
    path = tmp_output_adoc
  )
  body_adoc <- doc_citation_adoc(
    from_file = FALSE,
    jdoc = body_adoc
  )
  body_latex <- doc_retag_latex(
    from_file = TRUE,
    path = tmp_output_tex
  )
  body_latex <- doc_citation_latex(
    from_file = FALSE,
    jdoc = body_latex
  )
  output_adoc <- paste0(
    "= ",
    title_adoc,
    "\n",
    attributes_adoc,
    "\n\n",
    authornote_adoc,
    "\n\n",
    abstract_adoc, "\n",
    "\n\n",
    paste0(
      body_adoc,
      collapse = "\n"
    )
  )
  output_adoc <- gsub(
    pattern = "\n{3,}",
    replacement = "\n\n",
    x = output_adoc
  )
  output_latex <- paste0(
    "\\documentclass[", mode, "]{apa7}", "\n",
    knitr_preamble, "\n",
    "\\usepackage[american]{babel}", "\n",
    packages, "\n",
    bibliography, "\n",
    title_latex, "\n",
    author_latex, "\n",
    affiliation_latex, "\n",
    leftheader, "\n",
    authornote_latex, "\n",
    abstract_latex, "\n",
    # convert it to hyperref
    # attributes_latex, "\n",
    "\\begin{document}", "\n",
    "\\maketitle", "\n",
    "\n\n",
    paste0(
      body_latex,
      collapse = "\n"
    ),
    "\n",
    "\\end{document}"
  )
  output_latex <- gsub(
    pattern = "\n{3,}",
    replacement = "\n\n",
    x = output_latex
  )
  writeLines(
    text = output_adoc,
    con = tmp_output_adoc
  )
  writeLines(
    text = output_latex,
    con = tmp_output_tex
  )
  system(
    paste(
      "asciidoctor -r asciidoctor-bibliography",
      paste0(
        "--out-file=",
        shQuote(
          tmp_output_html
        )
      ),
      shQuote(
        tmp_output_adoc
      )
    )
  )
  writeLines(
    text = readLines(tmp_output_adoc),
    con = fn[["adoc"]]
  )
  writeLines(
    text = readLines(tmp_output_tex),
    con = fn[["tex"]]
  )
  writeLines(
    text = readLines(tmp_output_html),
    con = fn[["html"]]
  )
  writeLines(
    text = "FOR PDF",
    con = fn[["pdf"]]
  )
  setwd(wd)
}

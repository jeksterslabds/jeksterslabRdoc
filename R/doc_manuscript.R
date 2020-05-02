#' Generate Manuscript
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param output_dir Character string.
#'   Output directory.
#' @inheritParams doc_attributes
#' @inheritParams doc_retag
#' @export
doc_manuscript <- function(jdoc,
                           attributes_yml,
                           output_dir = getwd(),
                           author_latex = NULL,
                           affiliation_latex = NULL,
                           add_authornote = NULL) {
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
  title_adoc <- attributes[["processed"]][["title"]]
  authornote_adoc <- attributes[["authorinfo"]][["authornote"]][["adoc"]]
  attributes_adoc <- attributes[["attributes"]][["adoc"]]
  doc_knit_adoc(
    input = jdoc,
    output = tmp_output_adoc
  )
  body_adoc <- doc_retag_adoc(
    from_file = TRUE,
    path = tmp_output_adoc
  )
  body_adoc <- doc_citation_adoc(
    from_file = FALSE,
    jdoc = body_adoc
  )
  output_adoc <- paste0(
    "= ",
    title_adoc,
    "\n",
    attributes_adoc,
    "\n\n",
    authornote_adoc,
    "\n\n",
    attributes[["abstract"]][["adoc"]][["block"]],
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
  writeLines(
    text = output_adoc,
    con = tmp_output_adoc
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
    # con = fn[["adoc"]]
    con = "~/adoc.adoc"
  )
  writeLines(
    text = "LATEX FILE GOES HERE",
    # con = fn[["tex"]]
    con = "~/tex.tex"
  )
  writeLines(
    text = readLines(tmp_output_html),
    # con = fn[["html"]]
    con = "~/html.html"
  )
  writeLines(
    text = "FOR PDF",
    con = "~/pdf.pdf"
  )
  setwd(wd)
}

#' Abstract.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param abstract Character string.
#'   Abstract with `Jekdoc` tags.
#' @param keywords Character string.
#'   Keywords separated by commas.
#' @export
doc_abstract <- function(abstract,
                         keywords) {
  tmp_input <- tempfile()
  tmp_output_adoc <- tempfile()
  tmp_output_latex <- tempfile()
  writeLines(
    text = abstract,
    con = tmp_input
  )
  doc_knit_adoc(
    input = tmp_input,
    output = tmp_output_adoc
  )
  doc_knit_latex(
    input = tmp_input,
    output = tmp_output_latex
  )
  abstract_adoc <- readLines(tmp_output_adoc)
  abstract_latex <- readLines(tmp_output_latex)
  abstract_adoc <- trimws(
    paste0(
      abstract_adoc,
      collapse = "\n"
    )
  )
  abstract_latex <- trimws(
    paste0(
      abstract_latex,
      collapse = "\n"
    )
  )
  keywords <- trimws(
    keywords
  )
  abstract_adoc <- doc_retag_adoc(
    from_file = FALSE,
    jdoc = abstract_adoc
  )
  abstract_latex <- doc_retag_latex(
    from_file = FALSE,
    jdoc = abstract_latex
  )
  abstract_adoc <- doc_citation_adoc(
    from_file = FALSE,
    jdoc = abstract_adoc
  )
  # add this later
  # abstract_latex <- doc_citation_latex(
  #  from_file = FALSE,
  #  jdoc = abstract_latex
  # )
  adoc <- trimws(
    paste0(
      "[abstract]",
      "\n",
      ".Abstract",
      "\n",
      "--",
      "\n",
      abstract_adoc,
      "\n",
      "--",
      "\n\n",
      "_Keywords:_",
      " ",
      keywords
    )
  )
  latex <- trimws(
    paste0(
      "\\abstract{",
      abstract_latex,
      "}",
      "\n\n",
      "\\keywords{",
      keywords,
      "}"
    )
  )
  adoc <- list(
    block = adoc,
    text = abstract_adoc
  )
  latex <- list(
    block = latex,
    text = abstract_latex
  )
  list(
    adoc = adoc,
    latex = latex
  )
}

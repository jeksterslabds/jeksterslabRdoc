#' Convert Jekdoc file to Asciidoc file.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param jdoc_path Character string.
#'   Jekdoc input file path
#'   (e.g., jdoc_path = "~/sample.jdoc").
#' @param adoc Character string.
#'   Asciidoc output file.
#'   If \code{NULL},
#'   extracts the root of \code{jdoc}.
#' @param html Character string.
#'   HTML output file.
#'   If \code{NULL},
#'   extracts the root of \code{jdoc}.
#' @param output_dir Path.
#'   Output directory.
#' @inheritParams doc_preamble_adoc
#' @importFrom knitr knit
#' @importFrom tools file_path_sans_ext
#' @export
doc_jdoc2adoc2html <- function(jdoc_path,
                               jdoc_abs_path,
                               attr_path,
                               bib_path,
                               adoc = NULL,
                               html = NULL,
                               output_dir = getwd()) {
  #############################################################################
  # preamble
  #############################################################################
  preamble <- doc_preamble_adoc(
    jdoc_abs_path = jdoc_abs_path,
    attr_path = attr_path,
    bib_path = bib_path
  )
  #############################################################################
  # knit
  #############################################################################
  knit_file <- tempfile()
  doc_knit(
    input = jdoc_path,
    output = knit_file
  )
  #  tmp <- tempfile()
  #  knit_file <- tempfile()
  #  # step 1
  #  knit(
  #    input = jdoc_path,
  #    output = tmp,
  #    quiet = TRUE
  #  )
  #  # step 2
  #  knit(
  #    input = tmp,
  #    output = knit_file,
  #    quiet = TRUE
  #  )
  #############################################################################
  # adoc
  #############################################################################
  adoc_file <- paste0(readLines(knit_file), collapse = "\n")
  #############################################################################
  # adoc - retag
  #############################################################################
  adoc_file <- doc_retag_adoc(
    jdoc_from_file = FALSE,
    jdoc = adoc_file
  )
  adoc_file <- paste0(
    adoc_file,
    collapse = "\n"
  )
  #############################################################################
  # combine parts
  #############################################################################
  adoc_file <- paste0(
    preamble,
    "\n\n",
    adoc_file
  )
  #############################################################################
  # adoc - trim extra whitespace
  #############################################################################
  adoc_file <- trimws(adoc_file)
  adoc_file <- gsub(
    pattern = "\n{3,}",
    replacement = "\n\n",
    x = adoc_file
  )
  #############################################################################
  # write to adoc file
  #############################################################################
  if (is.null(adoc)) {
    adoc <- paste0(
      file_path_sans_ext(
        basename(path = jdoc_path)
      ),
      ".adoc"
    )
  }
  adoc_file_path <- file.path(
    output_dir,
    adoc
  )
  writeLines(
    text = adoc_file,
    con = adoc_file_path
  )
  cat(
    "See output Asciidoc file:",
    adoc_file_path,
    "\n"
  )
  #############################################################################
  # write to html file
  #############################################################################
  if (is.null(html)) {
    html <- paste0(
      file_path_sans_ext(
        basename(path = jdoc_path)
      ),
      ".html"
    )
  }
  html_file_path <- file.path(
    output_dir,
    html
  )
  system(
    paste(
      "asciidoctor -r asciidoctor-bibliography",
      adoc_file_path,
      "-o",
      html_file_path
    )
  )
  cat(
    "See output HTML file:",
    html_file_path,
    "\n"
  )
}

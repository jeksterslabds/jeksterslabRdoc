#' Abstract.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param jdoc_abs_from_file Logical.
#'   If \code{TRUE},
#'   Jekdoc abstract is loaded from a file.
#' @param jdoc_abs_path Path to Jekdoc abstract file.
#'   Ignored if \code{jdoc_abs_from_file = FALSE}.
#' @param jdoc_abs Character string.
#'   Jekdoc abstract input file.
#' @inheritParams doc_attr_authorheader
#' @export
doc_abstract <- function(jdoc_abs_from_file,
                         jdoc_abs_path,
                         jdoc_abs,
                         attr_from_file,
                         attr_path,
                         attr,
                         format) {
  if (jdoc_abs_from_file) {
    jdoc_abs <- readLines(jdoc_abs_path)
  }
  jdoc_abs <- paste0(
    jdoc_abs,
    collapse = "\n"
  )
  if (attr_from_file) {
    attr <- readLines(attr_path)
  }
  keywords <- doc_attr(
    key = "keywords",
    attr_from_file = FALSE,
    attr = attr
  )
  if (format == "adoc") {
    jdoc_abs <- doc_retag_adoc(
      jdoc_from_file = FALSE,
      jdoc = jdoc_abs
    )
    return(
      paste0(
        "[abstract]",
        "\n",
        ".Abstract",
        "\n",
        "--",
        "\n",
        jdoc_abs,
        "\n",
        "--",
        "\n\n",
        "_Keywords:_",
        " ",
        keywords
      )
    )
  }
  if (format == "latex") {
    # add retag
    return(
      paste0(
        "\\abstract{",
        jdoc_abs,
        "}",
        "\n\n",
        "\\keywords{",
        keywords,
        "}"
      )
    )
  }
}

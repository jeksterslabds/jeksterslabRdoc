#' Knit Jekdoc to Asciidoc.
#'
#' Converts `Jekdoc` `R` inline code and chunks to `Asciidoc` format
#' before `knitting`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams doc_knit
#' @export
doc_knit_adoc <- function(input,
                          output = NULL) {
  tmp_input <- paste0(
    tempfile(),
    ".Rasciidoc"
  )
  on.exit(
    unlink(tmp_input)
  )
  if (is.null(output)) {
    dir <- dirname(
      input
    )
    ext <- file_ext(
      basename(input)
    )
    if (ext != "") {
      ext <- paste0(
        ".",
        ext,
        ".",
        "adoc"
      )
    }
    if (ext == "") {
      ext <- paste0(
        ".",
        "adoc"
      )
    }
    output <- file_path_sans_ext(
      basename(input)
    )
    output <- paste0(
      output,
      ext,
      ".",
      "knit"
    )
    output <- file.path(
      dir,
      output
    )
  }
  jdoc <- readLines(input)
  tag <- c(
    # r inline
    "\\br",
    # r chunk
    "begin\\.rcode",
    "end\\.rcode"
  )
  open <- c(
    # r inline
    "+r ",
    # r chunk
    "// begin.rcode ",
    "// end.rcode"
  )
  close <- c(
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
  writeLines(
    text = jdoc,
    con = tmp_input
  )
  doc_knit(
    input = tmp_input,
    output = output
  )
}

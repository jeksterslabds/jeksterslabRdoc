#' Knit Jekdoc to LaTeX.
#'
#' Converts `Jekdoc` `R` inline code and chunks to `LaTeX` format
#' before `knitting`.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams doc_knit
#' @export
doc_knit_latex <- function(input,
                           output = NULL) {
  tmp_input <- paste0(
    tempfile(),
    ".Rtex"
  )
  tmp_intermediate <- paste0(
    tempfile(),
    ".Rtex"
  )
  on.exit(
    unlink(
      c(
        tmp_input,
        tmp_intermediate
      )
    )
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
        "tex"
      )
    }
    if (ext == "") {
      ext <- paste0(
        ".",
        "tex"
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
    "begin.rcode",
    "end.rcode"
  )
  open <- c(
    # r inline
    "\\\\rinline{",
    # r chunk
    "%% begin.rcode ",
    "%% end.rcode"
  )
  close <- c(
    # r inline
    "}",
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

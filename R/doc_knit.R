#' Knit Document.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param input Character string.
#'   Input path.
#' @param output Character string.
#'   Output path.
#' @importFrom tools file_path_sans_ext
#' @importFrom tools file_ext
#' @importFrom knitr knit
#' @export
doc_knit <- function(input,
                     output = NULL) {
  ext <- file_ext(
    basename(input)
  )
  tmp <- paste0(
    tempfile(),
    ".",
    ext
  )
  on.exit(
    unlink(
      tmp
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
        ext
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
  # step 1
  knit(
    input = input,
    output = tmp,
    quiet = TRUE
  )
  # step 2
  knit(
    input = tmp,
    output = output,
    quiet = TRUE
  )
  if (interactive()) {
    cat(
      paste(
        "doc_knit output file:",
        output,
        "\n"
      )
    )
  }
}

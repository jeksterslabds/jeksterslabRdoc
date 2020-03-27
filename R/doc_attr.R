#' Extract Asciidoctor Attribute Value Using Attribute Key.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param key Character vector.
#'   Asciidoctor attribute key.
#' @param attr_from_file Logical.
#'   If \code{TRUE},
#'   attributes (\code{attr}) are extracted from a file.
#' @param attr_path Character string.
#'   Path to attributes file.
#'   Ignored if \code{attr_from_file = FALSE}.
#' @param attr Character vector.
#'   Asciidoctor attributes key-value pairs.
#'   Ignored if \code{attr_from_file = TRUE}.
#' @importFrom jeksterslabRutils util_lapply
#' @export
doc_attr <- function(key,
                     attr_from_file,
                     attr_path,
                     attr) {
  if (attr_from_file) {
    attr <- readLines(attr_path)
  }
  exe <- function(key,
                  attr) {
    pattern <- paste0(
      "^:",
      key,
      ":"
    )
    gsub(
      pattern = pattern,
      replacement = "",
      x = grep(
        pattern = pattern,
        x = attr,
        value = TRUE
      )
    )
  }
  output <- util_lapply(
    FUN = exe,
    args = list(
      key = key,
      attr = attr
    ),
    par = FALSE
  )
  trimws(unlist(output))
}

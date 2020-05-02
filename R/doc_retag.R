#' Replace Jekdoc Tags
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param tag Character vector.
#'   Jekdoc tags.
#' @param open Character vector.
#'   Opening tag replacement.
#' @param close Character vector.
#'   Closing tag replacement.
#' @param from_file Logical.
#'   If \code{TRUE},
#'   Jekdoc (\code{jdoc}) is loaded from a file.
#' @param path Path to Jekdoc file.
#'   Ignored if \code{from_file = FALSE}.
#' @param jdoc Character string.
#'   Jekdoc input file.
#' @export
doc_retag <- function(tag,
                      open,
                      close,
                      from_file = FALSE,
                      path,
                      jdoc) {
  if (!all.equal(
    length(tag),
    length(open),
    length(close)
  )) {
    stop("tag, open, and close should have the same length.\n")
  }
  if (from_file) {
    jdoc <- paste0(
      readLines(path),
      collapse = "\n"
    )
  }
  exe <- function(tag,
                  open,
                  close,
                  jdoc) {
    gsub(
      pattern = paste0(
        tag,
        # ":\\[([^]]*)\\]"
        ":\\[(.*?)\\]"
      ),
      replacement = paste0(
        open,
        "\\1",
        close
      ),
      x = jdoc
    )
  }
  for (i in seq_along(tag)) {
    jdoc <- exe(
      tag = tag[i],
      open = open[i],
      close = close[i],
      jdoc = jdoc
    )
  }
  trimws(jdoc)
}

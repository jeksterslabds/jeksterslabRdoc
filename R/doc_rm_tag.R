#' Remove Jekdoc Tags
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams doc_retag
#' @export
doc_rm_tag <- function(tag,
                       jdoc) {
  exe <- function(tag,
                  jdoc) {
    gsub(
      pattern = paste0(
        tag,
        ":\\[\\][[:space:]]"
      ),
      replacement = "",
      x = jdoc
    )
  }
  for (i in seq_along(tag)) {
    jdoc <- exe(
      tag = tag[i],
      jdoc = jdoc
    )
  }
  jdoc
}

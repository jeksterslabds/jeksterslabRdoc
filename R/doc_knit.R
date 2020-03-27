#' Knit Document.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param input Character string.
#'   Input path.
#' @param output Character string.
#'   Output path.
#' @export
doc_knit <- function(input,
                     output) {
  tmp <- tempfile()
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
}

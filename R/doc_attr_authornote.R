#' Author Note.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams doc_attr_authorheader
#' @export
doc_attr_authornote <- function(attr_from_file,
                                attr_path,
                                attr,
                                format) {
  if (attr_from_file) {
    attr <- readLines(attr_path)
  }
  keys <- c(
    "corresponding_author",
    "corresponding_address",
    "corresponding_email"
  )
  attributes <- doc_attr(
    key = keys,
    attr_from_file = FALSE,
    attr = attr
  )
  names(attributes) <- keys
  content <- paste0(
    doc_attr_authorinfo(
      attr_from_file = FALSE,
      attr = attr,
      format = format
    ),
    "\n\n",
    "We have no known conflict of interest to disclose.",
    "\n\n",
    "Correspondence concerning this article should be addressed to",
    " ",
    attributes["corresponding_author"],
    ", ",
    attributes["corresponding_address"],
    ". ",
    "E-mail: ",
    attributes["corresponding_email"],
    ".",
    collapse = "\n"
  )
  if (format == "adoc") {
    return(
      paste0(
        "[authornote]",
        "\n",
        ".Author Note",
        "\n",
        "--",
        "\n",
        content,
        "\n",
        "--",
        "\n",
        collapse = ""
      )
    )
  }
  if (format == "latex") {
    return(
      paste0(
        "\\authornote{",
        "\n",
        content,
        collapse = "",
        "}"
      )
    )
  }
}

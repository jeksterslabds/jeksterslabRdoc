#' Extract Author and Email from Asciidoctor Attributes for Asciidoctor Header.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams doc_attr
#' @param format Character string.
#'   \code{"adoc"}, Asciidoctor formating.
#'   \code{"latex"}, Latex formating.
#' @export
doc_attr_authorheader <- function(attr_from_file,
                                  attr_path,
                                  attr,
                                  format) {
  if (attr_from_file) {
    attr <- readLines(attr_path)
  }
  if (format == "adoc") {
    key <- c(
      "author",
      "email"
    )
    keys <- paste0(key, "_[1-9]")
    output <- vector(
      mode = "list",
      length = length(keys)
    )
    for (i in seq_along(keys)) {
      output[[i]] <- doc_attr(
        key = keys[i],
        attr_from_file = FALSE,
        attr = attr
      )
    }
    if (length(unique(lengths(output))) > 1
    ) {
      stop("Lengths of entries are not equal.")
    }
    names(output) <- key
    output <- do.call(
      what = rbind,
      args = output
    )
    output["email", ] <- paste0(
      "<",
      output["email", ],
      ">"
    )
    bind <- vector(
      mode = "list",
      length = ncol(output)
    )
    for (j in 1:ncol(output)) {
      bind[[j]] <- paste0(
        output[, j],
        collapse = " "
      )
    }
    return(
      paste0(
        bind,
        collapse = "; "
      )
    )
  }
  if (format == "latex") {
    key <- c(
      "latex_author_format",
      "latex_affiliation_format",
      "leftheader"
    )
    attributes <- doc_attr(
      key = key,
      attr_from_file = FALSE,
      attr = attr
    )
    names(attributes) <- key
    return(
      paste0(
        attributes["latex_author_format"],
        "\n",
        attributes["latex_affiliation_format"],
        "\n\n",
        "\\leftheader{",
        attributes["leftheader"],
        "}"
      )
    )
  }
}

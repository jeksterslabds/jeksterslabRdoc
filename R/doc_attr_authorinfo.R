#' Extract Author Information from Asciidoctor Attributes.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams doc_attr_authorheader
#' @export
doc_attr_authorinfo <- function(attr_from_file,
                                attr_path,
                                attr,
                                format) {
  if (attr_from_file) {
    attr <- readLines(attr_path)
  }
  key <- c(
    "author",
    "email",
    "orcid",
    "department",
    "affiliation"
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
  if (length(unique(lengths(output))) > 1) {
    stop("Lengths of entries are not equal.")
  }
  names(output) <- key
  output <- do.call(what = rbind, args = output)
  output["department", ] <- paste0(
    output["department", ],
    ","
  )
  if (format == "adoc") {
    email_png <- system.file(
      "extdata",
      "email.png",
      package = "jeksterslabRdoc",
      mustWork = TRUE
    )
    output["email", ] <- paste0(
      "image:",
      email_png,
      "[link=",
      "\"mailto:",
      output["email", ],
      "\"",
      ", width=25px, height=25px",
      "]"
    )
    orcid_png <- system.file(
      "extdata",
      "orcid.png",
      package = "jeksterslabRdoc",
      mustWork = TRUE
    )
    output["orcid", ] <- paste0(
      "image:",
      orcid_png,
      "[link=",
      "\"https://orcid.org/",
      output["orcid", ],
      "\"",
      ", width=20px, height=20px",
      "]"
    )
  }
  if (format == "latex") {
    author_orcid <- paste0(
      "\\addORCIDlink",
      "{",
      output["author", ],
      "}",
      "{",
      output["orcid", ],
      "}"
    )
    email <- paste0(
      "\\href{",
      "mailto:",
      output["email", ],
      "}",
      "{",
      output["email", ],
      "}"
    )
    department <- output["department", ]
    affiliation <- output["affiliation", ]
    output <- rbind(
      author_orcid,
      email,
      department,
      affiliation
    )
  }
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
  paste0(
    bind,
    collapse = "\n\n"
  )
}

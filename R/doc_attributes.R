#' Generate `Asciidoc` Attributes and `LaTeX` PDF Information
#'
#' Generates `Asciidoc` attributes
#' and `LaTeX` PDF information
#' from a `YAML` file.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @inheritParams doc_authorinfo
#' @importFrom utils packageVersion
#' @export
doc_attributes <- function(attributes_yml,
                           author_latex = NULL,
                           affiliation_latex = NULL,
                           add_authornote = NULL) {
  # source template attributes.yml if attributes_yml = NULL
  # if (is.null(attributes_yml)) {
  #  attributes_yml <- system.file(
  #    "extdata",
  #    "attributes.yml",
  #    package = "jeksterslabRdoc",
  #    mustWork = TRUE
  #  )
  # }
  # read in attributes_yml
  input <- original <- read_yaml(file = attributes_yml)
  # check required fields
  required <- c(
    "title",
    "subject",
    "abstract",
    "keywords",
    "creationdate"
  )
  for (i in seq_along(required)) {
    if (!required[i] %in% names(input)) {
      stop(
        paste0(
          "The required field ",
          required[i],
          " is NOT PRESENT in ",
          attributes_yml,
          "\n"
        )
      )
    }
  }
  ################################################################
  # authornote
  ################################################################
  authorinfo <- doc_authorinfo(
    attributes_yml = attributes_yml,
    author_latex = author_latex,
    affiliation_latex = affiliation_latex,
    add_authornote = add_authornote
  )
  author <- authorinfo$author
  authors <- authorinfo$authors
  lastnames <- authorinfo$lastnames
  emails <- authorinfo$emails
  orcids <- authorinfo$orcids
  departments <- authorinfo$departments
  affiliations <- authorinfo$affiliations
  input[["author"]] <- author
  for (i in seq_along(input[["authors"]])) {
    input[[paste0("author", "_", i)]] <- authors[i]
    input[[paste0("email", "_", i)]] <- emails[i]
    input[[paste0("orcid", "_", i)]] <- orcids[i]
    input[[paste0("department", "_", i)]] <- departments[i]
    input[[paste0("affiliation", "_", i)]] <- affiliations[i]
  }
  input[["authors"]] <- paste0(
    authors,
    collapse = ", "
  )
  ################################################################
  # keywords
  ################################################################
  keywords <- input[["keywords"]]
  input[["keywords"]] <- trimws(
    paste0(
      keywords,
      collapse = ", "
    )
  )
  ################################################################
  # abstract
  ################################################################
  abstract <- doc_abstract(
    abstract = input[["abstract"]],
    keywords = input[["keywords"]]
  )
  input[["abstract"]] <- abstract[["adoc"]][["text"]]
  ################################################################
  # creationdate
  ################################################################
  creationdate_orig <- as.POSIXlt(
    x = input[["creationdate"]]
  )
  creationdate <- strftime(
    x = creationdate_orig,
    format = "%Y-%m-%d %H:%M:%S"
  )
  creationdate_latex <- strftime(
    x = creationdate_orig,
    format = "%Y%m%d%H%M%S"
  )
  input[["creationdate"]] <- creationdate
  ################################################################
  # revdate
  ################################################################
  # Note that revdate in the input is ignored in favor of the current date
  revdate_orig <- Sys.time()
  revdate <- strftime(
    x = revdate_orig,
    format = "%Y-%m-%d"
  )
  revdate_latex <- strftime(
    x = revdate_orig,
    format = "%Y%m%d%H%M%S"
  )
  input[["revdate"]] <- revdate
  ################################################################
  # creator
  ################################################################
  input[["creator"]] <- "Ivan Jacob Agaloos Pesigan"
  ################################################################
  # producer
  ################################################################
  input[["producer"]] <- paste0(
    "jeksterslabRdoc",
    " (",
    packageVersion("jeksterslabRdoc"),
    ")"
  )
  ###################################################
  # bibliography latex
  ###################################################
  if (!is.null(input[["bibliography-database"]]) & !is.null(input[["bibliography-style"]])) {
    bibliography <- doc_bib_latex(
      bib_path = input[["bibliography-database"]],
      style = input[["bibliography-style"]]
    )
  } else {
    bibliography <- NA
  }
  ###################################################
  # attributes
  ###################################################
  attributes <- rep(
    x = NA,
    times = length(input)
  )
  input_names <- paste0(
    ":",
    names(input),
    ":"
  )
  for (i in seq_along(input)) {
    if (is.logical(input[[i]])) {
      if (input[[i]]) {
        attributes[i] <- input_names[i]
      } else {
        attributes[i] <- paste0(
          "!",
          input_names[i]
        )
      }
    } else {
      attributes[i] <- paste(
        input_names[i],
        input[[i]]
      )
    }
  }
  attributes <- sort(
    attributes[!is.na(attributes)]
  )
  attributes <- paste0(
    attributes,
    collapse = "\n"
  )
  # remove blank lines
  attributes_adoc <- gsub(
    pattern = "\n{2}",
    replacement = "\n",
    x = attributes
  )
  attributes_latex <- paste0(
    "\\ifpdf", "\n",
    "\\pdfinfo{", "\n",
    "   /Title  (", input[["title"]], ")", "\n",
    "   /Author (", input[["author"]], ")", "\n",
    "   /Creator (", input[["creator"]], ")", "\n",
    "   /Producer (", input[["producer"]], ")", "\n",
    "   /CreationDate (D:", creationdate_latex, ")", "\n",
    "   /ModDate (D:", revdate_latex, ")", "\n",
    "   /Subject (", input[["subject"]], ")", "\n",
    "   /Keywords (", input[["keywords"]], ")", "\n",
    "}", "\n",
    "\\fi"
  )
  attributes <- list(
    adoc = attributes_adoc,
    latex = attributes_latex
  )
  invisible(
    list(
      attributes = attributes,
      authorinfo = authorinfo,
      abstract = abstract,
      bibliography = bibliography,
      original = original,
      processed = input
    )
  )
}

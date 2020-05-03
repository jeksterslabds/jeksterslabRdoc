#' Generate `Asciidoc` Attributes
#'
#' Generates `Asciidoc` attributes from a `YAML` file.
#'
#' @author Ivan Jacob Agaloos Pesigan
#' @param attributes_yml Character string.
#'   Path to the attributes `YAML` file.
#' @param author_latex Character string.
#'   If specified,
#'   overides default `latex` `\author`.
#' @param affiliation_latex Character string.
#'   If specified,
#'   overides default `latex` `\affiliation`.
#' @param add_authornote Character string.
#'   Additional information to include in author note.
#' @importFrom yaml read_yaml
#' @export
doc_authorinfo <- function(attributes_yml = NULL,
                           author_latex = NULL,
                           affiliation_latex = NULL,
                           add_authornote = NULL) {
  # source template attributes.yml if attributes_yml = NULL
  if (is.null(attributes_yml)) {
    attributes_yml <- system.file(
      "extdata",
      "attributes.yml",
      package = "jeksterslabRdoc",
      mustWork = TRUE
    )
  }
  # read in attributes_yml
  input <- read_yaml(file = attributes_yml)
  # check required fields
  required <- c(
    "firstname",
    "lastname",
    "authors",
    "corresponding_author",
    "corresponding_email",
    "corresponding_address"
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
  # author
  ################################################################
  if (is.null(input[["middlename"]])) {
    input[["middlename"]] <- ""
  }
  author <- trimws(
    paste(
      input[["firstname"]],
      input[["middlename"]],
      input[["lastname"]]
    )
  )
  ################################################################
  # authors
  ################################################################
  authors <- rep(
    x = NA,
    times = length(input[["authors"]])
  )
  lastnames <- emails <- orcids <- departments <- affiliations <- authors_emails <- authorinfo_adoc <- authorinfo_latex <- authors
  email_png <- system.file(
    "extdata",
    "email.png",
    package = "jeksterslabRdoc",
    mustWork = TRUE
  )
  orcid_png <- system.file(
    "extdata",
    "orcid.png",
    package = "jeksterslabRdoc",
    mustWork = TRUE
  )
  for (i in seq_along(input[["authors"]])) {
    authors[i] <- trimws(
      paste(
        input[["authors"]][[i]][["firstname"]],
        input[["authors"]][[i]][["middlename"]],
        input[["authors"]][[i]][["lastname"]]
      )
    )
    lastnames[i] <- input[["authors"]][[i]][["lastname"]]
    emails[i] <- input[["authors"]][[i]][["email"]]
    orcids[i] <- input[["authors"]][[i]][["orcid"]]
    departments[i] <- input[["authors"]][[i]][["department"]]
    affiliations[i] <- input[["authors"]][[i]][["affiliation"]]
    input[[paste0("author", "_", i)]] <- authors[i]
    input[[paste0("email", "_", i)]] <- emails[i]
    input[[paste0("orcid", "_", i)]] <- orcids[i]
    input[[paste0("department", "_", i)]] <- departments[i]
    input[[paste0("affiliation", "_", i)]] <- affiliations[i]
    authors_emails[i] <- paste0(
      authors[i],
      " ",
      "<",
      emails[i],
      ">"
    )
    authorinfo_adoc[i] <- paste0(
      authors[i],
      "\n",
      "image:",
      email_png,
      "[link=",
      "\"mailto:",
      emails[i],
      "\"",
      ", width=25px, height=25px",
      "]",
      "\n",
      "image:",
      orcid_png,
      "[link=",
      "\"https://orcid.org/",
      orcids[i],
      "\"",
      ", width=20px, height=20px",
      "]",
      "\n",
      departments[i],
      ",\n",
      affiliations[i]
    )
    authorinfo_latex[i] <- paste0(
      "\\addORCIDlink",
      "{",
      authors[i],
      "}",
      "{",
      orcids[i],
      "}",
      "\n",
      "\\href{",
      "mailto:",
      emails[i],
      "}",
      "{",
      emails[i],
      "}",
      "\n",
      departments[i],
      ",\n",
      affiliations[i]
    )
  }
  ################################################################
  # authorinfo
  ################################################################
  # adoc
  authorinfo_adoc <- paste0(
    authorinfo_adoc,
    collapse = ";\n"
  )
  # latex
  authorinfo_latex <- paste0(
    authorinfo_latex,
    collapse = ";\n"
  )
  ################################################################
  # latex leftheader
  ################################################################
  if (length(lastnames) == 1) {
    leftheader <- lastnames[1]
  }
  if (length(lastnames) == 2) {
    leftheader <- paste0(
      lastnames,
      collapse = " & "
    )
  }
  if (length(lastnames) > 2) {
    leftheader <- paste(
      lastnames[1],
      "et al."
    )
  }
  leftheader <- paste0(
    "\\leftheader{",
    leftheader,
    "}"
  )
  ################################################################
  # latex author
  ################################################################
  if (is.null(author_latex)) {
    if (length(lastnames) == 1) {
      author_latex <- paste0(
        "\\author{",
        authors[1],
        "}"
      )
    }
    if (length(lastnames) == 2) {
      author_latex <- paste0(
        "\\twoauthors{",
        authors[1],
        "}{",
        authors[2],
        "}"
      )
    }
    if (length(lastnames) == 3) {
      author_latex <- paste0(
        "\\threeauthors{",
        authors[1],
        "}{",
        authors[2],
        "}{",
        authors[3],
        "}"
      )
    }
    if (length(lastnames) == 4) {
      author_latex <- paste0(
        "\\fourauthors{",
        authors[1],
        "}{",
        authors[2],
        "}{",
        authors[3],
        "}{",
        authors[4],
        "}"
      )
    }
    if (length(lastnames) == 5) {
      author_latex <- paste0(
        "\\fiveauthors{",
        authors[1],
        "}{",
        authors[2],
        "}{",
        authors[3],
        "}{",
        authors[4],
        "}{",
        authors[5],
        "}"
      )
    }
    if (length(lastnames) == 6) {
      author_latex <- paste0(
        "\\sixauthors{",
        authors[1],
        "}{",
        authors[2],
        "}{",
        authors[3],
        "}{",
        authors[4],
        "}{",
        authors[5],
        "}{",
        authors[6],
        "}"
      )
    }
  }
  ################################################################
  # latex affiliation
  ################################################################
  if (is.null(affiliation_latex)) {
    if (length(lastnames) == 1) {
      affiliation_latex <- paste0(
        "\\affiliation{",
        affiliations[1],
        "}"
      )
    }
    if (length(lastnames) == 2) {
      affiliation_latex <- paste0(
        "\\twoaffiliations{",
        affiliations[1],
        "}{",
        affiliations[2],
        "}"
      )
    }
    if (length(lastnames) == 3) {
      affiliation_latex <- paste0(
        "\\threeaffiliations{",
        affiliations[1],
        "}{",
        affiliations[2],
        "}{",
        affiliations[3],
        "}"
      )
    }
    if (length(lastnames) == 4) {
      affiliation_latex <- paste0(
        "\\fouraffiliations{",
        affiliations[1],
        "}{",
        affiliations[2],
        "}{",
        affiliations[3],
        "}{",
        affiliations[4],
        "}"
      )
    }
    if (length(lastnames) == 5) {
      affiliation_latex <- paste0(
        "\\fiveaffiliations{",
        affiliations[1],
        "}{",
        affiliations[2],
        "}{",
        affiliations[3],
        "}{",
        affiliations[4],
        "}{",
        affiliations[5],
        "}"
      )
    }
    if (length(lastnames) == 6) {
      affiliation_latex <- paste0(
        "\\sixaffiliations{",
        affiliations[1],
        "}{",
        affiliations[2],
        "}{",
        affiliations[3],
        "}{",
        affiliations[4],
        "}{",
        affiliations[5],
        "}{",
        affiliations[6],
        "}"
      )
    }
  }
  ################################################################
  # correspondence
  ################################################################
  correspondence <- paste0(
    "Correspondence concerning this article should be addressed to",
    "\n",
    input[["corresponding_author"]],
    ",",
    "\n",
    trimws(input[["corresponding_address"]]),
    ".",
    "\n",
    "Email: "
  )
  correspondence_adoc <- paste0(
    correspondence,
    "mailto:",
    input[["corresponding_email"]],
    "[",
    input[["corresponding_email"]],
    "]"
  )
  correspondence_latex <- paste0(
    correspondence,
    "\\href{",
    "mailto:",
    input[["corresponding_email"]],
    "}",
    "{",
    input[["corresponding_email"]],
    "}"
  )
  ################################################################
  # authornote
  ################################################################
  if (is.null(add_authornote)) {
    add_authornote <- "\n\n"
  } else {
    add_authornote <- paste0(
      "\n\n",
      add_authornote,
      "\n\n"
    )
  }
  # adoc
  authornote_adoc <- paste0(
    "== Author Note",
    "\n\n",
    authorinfo_adoc,
    add_authornote,
    correspondence_adoc
  )
  # latex
  authornote_latex <- paste0(
    "\\authornote{",
    "\n",
    authorinfo_latex,
    add_authornote,
    correspondence_latex,
    "\n",
    "}"
  )
  authornote <- list(
    adoc = authornote_adoc,
    latex = authornote_latex
  )
  ################################################################
  # output
  ################################################################
  list(
    authornote = authornote,
    author = author,
    authors = authors,
    lastnames = lastnames,
    emails = emails,
    orcids = orcids,
    departments = departments,
    affiliations = affiliations,
    author_latex = author_latex,
    affiliation_latex = affiliation_latex,
    leftheader = leftheader
  )
}

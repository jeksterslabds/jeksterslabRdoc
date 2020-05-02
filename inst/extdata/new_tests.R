library(jeksterslabRdoc)
attributes_yml <- system.file(
  "extdata",
  "attributes.yml",
  package = "jeksterslabRdoc",
  mustWork = TRUE
)
jdoc <- system.file(
  "extdata",
  "jdoc.jdoc",
  package = "jeksterslabRdoc",
  mustWork = TRUE
)
bib_path <- file.path(
  "/media",
  "jeksterslab",
  "scripts",
  "r",
  "jeksterslabRdoc",
  "inst",
  "extdata",
  "bib.bib"
)




attributes <- doc_attributes(
  attributes_yml = attributes_yml,
  author_latex = NULL,
  affiliation_latex = NULL,
  add_authornote = NULL
)
title_adoc <- attributes[["processed"]][["title"]]
authornote_adoc <- attributes[["authorinfo"]][["authornote"]][["adoc"]]
attributes_adoc <- attributes[["attributes"]][["adoc"]]
doc_knit_adoc(
  input = jdoc,
  output = "/media/jeksterslab/scripts/r/jeksterslabRdoc/inst/extdata/jdoc.knit.adoc"
)
jdoc_body <- doc_retag_adoc(
  from_file = TRUE,
  path = "/media/jeksterslab/scripts/r/jeksterslabRdoc/inst/extdata/jdoc.knit.adoc"
)
jdoc_body <- doc_citation_adoc(
  from_file = FALSE,
  jdoc = jdoc_body
)
output <- paste0(
  "= ",
  title_adoc,
  "\n",
  attributes_adoc,
  "\n\n",
  authornote_adoc,
  "\n\n",
  attributes[["abstract"]][["adoc"]][["block"]],
  "\n\n",
  paste0(
    jdoc_body,
    collapse = "\n"
  )
)
output <- gsub(
  pattern = "\n{3,}",
  replacement = "\n\n",
  x = output
)
writeLines(
  text = output,
  con = "/media/jeksterslab/scripts/r/jeksterslabRdoc/inst/extdata/jdoc.adoc"
)
system(
  paste(
    "asciidoctor -r asciidoctor-bibliography",
    shQuote(
      "/media/jeksterslab/scripts/r/jeksterslabRdoc/inst/extdata/jdoc.adoc"
    )
  )
)


library(jeksterslabRdoc)
attributes_yml <- system.file(
  "extdata",
  "attributes.yml",
  package = "jeksterslabRdoc",
  mustWork = TRUE
)
jdoc <- system.file(
  "extdata",
  "jdoc.jdoc",
  package = "jeksterslabRdoc",
  mustWork = TRUE
)
doc_manuscript(
  jdoc = jdoc,
  attributes_yml = attributes_yml
)

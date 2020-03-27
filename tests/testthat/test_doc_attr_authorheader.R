#' ---
#' title: "Test doc_attr_authorheader"
#' author: "Ivan Jacob Agaloos Pesigan"
#' date: "`r Sys.Date()`"
#' output:
#'   rmarkdown::html_vignette:
#'     toc: true
#' ---

#+ setup
library(testthat)
library(jeksterslabRdoc)

#+ parameter
adoc <- "Ivan Jacob Agaloos Pesigan <i.j.a.pesigan@connect.um.edu.mo>; John Doe <johndoe@email.com>"
latex <- "\\author{Ivan Jacob Agaloos Pesigan and John Doe}\n\\affiliation{University of Macau}\n\n\\leftheader{Pesigan & Doe}"

#+ test
attr_path <- system.file(
  "extdata",
  "attributes",
  package = "jeksterslabRdoc",
  mustWork = TRUE
)
adoc_test <- doc_attr_authorheader(
  attr_from_file = TRUE,
  attr_path = attr_path,
  format = "adoc"
)
latex_test <- doc_attr_authorheader(
  attr_from_file = TRUE,
  attr_path = attr_path,
  format = "latex"
)

#+ testhat, echo=FALSE
test_that("adoc is accurate.", {
  expect_equivalent(
    adoc_test,
    adoc
  )
})
test_that("latex is accurate.", {
  expect_equivalent(
    latex_test,
    latex
  )
})

#' ---
#' title: "Test doc_attr"
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
description <- "Document description"
keywords <- "Jekdoc, Asciidoc, Latex"
title <- "Sample Asciidoctor/LaTeX Document Using Jekdoc Tags"
shorttitle <- "Jekdoc"
leftheader <- "Pesigan & Doe"
author_1 <- "Ivan Jacob Agaloos Pesigan"
email_1 <- "i.j.a.pesigan@connect.um.edu.mo"
orcid_1 <- "0000-0003-4818-8420"
department_1 <- "Department of Psychology"
affiliation_1 <- "University of Macau"
author_2 <- "John Doe"
email_2 <- "johndoe@email.com"
department_2 <- "Department of Psychology"
affiliation_2 <- "University of Macau"
orcid_2 <- "0000-0000-0000-0000"
corresponding_author <- "Ivan Jacob Agaloos Pesigan"
corresponding_address <- "Department of Psychology, Faculty of Social Sciences, Avenida da Universidade, University of Macau, Taipa, Macau SAR, China"
corresponding_email <- "i.j.a.pesigan@connect.um.edu.mo"
latex_author_format <- "\\author{Ivan Jacob Agaloos Pesigan and John Doe}"
latex_affiliation_format <- "\\affiliation{University of Macau}"
icons <- "font"
doctype <- "article"
stem <- "latexmath"
toc <- "left"
toclevels <- "5"

keys <- c(
  "description",
  "keywords",
  "title",
  "shorttitle",
  "leftheader",
  "author_1",
  "email_1",
  "orcid_1",
  "department_1",
  "affiliation_1",
  "author_2",
  "email_2",
  "department_2",
  "affiliation_2",
  "orcid_2",
  "corresponding_author",
  "corresponding_address",
  "corresponding_email",
  "latex_author_format",
  "latex_affiliation_format",
  "icons",
  "doctype",
  "stem",
  "toc",
  "toclevels"
)

#+ test
attr_path <- system.file(
  "extdata",
  "attributes",
  package = "jeksterslabRdoc",
  mustWork = TRUE
)
attr <- doc_attr(
  key = keys,
  attr_from_file = TRUE,
  attr_path = attr_path
)
names(attr) <- keys

#+ testhat, echo=FALSE
test_that("description is accurate.", {
  expect_equivalent(
    attr["description"],
    description
  )
})
test_that("keywords is accurate.", {
  expect_equivalent(
    attr["keywords"],
    keywords
  )
})
test_that("title is accurate.", {
  expect_equivalent(
    attr["title"],
    title
  )
})
test_that("shorttitle is accurate.", {
  expect_equivalent(
    attr["shorttitle"],
    shorttitle
  )
})
test_that("leftheader is accurate.", {
  expect_equivalent(
    attr["leftheader"],
    leftheader
  )
})
test_that("author_1 is accurate.", {
  expect_equivalent(
    attr["author_1"],
    author_1
  )
})
test_that("email_1 is accurate.", {
  expect_equivalent(
    attr["email_1"],
    email_1
  )
})
test_that("orcid_1 is accurate.", {
  expect_equivalent(
    attr["orcid_1"],
    orcid_1
  )
})
test_that("department_1 is accurate.", {
  expect_equivalent(
    attr["department_1"],
    department_1
  )
})
test_that("affiliation_1 is accurate.", {
  expect_equivalent(
    attr["affiliation_1"],
    affiliation_1
  )
})
test_that("author_2 is accurate.", {
  expect_equivalent(
    attr["author_2"],
    author_2
  )
})
test_that("email_2 is accurate.", {
  expect_equivalent(
    attr["email_2"],
    email_2
  )
})
test_that("department_2 is accurate.", {
  expect_equivalent(
    attr["department_2"],
    department_2
  )
})
test_that("affiliation_2 is accurate.", {
  expect_equivalent(
    attr["affiliation_2"],
    affiliation_2
  )
})
test_that("orcid_2 is accurate.", {
  expect_equivalent(
    attr["orcid_2"],
    orcid_2
  )
})
test_that("corresponding_author is accurate.", {
  expect_equivalent(
    attr["corresponding_author"],
    corresponding_author
  )
})
test_that("corresponding_address is accurate.", {
  expect_equivalent(
    attr["corresponding_address"],
    corresponding_address
  )
})
test_that("corresponding_email is accurate.", {
  expect_equivalent(
    attr["corresponding_email"],
    corresponding_email
  )
})
test_that("latex_author_format is accurate.", {
  expect_equivalent(
    attr["latex_author_format"],
    latex_author_format
  )
})
test_that("latex_affiliation_format is accurate.", {
  expect_equivalent(
    attr["latex_affiliation_format"],
    latex_affiliation_format
  )
})
test_that("icons is accurate.", {
  expect_equivalent(
    attr["icons"],
    icons
  )
})
test_that("doctype is accurate.", {
  expect_equivalent(
    attr["doctype"],
    doctype
  )
})
test_that("stem is accurate.", {
  expect_equivalent(
    attr["stem"],
    stem
  )
})
test_that("toc is accurate.", {
  expect_equivalent(
    attr["toc"],
    toc
  )
})
test_that("toclevels is accurate.", {
  expect_equivalent(
    attr["toclevels"],
    toclevels
  )
})

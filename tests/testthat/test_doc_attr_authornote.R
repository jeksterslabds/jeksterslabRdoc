#' ---
#' title: "Test doc_attr_authornote"
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

adoc <- "== Author Note\n\nIvan Jacob Agaloos Pesigan image:/home/jek/R/x86_64-pc-linux-gnu-library/3.6/jeksterslabRdoc/extdata/email.png[link=\"mailto:i.j.a.pesigan@connect.um.edu.mo\", width=25px, height=25px] image:/home/jek/R/x86_64-pc-linux-gnu-library/3.6/jeksterslabRdoc/extdata/orcid.png[link=\"https://orcid.org/0000-0003-4818-8420\", width=20px, height=20px] Department of Psychology, University of Macau\n\nJohn Doe image:/home/jek/R/x86_64-pc-linux-gnu-library/3.6/jeksterslabRdoc/extdata/email.png[link=\"mailto:johndoe@email.com\", width=25px, height=25px] image:/home/jek/R/x86_64-pc-linux-gnu-library/3.6/jeksterslabRdoc/extdata/orcid.png[link=\"https://orcid.org/0000-0000-0000-0000\", width=20px, height=20px] Department of Psychology, University of Macau\n\nWe have no known conflict of interest to disclose.\n\nCorrespondence concerning this article should be addressed to Ivan Jacob Agaloos Pesigan, Department of Psychology, Faculty of Social Sciences, Avenida da Universidade, University of Macau, Taipa, Macau SAR, China. E-mail: i.j.a.pesigan@connect.um.edu.mo."

latex <- "\\authornote{\n\\addORCIDlink{Ivan Jacob Agaloos Pesigan}{0000-0003-4818-8420} \\href{mailto:i.j.a.pesigan@connect.um.edu.mo}{i.j.a.pesigan@connect.um.edu.mo} Department of Psychology, University of Macau\n\n\\addORCIDlink{John Doe}{0000-0000-0000-0000} \\href{mailto:johndoe@email.com}{johndoe@email.com} Department of Psychology, University of Macau\n\nWe have no known conflict of interest to disclose.\n\nCorrespondence concerning this article should be addressed to Ivan Jacob Agaloos Pesigan, Department of Psychology, Faculty of Social Sciences, Avenida da Universidade, University of Macau, Taipa, Macau SAR, China. E-mail: i.j.a.pesigan@connect.um.edu.mo.}"

#+ test
attr_path <- system.file(
  "extdata",
  "attributes",
  package = "jeksterslabRdoc",
  mustWork = TRUE
)
adoc_test <- doc_attr_authornote(
  attr_from_file = TRUE,
  attr_path = attr_path,
  format = "adoc"
)
latex_test <- doc_attr_authornote(
  attr_from_file = TRUE,
  attr_path = attr_path,
  format = "latex"
)

#+ testhat, echo=FALSE
# Testing uses the temporary folder.
# Paths for the package are on tmp
# which means that paths for the ORCID and email icons vary.
# test_that("adoc is accurate.", {
#  expect_equivalent(
#    adoc_test,
#    adoc
#  )
# })
test_that("latex is accurate.", {
  expect_equivalent(
    latex_test,
    latex
  )
})

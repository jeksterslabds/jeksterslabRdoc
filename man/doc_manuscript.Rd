% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doc_manuscript.R
\name{doc_manuscript}
\alias{doc_manuscript}
\title{Generate Manuscript}
\usage{
doc_manuscript(
  jdoc,
  attributes_yml,
  output_dir = getwd(),
  author_latex = NULL,
  affiliation_latex = NULL,
  add_authornote = NULL,
  mode = "man",
  paper = "a4paper",
  packages = NULL,
  etc = NULL
)
}
\arguments{
\item{jdoc}{Character string.
Jekdoc input file.}

\item{attributes_yml}{Character string.
Path to the attributes \code{YAML} file.}

\item{output_dir}{Character string.
Output directory.}

\item{author_latex}{Character string.
If specified,
overides default \code{latex} \verb{\\author}.}

\item{affiliation_latex}{Character string.
If specified,
overides default \code{latex} \verb{\\affiliation}.}

\item{add_authornote}{Character string.
Additional information to include in author note.}

\item{mode}{Character string.
APA7 LaTeX mode.
The options are
\code{"jou"} for journal,
\code{"man"} for manuscript, and
\code{"stu"} for student paper.}

\item{paper}{Character string.
Paper size.
The options are
\code{"letter"} and
\code{"a4paper"}.}

\item{packages}{Character vector.
Additional LaTeX packages to include in the preamble.}

\item{etc}{Character vector.
Extra text to include in the preamble.}
}
\description{
Generate Manuscript
}
\author{
Ivan Jacob Agaloos Pesigan
}

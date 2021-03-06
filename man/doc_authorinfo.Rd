% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doc_authorinfo.R
\name{doc_authorinfo}
\alias{doc_authorinfo}
\title{Generate \code{Asciidoc} Attributes}
\usage{
doc_authorinfo(
  attributes_yml = NULL,
  author_latex = NULL,
  affiliation_latex = NULL,
  add_authornote = NULL
)
}
\arguments{
\item{attributes_yml}{Character string.
Path to the attributes \code{YAML} file.}

\item{author_latex}{Character string.
If specified,
overides default \code{latex} \verb{\\author}.}

\item{affiliation_latex}{Character string.
If specified,
overides default \code{latex} \verb{\\affiliation}.}

\item{add_authornote}{Character string.
Additional information to include in author note.}
}
\description{
Generates \code{Asciidoc} attributes from a \code{YAML} file.
}
\author{
Ivan Jacob Agaloos Pesigan
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corpus_tidiers.R
\name{tidy.Corpus}
\alias{tidy.Corpus}
\title{Tidy a Corpus object from the tm package}
\usage{
\method{tidy}{Corpus}(x, collapse = "\\n", ...)
}
\arguments{
\item{x}{A Corpus object, such as a VCorpus or PCorpus}

\item{collapse}{A string that should be used to
collapse text within each corpus (if a document has multiple lines).
Give NULL to not collapse strings, in which case a corpus
will end up as a list column if there are multi-line documents.}

\item{...}{Extra arguments, not used}
}
\description{
Tidy a Corpus object from the tm package. Returns a data frame
with one-row-per-document, with a \code{text} column containing
the document's text, and one column for each local (per-document)
metadata tag. For corpus objects from the quanteda package,
see \code{\link[=tidy.corpus]{tidy.corpus()}}.
}
\examples{

library(dplyr)   # displaying tbl_dfs

if (requireNamespace("tm", quietly = TRUE)) {
  library(tm)
  #' # tm package examples
  txt <- system.file("texts", "txt", package = "tm")
  ovid <- VCorpus(DirSource(txt, encoding = "UTF-8"),
                  readerControl = list(language = "lat"))

  ovid
  tidy(ovid)

  # choose different options for collapsing text within each
  # document
  tidy(ovid, collapse = "")$text
  tidy(ovid, collapse = NULL)$text

  # another example from Reuters articles
  reut21578 <- system.file("texts", "crude", package = "tm")
  reuters <- VCorpus(DirSource(reut21578),
                     readerControl = list(reader = readReut21578XMLasPlain))
  reuters

  tidy(reuters)
}

}

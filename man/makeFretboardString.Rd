% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeFretboardString.R
\name{makeFretboardString}
\alias{makeFretboardString}
\title{String Representation of a Chord Shape}
\usage{
makeFretboardString(notes, shape.column = "scalenum", trim = FALSE)
}
\arguments{
\item{notes}{A fretNotes object containing the fretboard positions of a chord shape
(strings and fret numbers of a set of notes).}

\item{shape.column}{Character string values specifying a column of the notes argument to
use.}

\item{trim}{Logical, should something be trimmed?}
}
\value{
Returns a named vector, with attribute "fret.range".
}
\description{
Convert a set of notes on a fretboard to a string representation.
}
\author{
M.W.Rowe, \email{mwr.stats@gmail.com}
}

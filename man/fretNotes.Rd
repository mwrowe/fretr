% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fretNotes.R
\name{fretNotes}
\alias{fretNotes}
\title{Assign Scale Notes To Positions of a Fretboard.}
\usage{
fretNotes(
  key = "E",
  scale = "major",
  n.frets = 22,
  tuning = c("E", "A", "D", "G", "B", "E")
)
}
\arguments{
\item{key}{Character value specifying tonic of the scale.}

\item{scale}{Character value specifying scale type; currently "major" or "minor".}

\item{n.frets}{Integer; number of frets on the fretboard (not counting the nut).}

\item{tuning}{Character vector of length 4 to 8, specifing number and tuning of
strings from lowest to highest by note; default is standard guitar,
c("E","A","D","G","B","E").}
}
\value{
Returns a fretNotes object, a data.frame with one row per fretboard
   position (fret/string combination), and named columns:
   \itemize{
      \item \strong{string}: Integer, 1 to number of strings, high to low.
      \item \strong{fret}: Integer, fret number, where 0 is the nut.
      \item \strong{at}: Number, distance from the nut in inches.
      \item \strong{notenum}: Integer, note number within the scale.
      \item \strong{note}: Character value, note name.
      \item \strong{semitone}: Integer value, interval (in semitones)
         relative to tonic.
   }
   The object will be assigned attributes:
   \itemize{
      \item \strong{key}: character value; tonic of the scale.
      \item \strong{scale}: character value; scale type.
      \item \strong{scalenotes}:
      \item \strong{fretrange}:
   }
}
\description{
Assign scale notes to positions of a fretboard given instrument parameters
(number of strings, frets and tuning) and major or minor key.
}
\author{
M.W.Rowe, \email{mwr.stats@gmail.com}
}

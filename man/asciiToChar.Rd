% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internal.R
\name{asciiToChar}
\alias{asciiToChar}
\title{asciiToChar: conversion between letters and ASCII codes}
\usage{
asciiToChar(ascii.codes)
}
\arguments{
\item{ascii.codes}{An integer vector specifying individual characters by ASCII code, with
values in the range 1:255 OR a vector of individual characters}
}
\value{
Returns a character vector where each element is a single character
  specified by the corresponding ASCII code in ascii.codes OR a numeric
  vector of ASCII codes corresponding to the characters in ascii.codes
}
\description{
asciiToChar converts ASCII codes to vector of single characters and vice
versa
}
\details{
\itemize{
   \item Useful for generating character sequences
   \item Missing values (NA) are ignored: asciiToChar(NA) -> NA
   \item This may be slow when converting characters to codes(?)
  }
}
\author{
M.W.Rowe, \email{mwr.stats@gmail.com}
}

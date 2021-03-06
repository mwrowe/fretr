% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internal.R
\name{titleCaps}
\alias{titleCaps}
\title{Title Capitalization}
\usage{
titleCaps(x, lower.too = TRUE, except = c())
}
\arguments{
\item{x}{A vector of character strings containing words separated by spaces.}

\item{lower.too}{Logical; if TRUE, force all letters except the first in each word to be
lower case.  Otherwise, only make the first letter in uppercase.}

\item{except}{Character vector of substrings where the capitalization should not be
altered, such as acronyms.  Pattern matching for this string is
case-insensitive; its capitalization will be exactly as specified.}
}
\value{
Returns a vector of character strings the same length as x, converted to
   title capitalization, i.e., each word capitalized.  Leading and trailing
   spaces will also be removed, and multiple adjacent spaces replaced with
   a single space.
}
\description{
titleCaps() capitalizes the first character of each word in character
strings.
}
\author{
M.W.Rowe, \email{mwr.stats@gmail.com}
}

#===============================================================================
# UTILITY FUNCTIONS FOR INTERNAL USE ONLY (DO NOT EXPORT)
#-------------------------------------------------------------------------------
# asciiToChar()
# as.char()
# is.char()
# sym()
# numberReplicates()
# titleCaps()
#===============================================================================
#' asciiToChar: conversion between letters and ASCII codes
#'
#' asciiToChar converts ASCII codes to vector of single characters and vice
#' versa
#'
#' @param ascii.codes
#'        An integer vector specifying individual characters by ASCII code, with
#'        values in the range 1:255 OR a vector of individual characters
#' @return
#'   Returns a character vector where each element is a single character
#'   specified by the corresponding ASCII code in ascii.codes OR a numeric
#'   vector of ASCII codes corresponding to the characters in ascii.codes
#'
#' @details
#'  \itemize{
#'    \item Useful for generating character sequences
#'    \item Missing values (NA) are ignored: asciiToChar(NA) -> NA
#'    \item This may be slow when converting characters to codes(?)
#'   }
#'
#' @author M.W.Rowe, \email{mike.rowe@gmail.com}
asciiToChar <-
function(ascii.codes){
   if(is.numeric(ascii.codes)){
      ascii.codes[ascii.codes<1 | ascii.codes>255] <- NA
      chars <- rep(NA,length(ascii.codes))
      chars[!is.na(ascii.codes)] <- unlist(strsplit(
         rawToChar(as.raw(ascii.codes[!is.na(ascii.codes)])),split=""))
   }else if(is.character(ascii.codes)){
      if(any(nchar(ascii.codes))>1)
         stop("ascii.codes may only contain single characters")
      chars <- rep(NA,length(ascii.codes))
      for(Ci in which(!is.na(ascii.codes))){
         chars[Ci] <- as.numeric(charToRaw(ascii.codes[Ci]))
      }
   }else if(all(is.na(ascii.codes))){
      char <- rep(NA,length(ascii.codes))
   }else stop("ascii.codes must be integers or single characters.")
   return(chars)
}
#-------------------------------------------------------------------------------
#' as.char: An Alias For as.character()
#'
#'as.char() is an alias for \code{\link{as.character}}().
#'
#' @inheritParams base::as.character
#'
#' @author M.W.Rowe, \email{mike.rowe@gmail.com}
as.char <- function(x, ...) return(as.character(x, ...))
#-------------------------------------------------------------------------------
#' is.char: An Alias For is.character()
#'
#' is.char() is an alias for \code{\link{is.character}}().
#'
#' @inheritParams base::is.character
#'
#' @author M.W.Rowe, \email{mike.rowe@gmail.com}
is.char <- function(x) return(is.character(x))
#-------------------------------------------------------------------------------
#' sym: Get A Numeric Range That Is Symmetric Around Zero
#'
#' sym returns a symmetric numeric interval, centered about zero.
#'
#' This function is useful for setting symmetric limits on graphs (among other
#' things.)
#'
#' @param lims
#'        A numeric value or vector.  If its length is greater than one, the
#'        quantile specified by the qtile argument will be calculated for
#'        the absolute values of lims.
#' @param qtile
#'        A numeric value in the range 0-1 (inclusive).  If lims contains
#'        multiple values, this quantile of the distributions of abs(lims)
#'        will be used to define the interval.  The default value of 1 will
#'        return max(abs(lims)).
#'
#' @return
#'    Returns a symmetric interval about zero, from -lims to lims, or if lims
#'    contains more than one value, a symmetric interval about zero that
#'    contains the fraction of the data specified by qtile.
#'
#' @author M.W.Rowe, \email{mike.rowe@gmail.com}
sym <-
   function(lims=1, qtile=1){
      lims <- abs(lims)
      if(length(lims)>1){
         lims <- stats::quantile(lims[TRUE], qtile, na.rm=T)
      }
      c(-1,1)*lims
   }
#-------------------------------------------------------------------------------
#' numberReplicates: Number Recurring Elements
#'
#' numberReplicates numbers instances of each non-unique value in the order
#' found
#' numberReplicates
#'
#' @param x
#'        a vector that includes non-unique elements
#' @param number.NAs
#'        logical; if TRUE, missing values will be numbered; otherwise,
#'        return an NA for each missing value
#'
#' @return
#'    returns a numeric vector where replicates of each unique value in x are
#'    numbered from 1 to the number of replicates found in the order in which
#'    they are found in x.  The replicate numbers returned correspond to the
#'    elements of x as it was passed in.
#'
#' @author M.W.Rowe, \email{mike.rowe@gmail.com}
numberReplicates <-
   # CREATED: 18SEP2014
   function(x, number.NAs=FALSE){
      # keep track of original order; sort x
      x <- data.frame(x=x[T],order=1:length(x),repnum=NA)
      x <- x[order(x$x),]
      # assign replicate numbers for each unique value of x
      not.na <- which(!is.na(x$x))
      if(length(not.na)>0){
         x[not.na,"repnum"] <-
            unlist(tapply(rep(1,length(not.na)),x[not.na,"x"],cumsum))
      }
      # deal with missing values
      na.ndxs <- which(is.na(x$x))
      if(number.NAs & length(na.ndxs)>0){
         x[na.ndxs,"repnum"] <- 1:length(na.ndxs)
      }
      # go back ot the original order of x
      x <- x[order(x$order),]
      x$repnum
   }
#-------------------------------------------------------------------------------
titleCaps <-
#' Title Capitalization
#'
#' titleCaps() capitalizes the first character of each word in character
#' strings.
#
#' @param x
#'    A vector of character strings containing words separated by spaces.
#' @param lower.too
#'    Logical; if TRUE, force all letters except the first in each word to be
#'    lower case.  Otherwise, only make the first letter in uppercase.
#' @param except
#'    Character vector of substrings where the capitalization should not be
#'    altered, such as acronyms.  Pattern matching for this string is
#'    case-insensitive; its capitalization will be exactly as specified.
#
#' @return
#'    Returns a vector of character strings the same length as x, converted to
#'    title capitalization, i.e., each word capitalized.  Leading and trailing
#'    spaces will also be removed, and multiple adjacent spaces replaced with
#'    a single space.
#'
#' @author M.W.Rowe, \email{mike.rowe@gmail.com}
#'
function(x,lower.too=TRUE,except=c()){
   y <- x
   if(lower.too) x <- tolower(x)
   for(ndx in which(nchar(x)>1)){
      # replace multiple spaces with one; strip leading and trailing spaces
      x[ndx] <- gsub("(^ | $)","",gsub(" +"," ",x[ndx]))
      z <- unlist(strsplit(x[ndx],""))
      upper <- c(1,which(z==" ")+1)
      z[upper] <- toupper(z[upper])
      y[ndx] <- paste0(z,collapse="")
   }
   for(ndx in except){
      y <- sub(ndx,ndx,y,ignore.case=TRUE)
   }
   y
}


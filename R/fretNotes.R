#' Assign Scale Notes To Positions of a Fretboard.
#'
#' Assign scale notes to positions of a fretboard given instrument parameters
#' (number of strings, frets and tuning) and major or minor key.
#
#' @param n.frets
#'    Integer; number of frets on the fretboard (not counting the nut).
#' @param key
#'    Character value specifying tonic of the scale.
#' @param scale
#'    Character value specifying scale type; currently "major" or "minor".
#' @param tuning
#'    Character vector of length 4 to 8, specifing number and tuning of
#'    strings from lowest to highest by note; default is standard guitar,
#'    c("E","A","D","G","B","E").
#
#' @return
#'    Returns a fretNotes object, a data.frame with one row per fretboard
#'    position (fret/string combination), and named columns:
#'    \itemize{
#'       \item \strong{string}: Integer, 1 to number of strings, high to low.
#'       \item \strong{fret}: Integer, fret number, where 0 is the nut.
#'       \item \strong{at}: Number, distance from the nut in inches.
#'       \item \strong{notenum}: Integer, note number within the scale.
#'       \item \strong{note}: Character value, note name.
#'       \item \strong{semitone}: Integer value, interval (in semitones)
#'          relative to tonic.
#'    }
#'    The object will be assigned attributes:
#'    \itemize{
#'       \item \strong{key}: character value; tonic of the scale.
#'       \item \strong{scale}: character value; scale type.
#'       \item \strong{scalenotes}:
#'       \item \strong{fretrange}:
#'    }
#'
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#'
#' @export
#'
fretNotes <-
function(key="E", scale="major", n.frets=22, tuning=c("E","A","D","G","B","E")){
   # define notes, scales
   notes <- c(E=0, F=1, "F#"=2, Gb=2, G=3, "G#"=4, Ab=4, A=5, "A#"=6, Bb=6, B=7,
              C=8, "C#"=9, Db=9, D=10, "D#"=11, Eb=11)
   scales <- list(
      major=c(0,2,4,5,7,9,11),
      minor=c(0,2,3,5,7,8,10),
      penta.maj=c(0,2,4,7,9),
      penta.min=c(0,3,5,7,10),
      lydian=    c(0,2,4,6,7,9,11),   # major with sharp 4th (tritone)
      ionian=    c(0,2,4,5,7,9,11),   # major
      mixolydian=c(0,2,4,5,7,9,10),   # pentatonic major + minor - (maj7 + b3)
      dorian=    c(0,2,3,5,7,9,10),   # pentatonic major + minor - (maj7 + 3)
      aeolian=   c(0,2,3,5,7,8,10),   # minor: flatted 3rd, 6th and 7th
      phrygian=  c(0,1,3,5,7,8,10),   # minor with flatted 2nd
      locrian=   c(0,1,3,5,6,8,10)    # minor flatted 2nd and 5th (tritone)
   )
   # check arguments
   if(!key%in%names(notes)) stop("invalid tonic argument")
   if(!scale%in%names(scales)) stop("invalid scale argument")
   n.strings <- length(tuning)
   if(!all(tuning%in%names(notes)) | n.strings<4 | n.strings>8){
      stop("invalid tuning argument")
   }
   tuning <- rev(tuning)  # reorder by string number
   # assign notes to each fret based on tuning
   frets <- data.frame(
      string=rep(1:n.strings, each=n.frets+1),
      fret=0:n.frets,
      at=NA)
   frets$notenum <- (frets$fret + notes[tuning[frets$string]])%%12
   # find the notes in the current scale and figure out how to label them
   tonicnum <- notes[key]
   scalenotes <- (tonicnum + scales[[scale]])%%12
   scalelabels <- rev(names(notes))[match(scalenotes,rev(notes))]
   if(any(table(substr(scalelabels,1,1))>1)){
      scalelabels <- names(notes)[match(scalenotes,notes)]
   }
   if(any(grepl("b", scalelabels))){
      notes <- notes[grep("#", names(notes), value=T, invert=T)]
   }else{
      notes <- notes[grep("b", names(notes), value=T, invert=T)]
   }
   frets$note <- names(notes)[match(frets$notenum, notes)]
   frets$semitone <- (frets$notenum - tonicnum)%%12
   names(scalenotes) <- names(notes)[match(scalenotes,notes)]
   frets$scalenum <- match(frets$note, names(scalenotes))
   attr(frets, "key") <- key
   attr(frets, "scale") <- scale
   attr(frets, "scalenotes") <- names(scalenotes)
   attr(frets, "fret.range") <- c(0, n.frets)
   class(frets) <- c("fretNotes", "data.frame")
   invisible(frets)
}

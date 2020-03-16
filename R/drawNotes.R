#' Draw Notes on the Fretboard
#'
#' Add note markers and labels at particular locations to a fretboard plot.
#'
#' @param notes
#'    fretNotes object.
#' @param frets.at
#'    Numeric
#' @param label
#'    Character value specifying how to label notes.
#' @param cex.pt
#'    Numeric value specifying size of note markers.
#' @param cex.label
#'    Numeric value specifying size of note labels.
#' @param from.fret
#'    Numeric value specifying how far from the fret the note markers should
#'    be placed.
#'
#' @return
#'    Returns TRUE upon completion.
#'
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#'
#' @export
#'
drawNotes <-
function(notes, frets.at, label="note", cex.pt=1.6, cex.label=0.6, from.fret){
   if(missing(frets.at)) stop("frets.at argument is required.")
   notes$at <- frets.at[as.char(notes$fret)]
   if(missing(from.fret)){
      # place notes just above the fret line
      from.fret <-
         0.375*par("cin")[2] * abs(diff(par("usr")[3:4])) / par("pin")[2]
   }
   if(!"pch"%in%names(notes)) notes$pch <- 21
   if(!"col"%in%names(notes)) notes$col <- "black"
   if(!"bg"%in%names(notes)) notes$bg <- "gray40"
   if(!"col.label"%in%names(notes)) notes$col.label <- "white"
   if(cex.pt>0){
      points(notes$string, notes$at-from.fret, cex=cex.pt, pch=notes$pch,
             bg=notes$bg, col=notes$col)
   }
   if(label%in%names(notes)){
      text(notes$string, notes$at-from.fret, col=notes$col.label,
           labels=notes[, label], cex=cex.label, adj=c(0.5, 0.5))
   }else if(!is.na(label)){
      warning(paste0('column "', label, '" not found in notes data.frame.'))
   }
   invisible(TRUE)
}

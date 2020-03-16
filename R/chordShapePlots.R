#' Plot Major and Minor Chord Shapes by Fretboard Position
#'
#' Plot major and minor chord shapes of a given root, individually and within
#' the neck of a guitar with standard tuning.  Optionally sevenths of each chord
#' may be included. Six fretboards will be plotted:
#' \itemize{
#'   \item All the notes of major pentatonic scale along the neck.
#'   \item All the major chord tones along the neck.
#'   \item Major chord diagrams for each of the CAGED chord shapes.
#'   \item Minor chord diagrams for each of the CAGED chord shapes.
#'   \item All the minor chord tones along the neck.
#'   \item All the notes of minor pentatonic scale along the neck.
#' }
#'
#' @param tonic
#'    Character value specifying root note of chord and tonic of scale.
#' @param pdffile
#'    Character value specifying path and file name of *.pdf output file.  If
#'    omitted, save plot as "X_chord_shapes.pdf", where X is the root note, with
#'    a "7" appended if seven==TRUE.  Set to NA to prevent a new file from being
#'    opened and closed (to permit external control of graphics device).
#' @param in.context
#'    Logical; if TRUE, indicate the positions of the other notes in the scale
#'    that are not part of the chord as gray circles.
#' @param seven
#'    Logical; if TRUE, include the positions of the dominant/minor 7th.
#'
#' @return
#'    Returns the notes of the chords as a fretNotes object.
#'
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#'
#' @export
#'
chordShapePlots <-
function(tonic, pdffile, in.context=TRUE, seven=F){
   roman <- c("i", "ii", "iii", "iv", "v", "vi", "vii")
   pch.chord <- c(22, 23, 23, 25, 24, 23, 23, 21, 23, 23, 23, 23)
   #   bg.scalenum <-  c("blue", "orange", "red", "green3", "forestgreen",
   #      "cornflowerblue", "purple")
   bg.semitone <- c(
      "blue",             # 1: unison
      "gray70",           # 2: minor 2nd
      "cornflowerblue",   # 3: 2nd
      "purple",           # 4: minor 3rd
      "red",              # 5: 3rd
      "cornflowerblue",           # 6: perfect fourth
      "gray70",           # 7: tritone
      "forestgreen",      # 8: perfect 5th
      "gray70",           # 9: minor 6th
      "cornflowerblue",           #10: 6th
      "orange",           #11: minor 7th
      "gray70")           #12: 7th
   # set up the graphics device to plot 6 fretboards
   if(missing(pdffile)){
      pdffile <- paste0(tonic, ifelse(seven, "7", ""), " chord shapes.pdf")
   }
   #dev.new(width=11, height=8.5)
   if(!is.na(pdffile))  pdf(file=pdffile, width=8.5, height=11, paper="a4")
   layout(matrix(c(rep(1:2,each=5), 2+1:5, 9+1:5, rep(9:8, each=5)), 5))
   pentatonic <- list(major=c(0, 2, 4, 7, 9), minor=c(0, 3, 5, 7, 10))
   par(omi=c(0.,0.,0.75,0))
   for(scale in c("major", "minor")){
      scalenotes <- fretNotes(key=tonic, scale=scale)
      scalenotes$bg <- bg.semitone[scalenotes$semitone+1]
      if(scale=="major") positions <- findPositions(scalenotes)
      root <- 1
      #chordnotes <- diatonicChordNotes(scalenotes, root, seven=seven)
      chordtype <- ifelse(scale=="major", ifelse(seven, "dom7", "maj"),
                          ifelse(seven, "min7", "min"))
      chordnotes <- chordNotesByType(scalenotes, chordtype)
      chord <- attr(chordnotes, "notes")
      chordtype <- attr(chordnotes, "type")
      chordname <- attr(chordnotes, "name")
      rootnote <- attr(chordnotes, "rootnote")
      chordnotes$pch <- pch.chord[chord[chordnotes$note, "semitone"]]
      # plot all the notes of the pentatonic scale on one continuous neck
      par(mar=c(0,0.75,0,0.5), cex=1.3)
      frets.at <- drawNeck(scalenotes, fret.space="even")#36)
      notes <- scalenotes[which(scalenotes$semitone%in%pentatonic[[scale]]), ]
      drawNotes(notes, frets.at)
      if(in.context){
         # add the other notes of the scale, semi-transparent and unlabeled
         context <- scalenotes[!is.na(scalenotes$scalenum), ]
         context <- context[which(context$fret%in%names(frets.at)), ]
         context <- context[which(!context$semitone%in%notes$semitone), ]
         context$note <- ""
         context$col <- "gray70"
         context$bg <- rgb(1,1,1,0.5)
         context$pch <- 21
         drawNotes(context, frets.at)
      }
      axis(2, at=frets.at[as.char(positions$fret)], positions$fret, las=1,
           cex.axis=0.5, mgp=c(1, 0.1, 0), tcl=0, tick=F)
      mtext(paste0(tonic, " ", scale, "\npentatonic"), side=3, line=-1,
            cex=1.1)
      # plot all the chord notes on one continuous neck
      frets.at <- drawNeck(scalenotes, fret.space="even")#36)
      drawNotes(chordnotes, frets.at)
      if(in.context){
         # add the other notes of the scale, semi-transparent and unlabeled
         context <- scalenotes[!is.na(scalenotes$scalenum), ]
         context <- context[which(context$fret%in%names(frets.at)), ]
         context <- context[which(!context$semitone%in%chordnotes$semitone), ]
         context$note <- ""
         context$col <- "gray70"
         context$bg <- rgb(1,1,1,0.5)
         context$pch <- 21
         drawNotes(context, frets.at)
      }
      axis(2, at=frets.at[as.char(positions$fret)], positions$fret, las=1,
           cex.axis=0.5, mgp=c(1, 0.1, 0), tcl=0, tick=F)
      mtext(paste0(tonic, ifelse(scale=="minor", "m ", " "), "chord\ntones"),
            side=3, cex=1.1, line=-1)
      # plot each chord shape by position in separate plots
      par(mar=c(0.25,0.5,1.75,0.25))
      for(posndx in 1:5){
         fret.range <- unlist(positions[posndx, c("lo.fret", "hi.fret")])
         frets.at <-
            drawNeck(scalenotes, fret.range=fret.range, fret.space="even")
         notes <- chordnotes[which(chordnotes$fret>=fret.range[1]
                                   & chordnotes$fret<=fret.range[2]), ]
         notes$pch <- pch.chord[chord[notes$note, "semitone"]]
         drawNotes(notes, frets.at)
         if(in.context){
            # add the other notes of the scale, semi-transparent and unlabeled
            context <- scalenotes[!is.na(scalenotes$scalenum), ]
            context <- context[which(context$fret%in%names(frets.at)), ]
            #if(tonic=="G" & posndx==2 & root==1) browser()
            context <- context[which(!context$semitone%in%notes$semitone), ]
            context$note <- ""
            context$col <- "gray70"
            context$bg <- rgb(1,1,1,0.5)
            context$pch <- 21
            drawNotes(context, frets.at)
         }
         ytix <- as.char(positions[posndx, "fret"])
         axis(2, at=frets.at[ytix], ytix, las=1, cex.axis=0.5, mgp=c(1, 0.1, 0),
              tcl=0, tick=F)
         # deduce the CAGED chord shape
         shape <- notes[order(notes$chordnum, notes$fret, -notes$string), ]
         # cat("\n",chordname, " ", positions[posndx, "position"],":\n", sep="")
         # print(shape)
         shape <- shape[1, "string"] - 1
         shape <- sub(rootnote, c("C","G","D","A","E")[shape], chordname)
         mtext(side=3, cex=1, col="gray40", paste0("(",shape," shape)"),
               line=ifelse(posndx==1 & fret.range[1]==0, -0.5, 0))
         # cat(posndx, sep="", ": ", fret.range[1], " to ", fret.range[2], "\n")
         if(posndx==1){
            mtext(paste(scale, "\npositions"), side=3, line=1, cex=1.2)
         }
      }
   }
   mtext(side=3, outer=T, line=1.75, adj=0, cex=1.7, titleCaps(paste(tonic,
      "Major and Minor Chord Shapes by fretboard position")))
   mtext(side=3, outer=T, line=3.25, adj=1, font=3, cex=0.7, col="gray30",
         "Copyright Michael W. Rowe, 2019")
   if(!is.na(pdffile))  dev.off()
   invisible(chordnotes)
}

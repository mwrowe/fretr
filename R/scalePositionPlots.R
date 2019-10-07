#' Fretboard Maps of Diatonic Chords Within a Scale
#'
#' Plot fretboards of a scale/mode, and its diatonic chords by position.
#'
#' @param tonic
#'    Character value specifying tonic note of scale or model
#' @param scale
#'    Character value specifying scale or mode type.
#' @param pdffile
#'    Character value specifying path and file name of *.pdf output file.  If
#'    omitted, save plot as "X type scale.pdf", where X is the tonic note and
#'    type is the type of scale or mode name.  Set to NA to prevent a new file
#'    from being opened and closed (to permit external control of graphics
#'    device).
#' @param in.context
#'    Logical; if TRUE, indicate the positions of the other notes in the scale
#'    that are not part of the chord as gray circles.
#' @param chorder
#'    Character value, specifying order of chords.
#'    If "4ths", IV-I-V-ii-vi-iii-V7; if "5ths", IV-V7-iii-vi-ii-V-I.
#'    Otherwise, order by scale degree, I - vii
#'
#' @return
#'    Returns a fretNotes objects containing the all notes of the scale or
#'    mode by fretboard position.
#'
#' @author M.W.Rowe, \email{mike.rowe@gmail.com}
#'
#' @export
#'
scalePositionPlots <-
function(tonic, scale="major", pdffile, in.context=FALSE, chorder="5ths"){
   roman <- c("i", "ii", "iii", "iv", "v", "vi", "vii")
   pch.chord <- c(22, 23, 23, 25, 24, 23, 23, 21, 23, 23, 23, 23)
   bg.scalenum <-  c("blue", "orange", "cornflowerblue", "green3", "purple",
                     "forestgreen", "red")
   # set up the graphics device to plot 8 fretboards
   if(missing(pdffile)){
      suffix <- ifelse(chorder%in%c("4ths", "5ths"), paste0(" by ", chorder), "")
      pdffile <- paste0(tonic, " " , scale, " scale", suffix, ".pdf")
   }
   if(!is.na(pdffile)) pdf(file=pdffile, width=11, height=8.5, paper="a4r")
   layout(matrix(c(rep(0,5), 1:45)+1, 5))
   par(mar=c(0,0.5,0,1), omi=c(0.,0.,0.8,0.), cex=1)
   # plot all the notes of the scale/mode on one continuous neck
   scalenotes <- fretNotes(key=tonic, scale=scale)
   scalenotes$bg <- bg.scalenum[scalenotes$scalenum]
   positions <- findPositions(scalenotes)
   frets.at <- drawNeck(scalenotes, fret.space=36)
   notes <- scalenotes[!is.na(scalenotes$scalenum), ]
   notes$pch <- ifelse(notes$scalenum==1, 22, 21)
   drawNotes(notes, frets.at)
   axis(2, at=frets.at[as.char(positions$fret)], positions$fret, las=1,
        cex.axis=0.5, mgp=c(1, 0.1, 0), tcl=0, tick=F)
   mtext("whole\nneck", side=3, line=0.125, cex=1.0)
   # plot all of the scale notes by position (in separate plots)
   par(mar=c(0.25,1.25,0.75,0.25))
   for(posndx in 1:5){
      fret.range <- unlist(positions[posndx, c("lo.fret", "hi.fret")])
      frets.at <- drawNeck(scalenotes, fret.range=fret.range, fret.space="even")
      notes <- scalenotes[!is.na(scalenotes$scalenum), ]
      notes <- notes[which(notes$fret%in%names(frets.at)), ]
      notes$pch <- ifelse(notes$scalenum==1, 22, 21)
      drawNotes(notes, frets.at)
      ytix <- as.char(positions[posndx, "fret"])
      axis(2, at=frets.at[ytix], ytix, las=1, cex.axis=0.5, mgp=c(1, 0.1, 0),
           tcl=0, tick=F)
      title(ylab=paste0("position ", positions[posndx, "position"]),
            mgp=c(0.5,0.2,0), cex.lab=0.9)
      if(posndx==1){
         txt <- "by\nposition"
         mtext(txt, side=3, line=0.75, cex=1.0)
      }
   }
   # plot the pentatonic major scale tones by position
   pentatonic <- list(major=c(0, 2, 4, 7, 9), minor=c(0, 3, 5, 7, 10))
   for(posndx in 1:5){
      fret.range <- unlist(positions[posndx, c("lo.fret", "hi.fret")])
      frets.at <- drawNeck(scalenotes, fret.range=fret.range, fret.space="even")
      notes <- scalenotes[which(scalenotes$semitone%in%pentatonic$major), ]
      notes <- notes[which(notes$fret%in%names(frets.at)), ]
      notes$pch <- ifelse(notes$scalenum==1, 22, 21)
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
      ytix <- as.char(positions[posndx, "fret"])
      axis(2, at=frets.at[ytix], ytix, las=1, cex.axis=0.5, mgp=c(1, 0.1, 0),
           tcl=0, tick=F)
      if(posndx==1){
         mtext("major\npentatonic", side=3, line=0.75, cex=1.0)
      }
   }
   # loop through diatonic chords and plot each position
   chorder <- switch(chorder,
                     "4ths"=c(4, 1, 5, 2, 6, 3, 7),
                     "5ths"=c(4, 7, 3, 6, 2, 5, 1),
                     1:7)
   for(root in chorder){
      seven <- FALSE
      if(root==7){
         root <- 5
         seven <- TRUE
      }
      chordnotes <- diatonicChordNotes(scalenotes, root, seven=seven)
      chord <- attr(chordnotes, "notes")
      chordtype <- attr(chordnotes, "type")
      chordname <- attr(chordnotes, "name")
      rootnote <- attr(chordnotes, "rootnote")
      if(grepl("dim", chordname)){
         chordname <- paste0(sub("dim", "", chordname), "\U00B0")
      }
      for(posndx in 1:5){
         fret.range <- unlist(positions[posndx, c("lo.fret", "hi.fret")])
         frets.at <-
            drawNeck(scalenotes, fret.range=fret.range, fret.space="even")
         notes <- chordnotes[which(chordnotes$fret>=fret.range[1]
                                   & chordnotes$fret<fret.range[2]), ]
         notes$pch <- pch.chord[chord[notes$note, "semitone"]]
         if(seven){
            # dominant 7 chord: fade root notes to emphasize viio notes
            to.adjust <- which(notes$chordnum==1)
            perstring <- table(notes[notes$string%in%notes[to.adjust, "string"],
                                     "string"])
            to.adjust <- which(notes$chordnum==1
                               & notes$string%in%names(perstring[perstring>1]))
            adjustcolor(notes[to.adjust, "bg"], alpha.f=0.2)
            notes[to.adjust, "bg"] <-
               adjustcolor(notes[to.adjust, "bg"], alpha.f=0.3)
            notes$col <- "black"
            notes[to.adjust, "col"] <-
               adjustcolor(notes[to.adjust, "col"], alpha.f=0.3)
         }
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
         mtext(side=3, cex=0.7, col="gray40", paste0("(",shape," shape)"))
         if(posndx==1){
            txt <- roman[root]
            if(seven) txt <- paste0(txt, "7")
            if(grepl("(maj|dom|aug)", chordtype)) txt <- toupper(txt)
            if(grepl("dim", chordtype)) txt <- paste0(txt, "\U00B0")
            txt <- paste0(txt, ": ", chordname)
            mtext(txt, side=3, line=1.25,
                  cex=ifelse(roman[root]=="i", 1.4, 1.1))
            if(seven){
               txt <- paste0("= ", rootnote, " + ", chord[2, "note"], "\U00B0",
                             " (vii\U00B0)")
               mtext(txt, side=3, line=0.6, cex=0.7)
            }
         }
      }
   }
   mtext(side=3, outer=T, line=2.5, adj=0, cex=1.7, titleCaps(paste("Diatonic",
      "Chords of the", tonic, scale, "scale by fretboard position")))
   mtext(side=3, outer=T, line=3.25, adj=1, font=3, cex=0.7, col="gray30",
      "Copyright Michael W. Rowe, 2019")
   if(!is.na(pdffile)) dev.off()
   invisible(scalenotes)
}

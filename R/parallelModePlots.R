#' Fretboard Maps of All Parallel Modes of a Given Tonic
#'
#' Plots seven fretboard maps showing all the modes of a given tonic note,
#' ordered such that only a single note differs between successive modes.
#'
#' @details
#' The modes are ordered according to the cycle of fifths, from brightest to
#' darkest: lydian, ionian, mixolydian, dorian, aeolian, phrygian, locrian.
#' The goal is to relate the modes to each other.
#'
#' @param tonic
#'    A character value specifying the (shared) tonic note of all the modes.
#' @param pdffile
#'    A character value specifying the path and name of the output *.pdf file
#'    to generate.
#'
#' @return
#' Returns a fretnote object.
#'
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#'
#' @export
#'
parallelModePlots <-
function(tonic, pdffile){
   bg.scalenum <-  c("black", "orange", "blue", "green3", "purple",
                     "forestgreen", "red")
   # set up the graphics device to plot 8 fretboards
   if(missing(pdffile)) pdffile <- paste(tonic, scale, "parallel modes.pdf")
   if(!is.na(pdffile)) pdf(file=pdffile, width=11, height=8.5, paper="a4r")
   layout(matrix(c(rep(0,5), 1:35)+1, 5))
   par(mar=c(0,1.5,0,1), omi=c(0.,0.,0.8,0.), cex=1)
   # get positions for the lydian (highest) mode; expand by one fret
   positions <- findPositions(fretNotes(key=tonic, scale="lydian"))
   positions$hi.fret <- positions$hi.fret + 1
   # plot all the notes of the major scale/mode on one continuous neck
   scalenotes <- fretNotes(key=tonic, scale="major")
   scalenotes$bg <- bg.scalenum[scalenotes$scalenum]
   #positions <- findPositions(scalenotes)
   frets.at <- drawNeck(scalenotes, fret.space=36)
   notes <- scalenotes[!is.na(scalenotes$scalenum), ]
   drawNotes(notes, frets.at, cex.pt=1.8)
   axis(2, at=frets.at[as.char(positions$fret)], positions$fret, las=1,
        cex.axis=0.5, mgp=c(1, 0.1, 0), tcl=0, tick=F)
   mtext("major\nscale\n(ionian)", side=3, line=-0.5, cex=1.0)
   modes <- c("lydian","ionian","mixolydian","dorian","aeolian","phrygian",
              "locrian")
   allmodes <- c()
   for(mode in modes){
      modenotes <- fretNotes(key=tonic, scale=mode)
      modenotes <-
         unique(modenotes[!is.na(modenotes$scalenum), c("semitone", "note")])
      modenotes <- modenotes[order(modenotes$semitone), ]
      if(mode=="ionian") colnames(allmodes) <- modenotes$note
      allmodes <- rbind(allmodes, modenotes$semitone)
   }
   rownames(allmodes) <- modes
   allmodes <- as.matrix(allmodes)
   scaleshapes <- c(DtmF="E", mFaFd="D", FdCB="C", CBeFc="A", eFcDt="G")
   # plot all of the scale notes by position (in separate plots), each mode
   par(mar=c(0.15,2.5,1.,0.25))
   for(mode in modes){
      mode.ndx <- which(rownames(allmodes)==mode)
      modenotes <- fretNotes(key=tonic, scale=mode)
      modenotes$bg <- bg.scalenum[modenotes$scalenum]
      not.major <- which(!modenotes$semitone%in%allmodes["ionian", ])
      modenotes[not.major, "bg"] <-
         adjustcolor(modenotes[not.major, "bg"], 0.5)
      modenotes$pch <- ifelse(modenotes$semitone==0, 22, 21)
      if(mode.ndx>1){
         changed <- allmodes[mode,
            which(allmodes[mode.ndx, ]!=allmodes[mode.ndx-1, ])]
         modenotes[which(modenotes$semitone==changed), "pch"] <- 25
      }
      if(mode.ndx<nrow(allmodes)){
         changed <-
            allmodes[mode, which(allmodes[mode.ndx, ]!=allmodes[mode.ndx+1, ])]
         modenotes[which(modenotes$semitone==changed), "pch"] <- 24
      }
      for(posndx in 1:5){
         fret.range <- unlist(positions[posndx, c("lo.fret", "hi.fret")])
         frets.at <- drawNeck(modenotes, fret.range=fret.range,
                              fret.space="even")
         notes <- modenotes[!is.na(modenotes$scalenum), ]
         notes <- notes[which(notes$fret%in%names(frets.at)), ]
         notes$col <- "black"
         fretstr <- makeFretboardString(notes, trim=TRUE)
         for(shape in names(scaleshapes)){
            shape.at <- regexpr(shape, fretstr)
            if(shape.at>0){
               within <- min(notes$fret):max(notes$fret)
               within <- within[shape.at + 0:(nchar(shape)-1)]
               notes[which(!notes$fret%in%within), "bg"] <- rgb(1,1,1,0.2)
               notes[which(!notes$fret%in%within), "col"] <- rgb(0,0,0,0.2)
               break
            }
            shape <- NA
         }
         drawNotes(notes, frets.at, cex.pt=1.8)
         ytix <- as.char(positions[posndx, "fret"])
         axis(2, at=frets.at[ytix], ytix, las=1, cex.axis=0.7, mgp=c(1, 0.1, 0),
              tcl=0, tick=F)
         if(mode==modes[1]){
            title(ylab=paste0("position ", positions[posndx, "position"]),
                  mgp=c(1.5,0.2,0), cex.lab=0.9)
         }
         shape <- scaleshapes[shape]
         if(!is.na(shape)){
            mtext(paste(shape, "shape"), side=3, line=0, col="blue", cex=0.8)
         }
         #title(ylab=paste(shape, "shape"), mgp=c(0.5,0.2,0), cex.lab=0.9)
         if(posndx==1){
            txt <- paste0(mode, "\nmode")
            mtext(txt, side=3, line=1, cex=ifelse(mode=="ionian", 1.3, 1.0),
                  col=ifelse(mode=="ionian", "blue", "black"))
         }
      }
   }
   mtext(side=3, outer=T, line=2.75, adj=0, cex=1.7, titleCaps(paste("Parallel ",
      "Modes of", tonic, "Major by fretboard position")))
   mtext(side=3, outer=T, line=3.25, adj=1, font=3, cex=0.7, col="gray30",
         "Copyright Michael W. Rowe, 2019")
   if(!is.na(pdffile))  dev.off()
   invisible(scalenotes)
}

#' Relative Scale Fretboard Maps (version 2)
#'
#' Plot fretboards of a scale/mode, and its diatonic chords by position
#' (alternate version.)
#'
#' @inheritParams parallelModePlots
#' @param params
#'    A list.
#'
#' @author M.W.Rowe, \email{mike.rowe@gmail.com}
#'
#' @export
#'
relativeScalePlots_v2 <-
function(tonic, pdffile, params=list()){
   bg.scalenum <-  c("black", "orange", "blue", "green3", "purple",
                     "forestgreen", "red")
   # set up the graphics device to plot 8 fretboards
   if(missing(pdffile)) pdffile <- paste0(tonic, " relative scales.pdf")
   #dev.new(width=11, height=8.5)
   pdf(file=pdffile, width=11, height=8.5, paper="a4r")
   layout(matrix(1:8, 1))
   par(mar=c(0,1.5,0,1), omi=c(0.,0.,0.8,0.), cex=1)
   # plot all the notes of the major scale/mode on one continuous neck
   scalenotes <- fretNotes(key=tonic, scale="major")
   scalenotes$bg <- bg.scalenum[scalenotes$scalenum]
   positions <- findPositions(scalenotes)
   frets.at <- drawNeck(scalenotes, fret.space=36)
   notes <- scalenotes[!is.na(scalenotes$scalenum), ]
   drawNotes(notes, frets.at, cex.pt=1.8)
   axis(2, at=frets.at[as.char(positions$fret)], positions$fret, las=1,
        cex.axis=0.5, mgp=c(1, 0.1, 0), tcl=0, tick=F)
   mtext("major\nscale\n(ionian)", side=3, line=-0.5, cex=1.0)
   modes <-
      c("lydian","ionian","mixolydian","dorian","aeolian","phrygian","locrian")
   # go through all modes to find how notes change
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
   # find the first complete position in lydian
   scaleshapes <- c(DtmF="E", mFaFd="D", FdCB="C", CBeFc="A", eFcDt="G")
   modenotes <- fretNotes(key=tonic, scale="lydian")
   fbstr <- makeFretboardString(modenotes, shape.column="scalenum", trim=T)
   position1 <- c()
   for(shape in names(scaleshapes)){
      position1[scaleshapes[shape]] <- regexpr(shape, fbstr)
   }
   position1 <- min(position1)
   # plot all of the scale notes by position (in one plot), each mode
   for(mode in modes){
      mode.ndx <- which(rownames(allmodes)==mode)
      modenotes <- fretNotes(key=tonic, scale=mode)
      modenotes <- modenotes[which(modenotes$fret>=position1-1), ]
      modenotes$bg <- bg.scalenum[modenotes$scalenum]
      not.major <- which(!modenotes$semitone%in%allmodes["ionian", ])
      modenotes[not.major, "bg"] <- adjustcolor(modenotes[not.major, "bg"], 0.5)
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
      plotFretboardByPosition(modenotes, params=params)
      txt <- paste0(mode, "\nmode")
      mtext(txt, side=3, line=1, cex=ifelse(mode=="ionian", 1.3, 1.0),
            col=ifelse(mode=="ionian", "blue", "black"))
   }
   mtext(side=3, outer=T, line=2.75, adj=0, cex=1.7, titleCaps(paste("Relative",
      "Scales of", tonic, "Major by fretboard position")))
   dev.off()
   invisible(scalenotes)
}

#===============================================================================
# 20180727_Fretboards.R
#-------------------------------------------------------------------------------
# A collection of functions for determining the notes of a guitar fretboard
# and drawing fretboards with the notes of particular scales, modes or chords.
# 
# Created Apr 11, 2018
# Author: M.W.Rowe
#===============================================================================
cleanSearch()
if("minions"%in%ls()) killMinions(minions)
rm(list=ls())
graphics.off()
# load standard packages; set up parallel computation
pkgs <- c("parallel","doParallel","foreach")
success <- loadPackages(pkgs)
# set up I/O for the local environment
today <- yyyymmdd()
HT.dir <- Sys.getenv("HT_TOP_DIR")
Rscript.file <- "20180411_Fretboards.R"
out.dir <- "/Users/mike.rowe/Documents/Junk/Guitar/Fretboard Maps/"
#out.file <- "20180411_Fretboards.RData"
#out.dir <- paste(HT.dir,sep="","Analysis/",sub("\\.RData$","",out.file),"/")
success <- prepDir(out.dir)
setwd(out.dir)
#-------------------------------------------------------------------------------
fretNotes <- 
# assign scale notes to positions of a fretboard 
#      
# ARGUMENTS
#    n.frets: integer; number of frets on the fretboard (not counting the nut)
#    key: character value specifying tonic of the scale
#    scale: character value specifying scale type; currently "major" or "minor"
#    tuning: character vector of length 4 to 8, specifing number and tuning of 
#       strings from lowest to highest by note; default is standard guitar,
#       c("E","A","D","G","B","E")
#      
# VALUE
#    Returns a fretNotes object, a data.frame with one row per fretboard 
#    position (fret/string combination), and named columns:
#      
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
      notes <- notes[grep("#", names(notes), v=T, inv=T)]
   }else{
      notes <- notes[grep("b", names(notes), v=T, inv=T)]
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
#...............................................................................
drawNeck <- 
# calculate fret spacing and draw empty fretboard
#      
# ARGUMENTS
#    frets: data.frame returned by fretNotes() function, which will specify
#       numbers of frets and strings
#    fret.space: numeric or character value specifying how evenly the frets 
#       are spaced; valid character values are:
#          "accurate" (=12), "even" (=1000) or between (=24)
#    fret.range: 2-element numeric vector, specifying range of frets to draw
#    decorations: logical; should decorative dots be placed between some frets?
#    scale.length: numeric value specifying distance from nut to bridge in 
#       inches
#  
# VALUE: returns a named vector of the fret positions, with names from "0"  
function(frets, fret.space="between", fret.range, decorations=TRUE, 
      scale.length=25.5){
   if(!"fretNotes"%in%class(frets)){
      stop("use the fretNotes() function to generate the frets argument")
   }
   # calculate where to place the frets
   n.frets <- max(frets$fret)
   n.strings <- max(frets$string)
   spacings <- c(accurate=12, even=1000, between=24)
   if(fret.space%in%names(spacings)) fret.space <- spacings[fret.space]
   if(!is.numeric(fret.space)) error("invalid fret.space argument")
   frets.at <- scale.length - scale.length / 2^(-1:n.frets/fret.space)
   frets$at <- frets.at[frets$fret+2]
   # draw the guitar neck
   if(missing(fret.range)){
      ylim <- rev(range(frets.at))
      #dylim <- c(0,0.025*diff(ylim))
      dylim <- c(0, 0)
   }else{
      ylim <- rev(range(frets.at[sort(fret.range+1)]))
      dylim <- c(0, 0)
      frets <- 
         frets[which(frets$fret>=fret.range[1] & frets$fret<=fret.range[2]), ]
   }
   # plot frets
   to.plot <- frets.at[which(frets.at>=0)]
   matplot(matrix(0.5 + c(0,n.strings), 2, length(to.plot)), 
      rbind(to.plot, to.plot), lty=1, type="l", yaxs="i", xaxs="i", axes=F, 
      xlab="", ylab="", xlim=0.5+c(n.strings,0), ylim=ylim+dylim, 
      lwd=ifelse(to.plot==0, 5, 2), col=ifelse(to.plot==0, "gray50", "gray70"))
   # add strings
   matlines(matrix(rep(1:n.strings,each=2),2), 
      matrix(range(to.plot),2,n.strings), col="gray85",lty=1, lwd=1)
   # add edges
   matlines(matrix(0.5+c(0,0,6,6), 2), matrix(range(to.plot), 2), lty=1,
      col="black")
   if(decorations){
      decos <- c(3,5,7,9,12,12,15,17,19,21,23)-1
      decos.y <- (frets.at[-1:-2] + diff(frets.at[-1])/2)[decos]
      decos.x <- rep(n.strings/2+0.5, length(decos))
      decos.x[which(decos+1==12)] <- n.strings/2+0.5 + sym(1)
      points(decos.x, decos.y, pch=19, cex=1.5, col="gray90")
   }
   frets.at <- unique(frets[, c("fret", "at")])
   frets.at <- structure(names=frets.at$fret, frets.at$at)
   invisible(frets.at)
}
#...............................................................................
diatonicChordNotes <- 
# find the notes of a diatonic chord within its scale, given the root note
#      
# ARGUMENTS
#    scalenotes: data.frame() of notes assigned to each fretboard position,
#       numbered and named according to the underlying scale, such as generated
#       by the fretNotes() function.
#    root: integer value specifying the root by number relative to the scale
#       from 1 to 7.
#    seven: logical; if TRUE, add the 7th of the chord to the triad
#      
# VALUE
#    Returns a fretNotes (data.frame) object with the subset of the scalenotes
#    argument found in the specified chord, with a chordnum column appended,
#    plus named attributes "rootnote", "notes" (data.frame), "type" and "name".
#  
function(scalenotes, root=1, seven=F){
   notes <- unique(scalenotes[!is.na(scalenotes$scalenum), 
      c("semitone","scalenum","note")])
   notes <- notes[order(notes$semitone), ]
   if(is.numeric(root)){
      root <- (root-1)%%8 + 1
      rootnote <- notes[match(root, notes$scalenum), "note"]
   }else if(root%in%notes$note){
      rootnote <- root
      root <- notes[match(rootnote, notes$note), "scalenum"]
   }else stop("Invalid root: must be a number 1-7 or a note in the scale.")
   notes$chordnum <- (notes$scalenum - root)%%7 + 1
   if(seven){
      notes <- notes[which(notes$chordnum%in%c(1,3,5,7)), ]
   }else{
      notes <- notes[which(notes$chordnum%in%c(1,3,5)), ]
   }
   scalenotes <- merge(scalenotes, notes, sort=F)
   scalenotes <- scalenotes[order(scalenotes$fret, -scalenotes$string), ]
   chord <- unique(scalenotes[, c("note", "chordnum", "semitone")])
   chord <- chord[order(chord$chordnum), ]
   chord$semitone <- (chord$semitone - chord$semitone[1])%%12 + 1
   chord$interval <- c(0, diff(chord$semitone))
   chordtypes <- c("43"="maj", "34"="min", "33"="dim", "44"="aug", "433"="dom7",
      "434"="maj7", "343"="min7", "344"="minmaj7", "333"="dim7", 
      "334"="halfdim7", "443"="aug7")
   chordtype <- chordtypes[paste0(chord[-1,"interval"], collapse="")]
   chord$interval <-
      ifelse(chord$interval==3, "min3", ifelse(chord$interval==4, "maj3", "-"))
   rownames(chord) <- chord$note
   rownames(scalenotes) <- NULL
   attr(scalenotes, "rootnote") <- rootnote
   attr(scalenotes, "notes") <- chord
   attr(scalenotes, "type") <- chordtype
   attr(scalenotes, "name") <- 
      paste0(rootnote, sub("min", "m", sub("(maj$|dom)", "", chordtype)))
   class(scalenotes) <- c("fretNotes", "data.frame")
   scalenotes
}
#...............................................................................
chordNotesByType <- 
# get notes of an arbitrary chord, without regards to underlaying scale/mode
#      
# ARGUMENTS
#    scalenotes: data.frame() of notes assigned to each fretboard position,
#       numbered and named according to the underlying scale, such as generated
#       by the fretNotes() function.
#    type: character value specifying a particular chord type by abbreviation.
#       Currently supported types are: "maj", "min", "dim", "aug", "sus2", 
#       "sus4", "maj7", "dom7", "min7", "halfdim7", "dim7", "minmaj7", "aug7"
#     root: character value specifying the root note of the chord.  If 
#        unspecified, the tonic note (key) of the scale will be used.
#      
# VALUE
#    Returns a fretNotes (data.frame) object with the subset of the scalenotes
#    argument found in the specified chord, with a chordnum column appended,
#    plus named attributes "rootnote", "notes" (data.frame), "type" and "name".
#     
function(scalenotes, type="maj", root){
   chordtypes <- list(   # list of successive intervals for each chord type
      maj      = c(4, 3),
      min      = c(3, 4),
      dim      = c(3, 3),
      aug      = c(4, 4),
      sus2     = c(2, 5),
      sus4     = c(5, 2),
      maj7     = c(4, 3, 4),
      dom7     = c(4, 3, 3),
      min7     = c(3, 4, 3),
      halfdim7 = c(3, 3, 4),
      dim7     = c(3, 3, 3),
      minmaj7  = c(3, 4, 4),
      aug7     = c(4, 4, 3))
   if(missing(scalenotes)) return(chordtypes)
   if(!type%in%names(chordtypes)){
      stop("Chord type not recognized.")
   }
   if(missing(root)){
      rootnum <- 0
      root <- scalenotes[which(scalenotes$semitone==rootnum)[1], "note"]
   }else{
      if(!root%in%scalenotes$note) stop("root is not a recognized note.")
      rootnum <- scalenotes[which(scalenotes$note==root)[1], "semitone"]
   }
   scale <- unique(
      scalenotes[!is.na(scalenotes$scalenum), c("semitone", "scalenum", "note")])
   scale <-  scale[order(scale$semitone), "note"]
   semitones <- c(rootnum, cumsum(chordtypes[[type]]))%%12
   chordnotes <- scalenotes[which(scalenotes$semitone%in%semitones),
      c("note", "semitone", "scalenum", "string", "fret", "at", "notenum","bg")]
   chordnotes <- chordnotes[order(chordnotes$fret, -chordnotes$string), ]
   chordnotes$chordnum <- match(substr(chordnotes$note,1,1), substr(scale,1,1))
   # create data.frame with just unique chord notes
   chord <- unique(chordnotes[, c("note", "chordnum", "semitone")])
   chord <- chord[order(chord$chordnum), ]
   chord$semitone <- (chord$semitone - chord$semitone[1])%%12 + 1
   chord$interval <- c(0, diff(chord$semitone))
   chord$interval <- c("-", "min2", "maj2", "min3", "maj3", "perf4", 
      "dim5", "perf5", "min6", "maj6", "min7", "maj7")[chord$interval+1]
   rownames(chord) <- chord$note
   # add information as attributes to the chordnotes data.frame
   rownames(chordnotes) <- NULL
   attr(chordnotes, "rootnote") <- root
   attr(chordnotes, "notes") <- chord
   attr(chordnotes, "type") <- type
   attr(chordnotes, "name") <- 
         paste0(root, sub("min", "m", sub("(maj$|dom)", "", type)))
   class(chordnotes) <- c("fretNotes", "data.frame")
   chordnotes
}
#...............................................................................
findPositions <- 
# find CAGED chord/scale positions along the fretboard for the present scale
#      
# ARGUMENTS
#    scalenotes: data.frame() of notes assigned to each fretboard position,
#       numbered and named according to the underlying scale, such as generated
#       by the fretNotes() function.
#    min.fret: integer defining the lowest fret to consider when searching for
#       the first occurrence of each scale/chord positions
#      
# NOTES:
# - Position 1 is defined where the tonic note falls under the middle finger
#   of the 6th string.  In the CAGED system, the tonic chord will use the 
#   E shape.  
# - Positions 2 thru 5 correspond to the CAGED D, C, A and G positions,
#   respectively.
function(scalenotes, min.fret=0){
   scalenums <- c(1,2,3,5,6)
   positions <- scalenotes[
      which(scalenotes$string==6 & scalenotes$scalenum%in%scalenums), 
      c("fret", "note", "scalenum", "semitone")]
   positions <- data.frame(position=match(positions$scalenum, scalenums),
      first=F, positions, lo.fret=positions$fret-1, hi.fret=positions$fret+4)
   max.fret <- attr(scalenotes, "fret.range")[2]
   positions <- positions[which(positions$lo.fret>=min.fret), ]
   positions <- positions[which(positions$hi.fret<=max.fret), ]
   positions$first <- numberReplicates(positions$position)==1
   rownames(positions) <- NULL
   positions
}
#...............................................................................
drawNotes <- 
# add note markers at particular locations to a fretboard plot
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
# END OF LOW-LEVEL FRETBOARD FUNCTIONS
#-------------------------------------------------------------------------------
# HIGHER LEVEL FUNCTIONS TO GENERATE COMPLEX PLOTS WITH MULTIPLE FRETBOARDS
scalePositionPlots <- 
# plot fretboards of a scale/mode, and its diatonic chords by position
#   
# ARGUMENTS
#    tonic: character value specifying tonic note of scale or model
#    scale: character value specifying scale or mode type
#    pdffile: path and file name of *.pdf output file.  If omitted, save plot
#       as "X type scale.pdf", where X is the tonic note and type is the type
#       of scale or mode name.  Set to NA to prevent a new file from being
#       opened and closed (to permit external control of graphics device).
#    in.context: logical; if TRUE, indicate the positions of the other notes
#       in the scale that are not part of the chord as gray circles.
#    by.5ths: logical; if TRUE, order chords by the circle of 5ths, starting
#       with the IV chord.  If FALSE, order by scale degree, I - vii
#      
function(tonic, scale="major", pdffile, in.context=FALSE, by.5ths=TRUE){
   roman <- c("i", "ii", "iii", "iv", "v", "vi", "vii")
   pch.chord <- c(22, 23, 23, 25, 24, 23, 23, 21, 23, 23, 23, 23)
   bg.scalenum <-  c("blue", "orange", "cornflowerblue", "green3", "purple",
      "forestgreen", "red")
   # set up the graphics device to plot 8 fretboards
   if(missing(pdffile)){
      pdffile <- 
         paste(tonic, scale, "scale", ifelse(by.5ths, "_by5ths", ""), ".pdf")
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
   if(by.5ths) chorder <- c(4, 1, 5, 2, 6, 3, 7) else chorder <- 1:7
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
            adjustcolor(notes[to.adjust, "bg"], alpha=0.2)
            notes[to.adjust, "bg"] <- 
                  adjustcolor(notes[to.adjust, "bg"], alpha=0.3)
            notes$col <- "black"
            notes[to.adjust, "col"] <- 
                  adjustcolor(notes[to.adjust, "col"], alpha=0.3)
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
#         cat("\n",chordname, " ", positions[posndx, "position"],":\n", sep="")
#         print(shape)
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
#...............................................................................
chordShapePlots <- 
# plot major and minor chord shapes, individually and within neck
#   
# ARGUMENTS
#    tonic: character value specifying root note of chord and tonic of scale
#    pdffile: path and file name of *.pdf output file.  If omitted, save plot
#       as "X_chord_shapes.pdf", where X is the root note, with a "7" appended
#       if seven==TRUE.  Set to NA to prevent a new file from being opened and
#       closed (to permit external control of graphics device).
#    in.context: logical; if TRUE, indicate the positions of the other notes
#       in the scale that are not part of the chord as gray circles.
#    seven: if TRUE, include the positions of the dominant/minor 7th.
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
   # set up the graphics device to plot 8 fretboards
   if(missing(pdffile)){
      pdffile <- paste0(tonic, ifelse(seven, "7", ""), " chord shapes.pdf")
   }
   #dev.new(width=11, height=8.5)
   if(!is.na(pdffile))  pdf(file=pdffile, width=8.5, height=11, paper="a4")
   #layout(matrix(c(rep(1:2,each=6), 2+1:5, 15, 9+1:5, 15, rep(9:8, each=6)), 6))
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
#         cat("\n",chordname, " ", positions[posndx, "position"],":\n", sep="")
#         print(shape)
         shape <- shape[1, "string"] - 1
         shape <- sub(rootnote, c("C","G","D","A","E")[shape], chordname)
         mtext(side=3, cex=1, col="gray40", paste0("(",shape," shape)"),
            line=ifelse(posndx==1 & fret.range[1]==0, -0.5, 0))
#         cat(posndx, sep="", ": ", fret.range[1], " to ", fret.range[2], "\n")
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
#...............................................................................
#identifyScaleShape <- 
## given a cluster of notes within a few frets, identify the chord/scale shape
#function(notes){
#   if("chordnum"%in%names(notes)){
#      notes <- notes[which(notes$chordnum==1), ]
#   }else{
#      notes <- notes[which(notes$semitone==0), ]
#   }
#   if(nrow(notes)==0) stop("root/tonic not found in this set of notes.")
#   shape <- notes[order(notes$fret, -notes$string), ]
#   shape <- shape[1, "string"] - 1
#   shape <- c("C","G","D","A","E")[shape]
#   shape
#}
makeFretboardString <- 
# convert a set of notes on a fretboard to a string representation
function(notes, shape.column="scalenum", trim=FALSE){
   alphabet <- c(letters, LETTERS)
   notes <- notes[which(notes$string<6), ] # strings 1 and 6 share the same note
   if(!is.null(shape.column)){
      if(!shape.column%in%names(notes)){
         stop("specifed shape.column not found in notes")
      }
      # remove any notes not found in shape.column (i.e., the scale or chord)
      notes <- notes[!is.na(notes[, shape.column]), ]
   }
   notes <- notes[order(notes$fret, notes$string), ]
   fretstr <- attr(notes, "fret.range")
   fretstr <- fretstr[1]:fretstr[2]
   fretstr <- structure(names=fretstr, rep("a", length(fretstr)))
   strs <- tapply(notes$string, notes$fret, function(x){
            alphabet[sum(2^(x-1))+1]
         })
   fretstr[names(strs)] <- strs
   fretstr <- paste0(fretstr, collapse="")
   if(trim) fretstr <- gsub("(^a+|a?.a+$)", "", fretstr)
   attr(fretstr, "fret.range") <- attr(notes, "fret.range")
   fretstr
}
#...............................................................................
plotFretboardByPosition <- 
# plot whole fretboard with positions separated by offset; join frets in common
function(notes, params=list()){
   defaults <- list(
      shape.column = "scalenum",
      shapes = c(DtmF="E", mFaFd="D", FdCB="C", CBeFc="A", eFcDt="G"),
      # parameters controlling how fretboards are drawn
      mar = c(0.25, 2.5, 1.25, 0.25),
      gap.factor = 0.8,
      lwd.string = 1, 
      col.string = "gray85", 
      lwd.fret = 2,
      col.fret = "gray70",
      lwd.nut = 5,
      col.nut = "gray50",
      fret.labels = c(3, 5, 7, 10, 12, 15, 17, 19, 22),
      cex.fret.labels = 0.7,
      # parameters controlling how primary notes are drawn
      note.column = "scalenum",
      note.label = "note", 
      col.notes = "black",
      bg.notes = "blue",
      pch.notes = 21,
      cex.notes = 1.7,
      col.text.notes = "white",
      cex.text.notes = 0.6,
      # notes to mark but not label (like scale notes around chord notes)
      context.column = "scalenum",  
      col.context = rgb(0,0,0,0.2),
      bg.context = rgb(1,1,1,0.2),
      cex.context = 1.6,
      pch.context = 21)
   for(Li in setdiff(names(defaults), names(params))){
      params[[Li]] <- defaults[[Li]]
   }
   # where unspecified, apply defaults to notes and context
   note.params <- grep("\\.notes$", names(params), v=T)
   note.params <- 
      structure(names=sub("\\.notes$", "", note.params), note.params)
   for(Li in setdiff(names(note.params), names(notes))){
      notes[, Li] <- params[[note.params[Li]]]
   }
   for(Li in setdiff(grep("\\.context$", names(params), v=T), names(notes))){
      notes[, Li] <- params[[Li]]
   }
   # generate fretstring and find 1st occurrence of each shape
   shapestr <- makeFretboardString(notes, params$shape.column, trim=F)
   frets <- attr(notes, "fret.range")
   frets <- frets[1]:frets[2]
   positions <- data.frame(shape=params$shapes, shapestr=names(params$shapes),
      position=NA, from.fret=NA, to.fret=NA, n.frets=NA, overlap=NA, offset=NA)
   for(Ri in 1:nrow(positions)){
      at <- regexpr(positions[Ri, "shapestr"], shapestr)
      if(at<0) next # shape not found
      positions[Ri, "from.fret"] <- frets[at]
      positions[Ri, "to.fret"] <- 
         frets[at + nchar(positions[Ri, "shapestr"]) -1 ]
      positions[Ri, "n.frets"] <- nchar(positions[Ri, "shapestr"])
   }
   if(any(is.na(positions$from.fret))){
      positions <- positions[!is.na(positions$from.fret), ]
      if(nrow(positions)==0){
         stop("no shapes found with these notes.")
      }else{
         warning("not all shapes found within these notes.")
      }
   }
   positions <- positions[order(positions$from.fret), ]
   rownames(positions) <- NULL
   positions$position <- 1:nrow(positions)
   positions$overlap <- c(0, 
      positions[-nrow(positions), "to.fret"] - positions[-1, "from.fret"] + 1)
   # calculate where fretboards for each shape will be placed
   frets <- do.call(rbind, 
      tapply(positions$position, positions$position, function(x){
         rng <- (positions[x, "from.fret"]-1):positions[x, "to.fret"]
         data.frame(position=x,  fret=rng, draw.notes=1:length(rng)>1, y=NA)
      }))
   frets$y <- 
      1:nrow(frets) - frets$position + params$gap.factor*(frets$position - 1)
   # start drawing
   n.strings <- max(notes$string, na.rm=T)
   par(mar=params$mar)
   ylim <- c(1.001*max(frets$y), ifelse(frets[1, "fret"]==0, 0.025, 0.0005))
   plot(0, 0, type="n", xlim=0.5 + c(n.strings, 0), ylim=ylim, xaxs="i",
      yaxs="i", axes=F, xlab="", ylab="")
   # place notes just above the fret line
   from.fret <- 0.375*par("cin")[2] * abs(diff(par("usr")[3:4])) / par("pin")[2]
   # draw frets
   frets <- frets[which(frets$fret>=0), ]
   matlines(matrix(0.5 + c(0, n.strings), 2, nrow(frets)), 
      matrix(rep(frets$y, each=2), 2), lty=1,
      lwd=ifelse(frets$fret==0, params$lwd.nut, params$lwd.fret),
      col=ifelse(frets$fret==0, params$col.nut, params$col.fret))
   context <- 
      (params$context.column!="" & params$context.column!=params$note.column)
   for(pndx in 1:nrow(positions)){
      # draw strings for this position
      posfrets <- frets[which(frets$position==pndx), ]
      matlines(matrix(rep(1:n.strings, each=2), 2), lwd=params$lwd.string,
         matrix(range(posfrets$y), 2, n.strings), lty=1, col=params$col.string)
      matlines(matrix(0.5+rep(c(0, n.strings), each=2), 2), lwd=1,
         matrix(range(posfrets$y), 2, 2), lty=1, col="black")
      if(context){
         # draw (unlabeled) contextual notes
         posnotes <- merge(posfrets[which(frets$draw.notes), ], 
            notes[!is.na(notes[, params$context.column]), ])
         points(posnotes$string, posnotes$y-from.fret, pch=posnotes$pch.context, 
            bg=posnotes$bg.context, col=posnotes$col.context, 
            cex=posnotes$cex.context)
      }
      posnotes <- merge(posfrets[which(posfrets$draw.notes), ], 
         notes[!is.na(notes[, params$note.column]), ])
      points(posnotes$string, posnotes$y-from.fret, pch=posnotes$pch, 
         bg=posnotes$bg, col=posnotes$col, cex=posnotes$cex)
      if(params$note.label%in%names(posnotes)){
         text(posnotes$string, posnotes$y-from.fret, adj=c(0.5,0.5),
            col=posnotes$col.text, posnotes[, params$note.label],
            cex=posnotes$cex.text)
      }
      y <- posfrets[1, "y"]-0.15-ifelse(posfrets[1,"fret"]==0, 2*from.fret, 0)
      if(y<0){
         mtext(side=3, line=0, paste0(positions[pndx, "shape"], " shape"),
            col="blue", adj=0.5, cex=0.7)
      }else{
         text(x=n.strings/2+0.5, y, paste0(positions[pndx, "shape"], " shape"),
            col="blue", adj=c(0.5,0), cex=0.7)
      }
      axis(2, posfrets[2, "y"], posfrets[2, "fret"], tcl=-0.2, 
         cex.axis=params$cex.fret.labels, las=1, lwd=0, mgp=c(1, 0.2, 0))
   }
   # add tick marks connecting the labelled frets between positions
#   tix <- frets[which(frets$fret%in%params$fret.labels), ]
#   axis(2, tix$y, tix$fret, tcl=-0.2, cex.axis=params$cex.fret.labels, las=1,
#      lwd=0, mgp=c(1, 0.2, 0))
#   joined <- table(tix$fret)
#   for(fret in as.numeric(names(joined[joined>1]))){
#      axis(2, tix[which(tix$fret==fret), "y"], mgp=c(1, 1.5, 1), lwd.tick=0,
#         labels=rep("", sum(tix$fret==fret)))
#   }
   frets
}
#...............................................................................
relativeScalePlots_v2 <- 
# plot fretboards of a scale/mode, and its diatonic chords by position
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
#...............................................................................
parallelModePlots <- 
# plot fretboards of a scale/mode, and its diatonic chords by position
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
   modes <- 
      c("lydian","ionian","mixolydian","dorian","aeolian","phrygian","locrian")
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
modes <- 
   c("ionian","mixolydian","dorian","aeolian","phrygian","locrian","lydian")
#===============================================================================
# END OF FUNCTION DEFINITIONS; DRAW FRETBOARDS!!!
# generate major/minor chord shape plots for each root in a single file
graphics.off()
seven <- TRUE
pdffile <- 
   paste0("maj", ifelse(seven,"7-min7", "or-minor"), " chord shapes.pdf")
pdf(file=pdffile,  width=8.5, height=11, paper="a4", onefile=TRUE)
for(key in c("F","C","G","D","A","E","B","F#","Db","Ab","Eb","Bb")){
   chordShapePlots(key, in.context=T, pdffile=NA, seven=seven)
}
dev.off()
# generate scale positions plots for each tonic in a single file
graphics.off()
for(by.5ths in c(T, F)){
   pdf(file=paste0("major scale fretboards", ifelse(by.5ths, " by 5ths", ""), 
      ".pdf"), width=11, height=8.5, paper="a4r", onefile=TRUE)
   for(key in c("F","C","G","D","A","E","B","F#","Db","Ab","Eb","Bb")){
      scalePositionPlots(key, in.context=T, pdffile=NA, by.5ths=by.5ths)
   }
   dev.off()
}

# generate relative scale plots for each tonic in a single file
graphics.off()
pdf(file="parallel scale fretboards.pdf", width=11, height=8.5, paper="a4r",
   onefile=TRUE)
for(key in c("F","C","G","D","A","E","B","F#","Db","Ab","Eb","Bb")){
   parallelModePlots(key, pdffile=NA)
}
dev.off()

browser()
#===============================================================================
# HALF BAKED STUFF...
# generate scale position plots
graphics.off()
for(key in c("F","C","G","D","A","E","B","F#","Db","Ab","Eb","Bb")){
   scalePositionPlots(key, in.context=T)
   chordShapePlots(key, in.context=T)
}
#scalePositionPlots("B", scale="mixolydian")
# single fretboard figure
graphics.off()
dev.new(width=0.75,height=7)
par(mar=rep(0.1,4))
tonic <- "G"
scale <- "major"
#pch.chord <- c(22, 23, 23, 25, 24, 23, 23, 21, 23, 23, 23, 23)
bg.scalenum <-  c("blue", "orange", "cornflowerblue", "green3", "purple",
      "forestgreen", "red")
scalenotes <- fretNotes(key=tonic, scale=scale)
scalenotes$bg <- bg.scalenum[scalenotes$scalenum]
positions <- findPositions(scalenotes)
frets.at <- drawNeck(scalenotes, fret.space="even")
notes <- scalenotes[!is.na(scalenotes$scalenum), ]
drawNotes(notes, frets.at)
axis(2, at=frets.at[as.char(positions$fret)], positions$fret, las=1,
      cex.axis=0.5, mgp=c(1, 0.1, 0), tcl=0, tick=F)

## eight-fretboard figure
#graphics.off()
#dev.new(width=11,height=8)
##layout(matrix(c(rep(0,5), 1:45)+1, 5))
#layout(matrix(1:8, 1))
#par(mar=c(0,2,0,1.25), omi=c(0.01,0.01,0.8,0.01), cex=1)
#
#for(mode in modes[c(1:7, 1)]){
#   scalenotes <- fretNotes(key="G", scale=mode)
#   scalenotes <- drawNeck(scalenotes, fret.space="between")
#   notes <- scalenotes[!is.na(scalenotes$scalenum), ]
#   drawNotes(notes)
#}
#
#scalePositionPlots("G")

#===============================================================================

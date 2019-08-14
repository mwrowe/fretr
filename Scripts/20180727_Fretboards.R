#===============================================================================
# 20180727_Fretboards.R
#-------------------------------------------------------------------------------
# A collection of functions for determining the notes of a guitar fretboard
# and drawing fretboards with the notes of particular scales, modes or chords.
#
# Created Apr 11, 2018
# Author: M.W.Rowe
#===============================================================================
#-------------------------------------------------------------------------------
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
#===============================================================================
# END OF FUNCTION DEFINITIONS; DRAW FRETBOARDS!!!
out.dir <- file.path(getwd(), "Scripts")
outmodes <-
   c("ionian","mixolydian","dorian","aeolian","phrygian","locrian","lydian")
# generate major/minor chord shape plots for each root in a single file
graphics.off()
seven <- TRUE
pdffile <- file.path(out.dir,
   paste0("maj", ifelse(seven,"7-min7", "or-minor"), " chord shapes.pdf"))
pdf(file=pdffile,  width=8.5, height=11, paper="a4", onefile=TRUE)
for(key in c("F","C","G","D","A","E","B","F#","Db","Ab","Eb","Bb")){
   chordShapePlots(key, in.context=T, pdffile=NA, seven=seven)
}
dev.off()
# generate scale position plots for each tonic in a single file
graphics.off()
for(by.5ths in c(T, F)){
   pdffile <- file.path(out.dir,
      paste0("major scale fretboards", ifelse(by.5ths, " by 5ths", ""), ".pdf"))
   pdf(file=pdffile, width=11, height=8.5, paper="a4r", onefile=TRUE)
   for(key in c("F","C","G","D","A","E","B","F#","Db","Ab","Eb","Bb")){
      scalePositionPlots(key, in.context=T, pdffile=NA, by.5ths=by.5ths)
   }
   dev.off()
}

# generate relative scale plots for each tonic in a single file
graphics.off()

pdf(file=file.path(out.dir, "parallel scale fretboards.pdf"),
    width=11, height=8.5, paper="a4r", onefile=TRUE)
for(key in c("F","C","G","D","A","E","B","F#","Db","Ab","Eb","Bb")){
   parallelModePlots(key, pdffile=NA)
}
dev.off()

#===============================================================================
# HALF BAKED STUFF...
# # generate scale position plots
# graphics.off()
# for(key in c("F","C","G","D","A","E","B","F#","Db","Ab","Eb","Bb")){
#    scalePositionPlots(key, in.context=T)
#    chordShapePlots(key, in.context=T)
# }
# #scalePositionPlots("B", scale="mixolydian")
# # single fretboard figure
# graphics.off()
# dev.new(width=0.75,height=7)
# par(mar=rep(0.1,4))
# tonic <- "G"
# scale <- "major"
# #pch.chord <- c(22, 23, 23, 25, 24, 23, 23, 21, 23, 23, 23, 23)
# bg.scalenum <-  c("blue", "orange", "cornflowerblue", "green3", "purple",
#       "forestgreen", "red")
# scalenotes <- fretNotes(key=tonic, scale=scale)
# scalenotes$bg <- bg.scalenum[scalenotes$scalenum]
# positions <- findPositions(scalenotes)
# frets.at <- drawNeck(scalenotes, fret.space="even")
# notes <- scalenotes[!is.na(scalenotes$scalenum), ]
# drawNotes(notes, frets.at)
# axis(2, at=frets.at[as.char(positions$fret)], positions$fret, las=1,
#       cex.axis=0.5, mgp=c(1, 0.1, 0), tcl=0, tick=F)

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

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

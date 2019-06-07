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

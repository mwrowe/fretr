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

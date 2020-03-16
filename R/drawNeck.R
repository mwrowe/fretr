#' Draw A Fretboard
#'
#' Calculate fret spacing and draw an empty fretboard.
#
#' @param frets
#'    Data.frame returned by fretNotes() function, which will specify numbers
#'    of frets and strings
#' @param fret.space
#'    Numeric or character value specifying how evenly the frets are spaced;
#'    valid character values are: "accurate" (=12), "even" (=1000) or "between"
#'    (=24).
#' @param fret.range
#'    2-element numeric vector, specifying range of frets to draw.
#' @param decorations
#'    Logical; should decorative dots be placed between some frets?
#' @param scale.length
#'    Numeric value specifying distance from nut to bridge in inches.
#
#' @return
#'    Returns a named vector of the fret positions, with names from "0".
#'
#' @author M.W.Rowe, \email{mwr.stats@gmail.com}
#'
#' @export
drawNeck <-
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
   if(!is.numeric(fret.space)) stop("invalid fret.space argument")
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

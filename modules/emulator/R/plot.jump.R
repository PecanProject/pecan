##' @name plot.jump
##' @title plot.jump
##' @export
##' 
##' @param jmp jump parameter
##' 
##' @author Michael Dietze
`plot.jump` <-
function(jmp){
  par(mfrow=c(1,2))
  plot(attr(jmp,"history"),ylab="Jump Parameter",main="Jump Parameter")
  abline(h=mean(attr(jmp,"history"),na.rm=TRUE))
  plot(attr(jmp,"arate"),main="Acceptance Rate",ylim=c(0,1),ylab="Acceptance Rate")
  abline(h=mean(attr(jmp,"arate"),na.rm=TRUE))
  abline(h=attr(jmp,"target"),col=2)
}


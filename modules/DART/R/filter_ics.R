create_filter_ics <- function(n,t,x,y,ms,ss,mf,sf){
	px <- x + runif(n,-0,0)
	py <- y + runif(n,-0,0)	
	pz <- rnorm(n,ms,ss)
	pf <- rnorm(n,mf,sf)

	bt <- c(0,t)
	g <- cbind(px,py,pz,pf)

	for(j in 1:n){
		write(bt,file="filter_ics",append=j>1 )	
		write(g[j,],file="filter_ics",sep="\n",append=TRUE)
  }
}
#create_filter_ics(64,101,1,0.1)
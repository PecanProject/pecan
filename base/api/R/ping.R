#------------------------------------------- Ping function -------------------------------------------#
##' Function to be executed when /ping API endpoint is called
##'
##' If successful connection to API server is established, this function will return the "pong" message
##' @return Mapping containing response as "pong"
##' @author Tezan Sahu
##' @export
ping <- function(){
	list(
		request = "ping",
		response = "pong"
	)
}

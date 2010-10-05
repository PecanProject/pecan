##input variables
pft   <- system("echo $PFT", intern = TRUE)
ITER  <- as.numeric(system("echo $ITER", intern = TRUE)) 
M     <- as.numeric(system("echo $ENSN", intern = TRUE))
print(cat("PECAn run with ",pft,
          "\nmeta-analysis has",ITER,"iterations",
          "\nensemble has",M,"config files"))
save.image(file='pecan.start.Rdata')

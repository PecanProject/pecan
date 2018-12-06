## testing LBL remote

setwd("~/git/pecan/web")

## connection settings
## define 'pass' as Google Authenticator current password (string)
exe_host <- "lrc-login.lbl.gov"
data_host <- "lrc-xfer.lbl.gov"
user <- 'dietze'
tunnelDir <- "/tmp/LBNL"
exeDir <- file.path(tunnelDir,"exe")
dataDir <- file.path(tunnelDir,"data")
dir.create(tunnelDir)
dir.create(exeDir)
dir.create(dataDir)
R <- "/global/software/sl-6.x86_64/modules/langs/r/3.2.5/bin/R"

##open connections
write(pass,file.path(exeDir,"password"))
con1 <- system2("./sshtunnel.sh",c(exe_host,user,exeDir,">",file.path(exeDir,"log"),"&"))
file.remove(file.path(exeDir,"password"))

write(pass,file.path(dataDir,"password"))
con2 <- system2("./sshtunnel.sh",c(data_host,user,dataDir,">",file.path(dataDir,"log"),"&"))
file.remove(file.path(dataDir,"password"))

## build host list
host <- list(name=exe_host,
             data_hostname = data_host,
             tunnel = file.path(exeDir,"tunnel"),
             data_tunnel = file.path(dataDir,"tunnel"),
             Rbinary = R)
settings <- list(host=host)


## test remote.copy.to
PEcAn.remote::remote.copy.to(host,"favicon.ico","~/favicon.ico")

## test remote.execute.cmd
foo <- PEcAn.remote::remote.execute.cmd(host,"pwd")
print(foo)
PEcAn.remote::remote.execute.cmd(host,"mv",c("/global/home/users/dietze/favicon.ico","/global/home/users/dietze/favicon.jpg"))

## test remote.copy.from
PEcAn.remote::remote.copy.from(host,"~/favicon.jpg","favicon.jpg")

## test remote.execute.R
b <- PEcAn.remote::remote.execute.R(script = "return(1)",host = host,R=R,verbose=TRUE,scratchdir="/global/scratch/dietze/")

c <- PEcAn.remote::remote.execute.R(script = "return(require(PEcAn.data.atmosphere))",host = host,R=R,verbose=TRUE,scratchdir="/global/scratch/dietze/")

d <- PEcAn.remote::remote.execute.R(script = "return(.libPaths())",host = host,R=R,verbose=TRUE,scratchdir="/global/scratch/dietze/")


## kill tunnels
PEcAn.remote::kill.tunnel(settings)

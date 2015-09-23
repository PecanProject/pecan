##' @title PEcAn Network Status
##' @author Michael Dietze
##' 
##'
##' 

#args <- commandArgs(TRUE)

library(PEcAn.DB)
library(maps)

## general settings
network.file = "Network.RData"
log.file     = "Network.log.txt"

## Read PHP config file for webserver
php.config   = "config.php"
config = scan(php.config,what="character",sep="\n")
config = config[grep("^\\$",config)]  ## find lines that begin with $ (variables)
config = sub("$","",config,fixed = TRUE) ## remove $
config = sub(";","",config,fixed = TRUE) ## remove ;
config = sub("false","FALSE",config,fixed = TRUE) ##  Boolean capitalization
config = sub("true","TRUE",config,fixed = TRUE) ##  Boolean capitalization
config = config[-grep("$",config,fixed=TRUE)] ## lines with variable references fail
config = config[-grep("exec",config,fixed=TRUE)] ## lines 'exec' fail
config.list = eval(parse(text=paste('list(', paste0(config[1:14],collapse=","), ')'))) 

## Database connection
bety = list(user     = config.list$db_bety_username,
            password = config.list$db_bety_password,
            host     = config.list$db_bety_hostname,
            dbname   = config.list$db_bety_database)

## load previous state or initialize
if(file.exists(network.file)){
  load(network.file)
} else {
  
  ## ALSO NEED TO RUN THIS TO Make sure node info is up-to-date **** 
  
  ## get nodes from database
  con = db.open(bety)
  pecan.nodes = db.query("select * from machines where sync_host_id >= 0 order by sync_host_id",con)
  db.close(con)
  
  ## get lat/lon  (SLOW, ONLY RUN IF THINGS HAVE CHANGES)
  m = nrow(pecan.nodes)
  n = pecan.nodes$sync_host_id[m]+1
  pecan.geo = as.data.frame(matrix(NA,n,11))
  for(i in 1:m){
    hostname = sub("https://","",pecan.nodes$sync_url[i])
    hostname = sub("http://","",hostname)
    hostname = strsplit(hostname,split = "/")[[1]][1]
    hostname = strsplit(hostname,split=":")[[1]][1]
    result <- getURL(paste0("freegeoip.net/csv/",hostname))
    pecan.geo[pecan.nodes$sync_host_id[i]+1,] = strsplit(result,",")[[1]]
  }
  ## HACK FOR BNL
  pecan.geo[3,9:10] = c(40.868,-72.879)

  latest.schema = 0
  pecan.state = matrix(NA,n,n)
  schema.list = NULL
  node.schemas = rep(0,n)
  last.dump.size = rep(0,n)
  last.dump.time = rep(Sys.time(),n)
} ## end init

### FIRST PASS, GET NODE STATE
curr.schema = latest.schema
for(j in 1:m){
  i = pecan.nodes$sync_host_id[j]+1 ## row in vectors & matrices
  
  ## get version.txt
  sync.url = sub("bety.tar.gz","version.txt",pecan.nodes$sync_url[j])
  if(url.exists(sync.url)) {
    temporaryFile <- tempfile()
    download.file(sync.url,destfile=temporaryFile, method="curl")
    schema = scan(temporaryFile,what = "character") 
    unlink(temporaryFile)
    
    if(length(schema)>1){
      ## wasn't actually a version.txt file
      pecan.state[i,i] = 2    ## set status to DOWN
    } else {
      schema = as.numeric(schema[1])    
      
      ## look to see if the schema's NEW or already on the list
      if(schema %in% schema.list){
        if(schema == schema.list[latest.schema]){
          pecan.state[i,i] = 0 ## set status to UP-TO-DATE
        } else {
          pecan.state[i,i] = 1 ## set status to BEHIND
        }
      } else { ## new schema that's not on the list
        schema.list = c(schema.list,schema)
        latest.schema = length(schema.list)
      }
      ## check if the schema has been updated, if so log the event
      if(schema != schema.list[node.schemas[i]]){
        msg = paste(Sys.time(),"SCHEMA UPDATE DETECTED ON NODE",i-1,pecan.nodes$hostname[j],
                    "FROM",schema.list[node.schemas[i]],"TO",schema)
        write(msg,file=log.file,append=TRUE)
        node.schemas[i] = which(schema == schema.list)      
      }
    } ## end check length of version.txt
  } else { ## version.txt did not exist
    pecan.state[i,i] = 2    ## set status to DOWN
  }
  
  ## CHECK TO SEE IF THE SIZE OF THE DUMP HAS CHANGED
  bety.state = system(paste("curl -I -L",pecan.nodes$sync_url[j]),intern=TRUE)
  if(length(grep("404",bety.state[1]))){
    pecan.state[i,i] = 2    ## set status to DOWN
  } else {
    if(pecan.state[i,i]==2) pecan.state[i,i]=3
    bety.size = as.numeric(sub("Content-Length:","",bety.state[grep("Content-Length",bety.state)]))
    if(bety.size != last.dump.size[i]){ ## size has changed
      bety.time = sub("Last-Modified: ","",bety.state[grep("Last-Modified",bety.state)])
      bety.time=sub(" GMT\r","",bety.time)
      #bety.time=sub("GMT\r","0000",bety.time)
      bety.time = strptime(bety.time,"%a, %d %b %Y %T",tz="GMT")
      last.dump.time[i] = as.POSIXlt(bety.time)   
      last.dump.size[i] = bety.size
    }
  }
  
} ## end loop over nodes

## If schema changed, update statues
if(latest.schema > curr.schema){
  for(i in 1:n){
    if(pecan.state[i,i]==0 & node.schemas[i] < latest.schema) pecan.state[i,i]= 1
  }
}

## TO-DO NEXT
## modify load.bety.sh to write a log file of which nodes were synced when
## grab those logs from each server to construct a network of
##   A) what edges exist
##   B) when those edges are out of date (sync time < dump time)
## update figures to include edges
## update status page to include more node info and log messages.

for(j in 1:m){
  i = pecan.nodes$sync_host_id[j]+1 ## row in vectors & matrices
  
  ## get sync.log
  sync.url = sub("dump/bety.tar.gz","sync.log",pecan.nodes$sync_url[j])
  if(url.exists(sync.url) & length(grep("sync.log",sync.url))) {
    ## Parse sync file
    temporaryFile <- tempfile()
    download.file(sync.url,destfile=temporaryFile, method="curl")
    sync = scan(temporaryFile,what = "character",sep="\n") 
    unlink(temporaryFile)
    sync.time = sub("UTC ","",substr(sync,1,28))
    sync.time = strptime(sync.time,"%a %b %d %T %Y",tz="GMT")
    sync.stat = matrix(as.numeric(unlist(strsplit(substring(sync,30)," "))),ncol=2,byrow = TRUE)
    
    ## Do we need to reset all edges for a machine before updating, and if so where
    pecan.state[-i,i] = NA
    
    ## Loop over edges
    for(k in unique(sync.stat[,1])){
      ## find latest sync
      sel = which(sync.stat[,1] == k) ## choose node
      l = sel[which.max(as.POSIXct(sync.time[sel]))] ## latest
      if(sync.stat[l,2]>0) {
        pecan.state[k+1,i] = 2 ## FAILED
      } else {
        if(sync.time[l] > last.dump.time[i]){
          pecan.state[k+1,i] = 0 ## UP-TO-DATE
        } else {
          pecan.state[k+1,i] = 1 ## BEHIND
        }
      }
    }
    
  } ## end sync.log exists
} ## end loop over nodes (EDGE CHECK)

save.image(network.file)

rng.buffer <- function(x,b=0.1){
  x[1] = ifelse(x[1]<0,x[1]*(1+b),x[1]*(1-b))
  x[2] = ifelse(x[2]<0,x[2]*(1-b),x[2]*(1+b))
  x
}

## STATUS MAP
x = as.numeric(pecan.geo[,10])
y = as.numeric(pecan.geo[,9])
png(filename="NetworkStatus.png",width=1200)
colors = c("grey","green","yellow","red","purple")
status = diag(pecan.state)+2;status[is.na(status)]=1
xlim=rng.buffer(range(as.numeric(x),na.rm=TRUE))
ylim=rng.buffer(range(as.numeric(y),na.rm=TRUE))
map("world",xlim=xlim,ylim=ylim)
map("state",add=TRUE)
## EDGES THAT EXIST
for(i in 1:n){
  for(j in (1:n)[-i]){
    if(!is.na(pecan.state[i,j])){
      lines(x[c(i,j)],y[c(i,j)],col="grey",lty=3)
    }
  }
}
## EDGE STATE = OK
for(i in 1:n){
  for(j in (1:n)[-i]){
    if(!is.na(pecan.state[i,j]) & pecan.state[i,j]==0){
      arrows((x[i]+x[j])/2,(y[i]+y[j])/2,x[j],y[j],col="green",length=0.1,angle=15,lwd=2)
    }
  }
}
## EDGE STATE = BEHIND
for(i in 1:n){
  for(j in (1:n)[-i]){
    if(!is.na(pecan.state[i,j]) & pecan.state[i,j]==1){
      arrows((x[i]+x[j])/2,(y[i]+y[j])/2,x[j],y[j],col="yellow",length=0.1,angle=15,lwd=2)
    }
  }
}
## EDGE STATE = FAIL
for(i in 1:n){
  for(j in (1:n)[-i]){
    if(!is.na(pecan.state[i,j]) & pecan.state[i,j]>1){
      arrows((x[i]+x[j])/2,(y[i]+y[j])/2,x[j],y[j],col="red",length=0.1,angle=15,lwd=2)
    }
  }
}
## NODES
points(pecan.geo[,10:9],col=colors[status],pch=19,cex=3)
dev.off()


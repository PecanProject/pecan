in.path = "/Users/josh/Downloads/"
in.prefix = "AMF_USMOz"

setwd(in.path)
system2("wget","URL")

outfolder = "/Users/josh/temp/"

met2CF.Ameriflux(in.path,in.prefix,outfolder)
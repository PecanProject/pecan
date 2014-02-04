### R code from vignette source 'rhdf5.Rnw'

###################################################
### code chunk number 1: installation (eval = FALSE)
###################################################
## source("http://bioconductor.org/biocLite.R")
## biocLite("rhdf5")


###################################################
### code chunk number 2: createHDF5file
###################################################
library(rhdf5)
h5createFile("myhdf5file.h5")


###################################################
### code chunk number 3: create groups
###################################################
h5createGroup("myhdf5file.h5","foo")
h5createGroup("myhdf5file.h5","baa")
h5createGroup("myhdf5file.h5","foo/foobaa")
h5ls("myhdf5file.h5")


###################################################
### code chunk number 4: writeMatrix
###################################################
A = matrix(1:10,nr=5,nc=2)
h5write(A, "myhdf5file.h5","foo/A")
B = array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
attr(B, "scale") <- "liter"
h5write(B, "myhdf5file.h5","foo/B")
C = matrix(paste(LETTERS[1:10],LETTERS[11:20], collapse=""),
  nr=2,nc=5)
h5write(C, "myhdf5file.h5","foo/foobaa/C")
df = data.frame(1L:5L,seq(0,1,length.out=5),
  c("ab","cde","fghi","a","s"), stringsAsFactors=FALSE)
h5write(df, "myhdf5file.h5","df")
h5ls("myhdf5file.h5")
D = h5read("myhdf5file.h5","foo/A")
E = h5read("myhdf5file.h5","foo/B")
F = h5read("myhdf5file.h5","foo/foobaa/C")
G = h5read("myhdf5file.h5","df")


###################################################
### code chunk number 5: writeMatrixSubsetting
###################################################
h5createDataset("myhdf5file.h5", "foo/S", c(5,8), 
                storage.mode = "integer", chunk=c(5,1), level=7)
h5write(matrix(1:5,nr=5,nc=1), file="myhdf5file.h5", 
        name="foo/S", index=list(NULL,1))
h5read("myhdf5file.h5", "foo/S")
h5write(6:10, file="myhdf5file.h5",
        name="foo/S", index=list(1,2:6))
h5read("myhdf5file.h5", "foo/S")
h5write(matrix(11:40,nr=5,nc=6), file="myhdf5file.h5", 
        name="foo/S", index=list(1:5,3:8))
h5read("myhdf5file.h5", "foo/S")
h5write(matrix(141:144,nr=2,nc=2), file="myhdf5file.h5", 
        name="foo/S", index=list(3:4,1:2))
h5read("myhdf5file.h5", "foo/S")
h5write(matrix(151:154,nr=2,nc=2), file="myhdf5file.h5", 
        name="foo/S", index=list(2:3,c(3,6)))
h5read("myhdf5file.h5", "foo/S")
h5read("myhdf5file.h5", "foo/S", index=list(2:3,2:3))
h5read("myhdf5file.h5", "foo/S", index=list(2:3,c(2,4)))
h5read("myhdf5file.h5", "foo/S", index=list(2:3,c(1,2,4,5)))


###################################################
### code chunk number 6: writeMatrixHyperslab
###################################################
h5createDataset("myhdf5file.h5", "foo/H", c(5,8), storage.mode = "integer",
                chunk=c(5,1), level=7)
h5write(matrix(1:5,nr=5,nc=1), file="myhdf5file.h5", name="foo/H", 
        start=c(1,1))
h5read("myhdf5file.h5", "foo/H")
h5write(6:10, file="myhdf5file.h5", name="foo/H",
        start=c(1,2), count=c(1,5))
h5read("myhdf5file.h5", "foo/H")
h5write(matrix(11:40,nr=5,nc=6), file="myhdf5file.h5", name="foo/H", 
        start=c(1,3))
h5read("myhdf5file.h5", "foo/H")
h5write(matrix(141:144,nr=2,nc=2), file="myhdf5file.h5", name="foo/H", 
        start=c(3,1))
h5read("myhdf5file.h5", "foo/H")
h5write(matrix(151:154,nr=2,nc=2), file="myhdf5file.h5", name="foo/H",
        start=c(2,3), stride=c(1,3))
h5read("myhdf5file.h5", "foo/H")
h5read("myhdf5file.h5", "foo/H", 
       start=c(2,2), count=c(2,2))
h5read("myhdf5file.h5", "foo/H", 
       start=c(2,2), stride=c(1,2),count=c(2,2))
h5read("myhdf5file.h5", "foo/H", 
       start=c(2,1), stride=c(1,3),count=c(2,2), block=c(1,2))


###################################################
### code chunk number 7: h5save
###################################################
A = 1:7;  B = 1:18; D = seq(0,1,by=0.1)
h5save(A, B, D, file="newfile2.h5")
h5dump("newfile2.h5")


###################################################
### code chunk number 8: h5ls
###################################################
h5ls("myhdf5file.h5")
h5ls("myhdf5file.h5", all=TRUE)
h5ls("myhdf5file.h5", recursive=2)


###################################################
### code chunk number 9: h5dump
###################################################
h5dump("myhdf5file.h5",load=FALSE)
D <- h5dump("myhdf5file.h5")


###################################################
### code chunk number 10: h5dump (eval = FALSE)
###################################################
## system("h5dump myhdf5file.h5")


###################################################
### code chunk number 11: createfile
###################################################
library(rhdf5)
h5file = H5Fcreate("newfile.h5")
h5file


###################################################
### code chunk number 12: createfile
###################################################
h5group1 <- H5Gcreate(h5file, "foo")
h5group2 <- H5Gcreate(h5file, "baa")
h5group3 <- H5Gcreate(h5group1, "foobaa")
h5group3


###################################################
### code chunk number 13: createdataspace
###################################################
d = c(5,7)
h5space1 = H5Screate_simple(d,d)
h5space2 = H5Screate_simple(d,NULL)
h5space3 = H5Scopy(h5space1)
h5space4 = H5Screate("H5S_SCALAR")
h5space1
H5Sis_simple(h5space1)


###################################################
### code chunk number 14: create dataset
###################################################
h5dataset1 = H5Dcreate( h5file, "dataset1", "H5T_IEEE_F32LE", h5space1 )
h5dataset2 = H5Dcreate( h5group2, "dataset2", "H5T_STD_I32LE", h5space1 )
h5dataset1


###################################################
### code chunk number 15: writedata
###################################################
A = seq(0.1,3.5,length.out=5*7)
H5Dwrite(h5dataset1, A)
B = 1:35
H5Dwrite(h5dataset2, B)


###################################################
### code chunk number 16: closefile
###################################################
H5Dclose(h5dataset1)
H5Dclose(h5dataset2)

H5Sclose(h5space1)
H5Sclose(h5space2)
H5Sclose(h5space3)
H5Sclose(h5space4)

H5Gclose(h5group1)
H5Gclose(h5group2)
H5Gclose(h5group3)

H5Fclose(h5file)


###################################################
### code chunk number 17: sessioninfo
###################################################
sessionInfo()



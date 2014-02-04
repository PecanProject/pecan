##
## This uses numpy + h5py to construct some example h5 files. 
##
import h5py
from numpy import *
import os

FILE = "ex_1.h5"

if (os.path.exists(FILE)):
    os.remove(FILE)

## file 1
f = h5py.File(FILE)
g = f.create_group("group_1")
m = array([ random.normal(1, 1, 1000) for i in xrange(0, 10) ]).reshape(1000, 10)
d = g.create_dataset("ds_1", data = m, maxshape = (None, None))
s = array([ "".join(array(['A','C','G','T'])[random.randint(0, 4, i)]) for i in random.randint(1, 100, 20) ], 
          dtype = h5py.new_vlen(str))
ds = g.create_dataset("ds_2", data = s, maxshape = (None))

ds.attrs.create("x", array([1,2,3], dtype = "uint32"))
ds.attrs.create("y", array([[1,2,3], [5,6,7]], dtype = "uint32"))
ds.attrs.create("z", s)

a = random.randint(0, int(1e6), 3 * 7 * 9)
a = a.reshape((3, 7, 9))

g.create_dataset("ds_3", data = a, maxshape = (None, None, None))
g.create_dataset("ds_4", data = s.reshape((2, 10)))
g.create_dataset("ds_5", data = a.reshape(21, 9))
g.create_dataset("ds_6", data = random.randint(0, int(1e4), int(1e5)), dtype = "uint32")

a = random.rand(1000 * 10 * 5) * 10000
a = a.reshape(1000, 10, 5)
g.create_dataset("ds_7", data = a, maxshape = (None, None, None))

g.create_dataset("ds_8", data = array([[1,2,3,4,5], [6,7,8,9,10]], dtype = "uint32"))

## create some more intricate group structure.
h = g.create_group("group_2")
i = h.create_group("group_3")

j = f.create_group("group_4")
j.create_dataset("ds_1", data = a, maxshape = (None, None, None))

## create a 5-dimensional dataset for the hell of it.
a = random.randint(1, 10000, prod([10]*5)).reshape(tuple([10]*5))
g.create_dataset("ds_9", data = a, maxshape = tuple([None]*5))

## create a fixed-length string dataset.
g.create_dataset("ds_10", data = array(['rosalind', 'james', 'joseph', 'michael', 'rebecca']))

letterArray = array(['a', 'bb', 'ccc', 'dddd', 'eeeee', 'ffffff'])
g.create_dataset("ds_11", data = letterArray.reshape((2, 3)))

f.close()

## shrink the file for submission.
os.system("h5repack -v -f GZIP=7 %s %s-repacked" % (FILE, FILE))
os.system("mv %s-repacked %s" % (FILE, FILE))

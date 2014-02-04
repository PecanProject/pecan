from h5py import *
from numpy import *
import glob

uh5 = File("/scratch/u_big.h5")
zh5 = File("/scratch/z_big.h5")
x   = random.randint(0, 1e8, 1e8)

for s in ['1e3', '1e4', '1e5']:
    print s

    uh5.create_dataset("data_" + s, data = x, chunks = (int(double(s)),))
    zh5.create_dataset("data_" + s, data = x, chunks = (int(double(s)),), compression = 'gzip')

uh5.close()
zh5.close()





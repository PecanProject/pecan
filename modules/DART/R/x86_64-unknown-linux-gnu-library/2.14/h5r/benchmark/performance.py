from h5py import *
from numpy import *
import time

def f(ds, N):
    start = random.randint(0, len(ds), N)
    end   = start + random.exponential(1000, N) + 1
    end[end > len(ds)] = len(ds)
    
    for j in zip(start, end):
        z = ds[j[0]:j[1]]
    return True


def myTime(K, ds, N):
    res = [0]*K

    for i in range(0, K):
        s = time.time()
        f(ds, N)
        res[i] = time.time() - s
    return res

o = file('pyres.txt', 'w')

for h5 in [File("./u_big.h5"), File("./z_big.h5")]:
    for k in h5.keys():
        ds = h5[k]
        times = myTime(100, ds, N = 1000)
        o.write(k + " ")
        o.write(" ".join(map(str, times)))
        o.write("\n")

o.close()

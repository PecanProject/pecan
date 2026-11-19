"""Bounded anonymous memfd slots. No /dev/shm or disk-backed data files."""

import mmap, os
from multiprocessing.reduction import DupFd
import numpy as np


class Ring:
    def __init__(self, slots, columns, numeric_columns=67, maximum_rows=65536):
        self.slots = slots
        self.columns = columns
        self.numeric_columns = numeric_columns
        self.maximum_rows = maximum_rows
        self.x_bytes = maximum_rows * columns * 4
        self.numeric_bytes = maximum_rows * numeric_columns * 4
        self.vector_bytes = maximum_rows * 8

        self.stride = self.x_bytes + self.numeric_bytes + 2 * self.vector_bytes
        self.size = self.stride * slots
        self.fd = os.memfd_create("residual-input-ring", os.MFD_CLOEXEC)
        os.ftruncate(self.fd, self.size)
        self.memory = mmap.mmap(
            self.fd,
            self.size,
            flags=mmap.MAP_SHARED,
            prot=mmap.PROT_READ | mmap.PROT_WRITE,
        )

    def __reduce__(self):
        return _rebuild, (
            DupFd(self.fd),
            self.slots,
            self.columns,
            self.numeric_columns,
            self.maximum_rows,
        )

    def arrays(self, slot, n):
        if not 0 <= slot < self.slots or not 0 <= n <= self.maximum_rows:
            raise ValueError("Shared slot or row count out of range")
        start = slot * self.stride
        return dict(
            x=np.ndarray(
                (n, self.columns),
                np.float32,
                buffer=self.memory,
                offset=start,
                order="F",
            ),
            numeric=np.ndarray(
                (n, self.numeric_columns),
                np.float32,
                buffer=self.memory,
                offset=start + self.x_bytes,
            ),
            annual=np.ndarray(
                (n,),
                np.int64,
                buffer=self.memory,
                offset=start + self.x_bytes + self.numeric_bytes,
            ),
            year=np.ndarray(
                (n,),
                np.int64,
                buffer=self.memory,
                offset=start + self.x_bytes + self.numeric_bytes + self.vector_bytes,
            ),
        )

    def close(self):
        if getattr(self, "memory", None) is not None:
            self.memory.close()
            self.memory = None
        if getattr(self, "fd", None) is not None:
            os.close(self.fd)
            self.fd = None


def _rebuild(handle, slots, columns, numeric_columns, maximum_rows):
    ring = object.__new__(Ring)
    ring.slots = slots
    ring.columns = columns
    ring.numeric_columns = numeric_columns
    ring.maximum_rows = maximum_rows
    ring.x_bytes = maximum_rows * columns * 4
    ring.numeric_bytes = maximum_rows * numeric_columns * 4
    ring.vector_bytes = maximum_rows * 8
    ring.stride = ring.x_bytes + ring.numeric_bytes + 2 * ring.vector_bytes
    ring.size = ring.stride * slots
    ring.fd = handle.detach()
    ring.memory = mmap.mmap(
        ring.fd, ring.size, flags=mmap.MAP_SHARED, prot=mmap.PROT_READ | mmap.PROT_WRITE
    )
    return ring

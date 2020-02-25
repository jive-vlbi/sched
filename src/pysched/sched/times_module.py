from . import parameter
from .. import util

import schedlib as s

import numpy as np

def times(lst, start, stop, day, year):
    # the parameter LST is both a boolean flag (implemented by floats) 
    # and a string for the LST station
    use_lst = True
    if isinstance(lst, (float, int)):
        if lst == 0:
            lst = "VLA"
        else:
            use_lst = False
    elif isinstance(lst, str):
        lst = lst.upper()
    if use_lst:
        indices = np.argwhere(s.schcst.station == util.resize_string(
            lst, s.schcst.station.itemsize, "lst").encode())
        if len(indices) == 0:
            s.errlog("TIMES: LST specified but station not in catalog")

        s.schn1.lststa = indices[0] + 1
        s.schn1.lst = True
    else:
        s.schn1.lst = False

    for index in range(s.schn1.nscans):
        s.schn2a.duronly[index] = 0
        if s.schn2b.dur[index] > 0:
            s.schn2a.duronly[index] += 1
        if start[index] != parameter.unset:
            s.schn2a.duronly[index] += 2
        if stop[index] != parameter.unset:
            s.schn2a.duronly[index] += 4
        s.sattim(index+1, start[index], stop[index], day[index], year[index])

    s.schn1.iatutc = s.sla_dat(s.schn2c.startj[0])

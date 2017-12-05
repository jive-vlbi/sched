import schedlib as s

import numpy as np

def sttant(tantsta1, tantsta2):
    s.schn1.tants1[:s.schn1.nsta] = False
    s.schn1.tants2[:s.schn1.nsta] = False

    tantsta1 = set(tant.ljust(s.schc1.staname.itemsize).encode() 
                   for tant in tantsta1)
    tantsta2 = set(tant.ljust(s.schc1.staname.itemsize).encode() 
                   for tant in tantsta2)
    
    s.schn1.tants1 = [station in tantsta1 for station in s.schc1.staname]
    s.schn1.tants2 = [station in tantsta2 for station in s.schc1.staname]

    tant1or2 = np.logical_or(s.schn1.tants1, s.schn1.tants2)
    control = set(("SNAP", "SN50", "VEX", "NRAO"))
    for index in range(s.schn1.nsta):
        if (s.schn1.tants1[index] or s.schn1.tants2[index]) and \
           (s.schcst.control[s.schn1.stanum[index]-1].decode().strip() in 
            control):
            s.wlog(1, "SCHIN: TANT request for {} will be ignored.".format(
                s.schcst.station[s.schn1.stanum[index]-1].decode().strip()))
    if not (s.schn1.tants1[:s.schn1.nsta].any() or \
            s.schn1.tants2[:s.schn1.nsta].any()):
        mask = (s.schc1.staname[:s.schn1.nsta] == 
                "EFLSBERG".ljust(s.schc1.staname.itemsize).encode())
        s.schn1.tants1[:s.schn1.nsta][mask] = True

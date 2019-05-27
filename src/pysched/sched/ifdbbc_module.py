import schedlib as s

import numpy as np

def ifdbbc(dbbc_version, e_firmware):
    if s.schcon.debug:
        s.wlog(1, "IFDBBC: Starting")
    max_bbc = 16
    max_if = 4
    ifbbc = np.zeros((max_bbc, max_if))
    if dbbc_version == "ASTRO":
        mif = 4
        for if_ in range(mif):
            ifbbc[if_ * 4: (if_+1) *4, if_] = 1
    elif dbbc_version == "GEO":
        mif = 2
        for if_ in range(mif):
            ifbbc[if_ * 8: (if_+1) *8, if_] = 1
    elif dbbc_version == "HYBRID":
        mif = 3
        ifbbc[0:4, 0] = 1
        ifbbc[4:8, 1] = 1
        ifbbc[8:16, 2] = 1
    else:
        s.wlog(1, "IFDBBC: DBBCVER not recognised: {}".format(dbbc_version))
        s.errlog("catalog error")

    if e_firmware: 
        # 32 MHz (only in E) DDC mode can only do odd (count starting at 1) BBCs
        ifbbc[1::2,:] = 0
    return ifbbc, mif

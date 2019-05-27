import schedlib as s

import numpy as np

def ifdbbc3():
    if s.schcon.debug:
        s.wlog(1, "IFDBBC3: Starting")
    max_bbc = 64
    max_if = 8
    ifbbc = np.zeros((max_bbc, max_if))
    for if_ in range(max_if):
        ifbbc[if_ * 8: (if_+1) * 8, if_] = 1
    return ifbbc, max_if

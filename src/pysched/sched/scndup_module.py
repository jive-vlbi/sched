import schedlib as s

import copy

def scndup(entries, to, from_, copyall, caller):
    if s.schcon.debug:
        s.wlog(0, "SCNDUP: Duplicating scan {} to scan {}.  Called by: {} "
               "copyall = {}".format(from_+1, to+1, caller, copyall))
        s.wlog(0, "SCNDUP: ANNOT: {}".format(entries[from_].annot))
    if to > len(entries):
        s.errlog("SCHDUP: Output scan number {} too big for arrays of "
                 "dimension: {}".format(to+1, len(entries)))
    entries[to] = copy.deepcopy(entries[from_])
    s.schn2a.nsetup[to, :] = s.schn2a.nsetup[from_, :]
    s.schn2a.fseti[to, :] = s.schn2a.fseti[from_, :]
    s.schn2a.stascn[to, :] = s.schn2a.stascn[from_, :]
    s.schn2b.dopincr[to, :] = s.schn2b.dopincr[from_, :]
    if not copyall:
        # reset some entries
        entries[to].duronly = 1
        entries[to].annot = ""
    else:
        # copy station entries
        s.schn5.tpstart[to, :] = s.schn5.tpstart[from_, :]
        s.schn5.tcorr[to, :] = s.schn5.tcorr[from_, :]
        s.schn5.gbytes[to, :] = s.schn5.gbytes[from_, :]
        s.schn6.lst1[to, :] = s.schn6.lst1[from_, :]
        s.schn6.lst2[to, :] = s.schn6.lst2[from_, :]
        s.schn6.tonsrc[to, :] = s.schn6.tonsrc[from_, :]
        s.schn6.tslew[to, :] = s.schn6.tslew[from_, :]
        s.schn6.el1[to, :] = s.schn6.el1[from_, :]
        s.schn6.az1[to, :] = s.schn6.az1[from_, :]
        s.schn6.ha1[to, :] = s.schn6.ha1[from_, :]
        s.schn6.pa1[to, :] = s.schn6.pa1[from_, :]
        s.schc6.up1[to, :] = s.sccn6.up1[from_, :]
        s.schn6.el2[to, :] = s.schn6.el2[from_, :]
        s.schn6.az2[to, :] = s.schn6.az2[from_, :]
        s.schn6.ha2[to, :] = s.schn6.ha2[from_, :]
        s.schn6.pa2[to, :] = s.schn6.pa2[from_, :]
        s.schc6.up2[to, :] = s.schc6.up2[from_, :]
        

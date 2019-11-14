from . import scndup
from ..catalog import ScanCatalog, StationCatalog
from ..util import f2str

import schedlib as s

import numpy as np

in_scan = 0
def optupt(last_scan_index, k_scan, scan_index):
    global in_scan

    done = False
    keep = True
    adjust = False
    
    stations = StationCatalog().used(use_direct_access=True)
    scans = ScanCatalog().direct_access_entries
    
    if k_scan == 1:
        s.wlog(0, "OPTUPT:  OPTMODE=UPTIME was specified.  Station files "
               "(sch, crd, ")
        s.wlog(0, "         vex, and drudg) will not be written.")

        in_scan = 1
        scans[scan_index - 1].startj = scans[0].startj
        if s.schcon.opdur <= 0:
            s.errlog(" OPTCUPT: For OPTMODE=UPTIME, OPDUR must be given.")

        s.wlog(0, "         There are {} input scans with sources: ".format(
            s.schn1.nscans))
        sources = set()
        for j_scan, scan in enumerate(scans[:s.schn1.nscans]):
            s.wlog(0, "     {:>5d}: {}".format(j_scan + 1, scans[j_scan].scnsrc))
            sources.add(scans[j_scan].scnsrc)

        if len(sources) < s.schn1.nscans:
            s.wlog(1, "       There were duplicate sources. ")
            s.wlog(1, "       This is probably not what you wanted for "
                   "OPTMODE=UPTIME.")
    else:
        scans[scan_index - 1].startj = scans[scan_index - 2].stopj + \
                                       scans[in_scan - 1].gap

    scans[scan_index - 1].stopj = scans[scan_index - 1].startj + \
                                  scans[in_scan - 1].dur
    if scans[scan_index - 1].stopj > scans[0].startj + s.schcon.opdur:
        if in_scan >= s.schn1.nscans:
            done = True
            keep = False
        else:
            in_scan += 1
            scans[scan_index - 1].startj = scans[0].startj
            scans[scan_index - 1].stopj = scans[scan_index - 1].startj + \
                                          scans[in_scan - 1].dur
            last_scan_index.fill(0)

    if not done:
        scndup(scan_index - 1, in_scan - 1, False, "OPTUPT")

        for i, station in enumerate(stations):
            if station.stascn[scan_index - 1]:
                s.stageo(scan_index, i + 1, scans[scan_index - 1].startj, 0, 
                         "OPTUPT")
                station.stascn[scan_index - 1] = \
                    (f2str(station.up1[scan_index - 1]) == "") and \
                    (f2str(station.up2[scan_index - 1]) == "") and \
                    ((station.el1[scan_index - 1] + 
                      station.el2[scan_index - 1]) / 2
                     > scans[scan_index - 1].opminel)

    return last_scan_index, adjust, keep, done

from . import scndup
from ..catalog import ScanCatalog, StationCatalog
from ..util import f2str

import schedlib as s

import numpy as np

# According to comments in SCHED, the Fortran code for which this is a 
# translation, has bugs.
# Indeed, the code doesn't make sense (particularly, many uninitialized 
# variables, where even initialization to 0 doesn't make sense).
# This translation reflects those problems ...

ns1 = ns2 = ns3 = scan1 = scan2 = scan3 = 0
scan_station = np.zeros(dtype=int, shape=(StationCatalog.maxsta,))
def optcsub(last_scan_index, k_scan, scan_index):
    global ns1, ns2, ns3, scan1, scan2, scan3, time1, time2, time3, scan_station

    if s.schcon.debug:
        s.wlog(1, "OPTCSUB starting")

    done = False
    keep = True
    adjust = True
    
    stations = StationCatalog().used(use_direct_access=True)
    scans = ScanCatalog().direct_access_entries
    
    if k_scan == 1:
        if s.schcon.opnosub:
            s.errlog("OPTCSUB: Mode CSUB and OPNOSUB do not make sense "
                     "together.")

        adjust = False
        scans[scan_index - 1].startj = scans[0].startj
        if s.schcon.opdur <= 0:
            s.errlog(" OPTCSUB: For OPTMODE=CSUB, OPDUR must be given.")

    if (ns1 < 2) and (ns2 < 2) and (ns3 < 2):
        scan_station, ns1, ns2, ns3, scan1, scan2, scan3, time1, time2, time3 = \
            s.optcsar(last_scan_index, k_scan, scan_index, scan_station,
                      ns1, ns2, ns3, scan1, scan2)

    j_scan = 0
    scan = scans[scan_index - 1]
    if ns1 >= 2:
        j_scan = scan1
        scan.startj = time1
        ns1 = 0
    elif ns2 >= 2:
        j_scan = scan2
        scan.startj = time2
        ns2 = 0
    elif ns3 >= 2:
        j_scan = scan3
        scan.startj = time3
        ns3 = 0
    
    if j_scan == 0:
        s.errlog("OPTCSUB: No data for scan {}".format(scan_index))

    scndup(scan_index - 1, j_scan - 1, False, "OPTCSUB")

    scan.stopj = scan.startj + scan.dur

    for i, station in enumerate(stations):
        station.stascn[scan_index - 1] = (j_scan == scan_station[i])

    return adjust, keep, done

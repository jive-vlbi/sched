from . import scndup
from ..catalog import ScanCatalog, StationCatalog, SourceCatalog
from ..util import f2str

import schedlib as s

import numpy as np

skip = np.empty(dtype=int, shape=(SourceCatalog.maxsource,))
missed = 0
def optskd(last_scan_index, k_scan, scan_index):
    global skip, missed

    if k_scan > s.schn1.nscans:
        return False, False, True
    
    if k_scan != scan_index:
        scndup(scan_index, k_scan, True, "OPTSKD")

    scans = ScanCatalog().direct_access_entries
    stations = StationCatalog().used(use_direct_access=True)
    scan = scans[scan_index - 1]
    
    adjust = (scan.duronly == 1)

    if scan_index == 1:
        skip.fill(s.schcon.opskip)
        missed = 0

    if not adjust:
        approx_time = scan.startj
    else:
        approx_time = s.schn1.tfirst
        for i, station in enumerate(stations):
            if station.stascn[scan_index - 1] and (last_scan_index[i] != 0):
                approx_time = max(approx_time, 
                                  scans[last_scan_index[i] - 1].stopj + scan.gap)

    n_good = 0
    n_prio = 0
    begin_time = s.schn1.tfirst
    ok_sta = np.full(fill_value=False, dtype=bool, shape=(len(stations,)))
    for i, station in enumerate(stations):
        if station.stascn[scan_index - 1]:
            last_time, avail_time = s.stageo(scan_index, i + 1, approx_time, 
                                             last_scan_index[i], "OPTSKD")
            if (f2str(station.up1[scan_index - 1]) == "") and \
               (f2str(station.up2[scan_index - 1]) == "") and \
               (station.el1[scan_index - 1] > scan.opminel) and \
               (station.el2[scan_index - 1] > scan.opminel):
                ok_sta[i] = True
                n_good += 1
                if last_scan_index[i] != 0:
                    avail_time = max(
                        avail_time, 
                        scans[last_scan_index[i] - 1].stopj + scan.gap)
                    begin_time = max(begin_time, avail_time)
                opelprio = s.schcon.opelprio
                if (opelprio[0] < station.el1[scan_index - 1] < opelprio[1]) or \
                   (opelprio[2] < station.el1[scan_index - 1] < opelprio[3]):
                    n_prio += 1
    
    if (not adjust) or (begin_time == s.schn1.tfirst):
        begin_time = approx_time

    source_index = scan.srcnum

    keep = (n_good >= max(scan.opmian, 1)) and \
           ((skip[source_index - 1] >= s.schcon.opskip) or (n_prio >= 1))
    if f2str(s.schsco.obstyp) not in {"NONE", "PTVLBA"}:
        keep = keep or ((n_good >= 1) and 
                        (scan.pntvlba or scan.tanvlba or scan.dopn3db))
    keep = keep and (missed >= scan.opmiss)
    
    if keep:
        skip[source_index - 1] = 0
        if adjust:
            scan.startj = begin_time
            scan.stopj = begin_time + scan.dur

        for i, station in enumerate(stations):
            station.stascn[scan_index - 1] = (ok_sta[i] or s.schcon.opnosub)

        missed = 0
    else:
        if n_good >= scan.opmian:
            skip[source_index - 1] += 1

        for station in stations:
            station.stascn[scan_index - 1] = False

        missed += 1
        
    return adjust, keep, False

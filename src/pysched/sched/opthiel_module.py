from . import scndup
from ..catalog import ScanCatalog, StationCatalog, SourceCatalog
from ..util import f2str

import schedlib as s

import numpy as np

skip = np.empty(dtype=int, shape=(SourceCatalog.maxsource))
missed = 0
def opthiel(last_scan_index, k_scan, scan_index):
    global skip, missed

    done = False
    keep = True
    adjust = True
    
    stations = StationCatalog().used(use_direct_access=True)
    scans = ScanCatalog().direct_access_entries
    
    if k_scan > s.schn1.nscans:
        done = True
        return k_scan, adjust, keep, done

    scan = scans[k_scan - 1]
    scan.higroup = max(scan.higroup, 1)
    if k_scan + scan.higroup - 1 > s.schn1.nscans:
        s.errlog("OPTHIEL: HIGROUP in last scan too large.")

    if scan.higroup == 1:
        scndup(scan_index - 1, k_scan - 1, True, "OPTHIEL")
        return k_scan, adjust, keep, done

    approx_time = s.schn1.tfirst
    for i, station in enumerate(stations):
        use_station = np.any(station.stascn[k_scan - 1: 
                                            k_scan - 1 + scan.higroup])
        if use_station and (last_scan_index[i] != 0):
            approx_time = max(approx_time, scans[last_scan_index[i] - 1].stopj +
                              scans[scan_index - 1].gap)

    group_max_low_el = -100
    for j in range(scan.higroup):
        j_k_scan = k_scan - 1 + j
        j_scan_index = scan_index - 1 + j
        scndup(j_scan_index, j_k_scan, True, "OPTHIEL")
        scan_low_el = 100
        for i, station in enumerate(stations):
            if station.stascn[j_scan_index]:
                s.stageo(j_scan_index + 1, i + 1, approx_time, 
                         last_scan_index[i], "OPTHIEL")
                average_el = (station.el1[j_scan_index] + 
                              station.el2[j_scan_index]) / 2
                scan_low_el = min(scan_low_el, average_el)
                
        if scan_low_el > group_max_low_el:
            group_max_low_el = scan_low_el
            max_scan = j_scan_index

    scndup(scan_index - 1, max_scan, True, "OPTHIEL")

    k_scan += scan.higroup - 1
    
    return k_scan, adjust, keep, done

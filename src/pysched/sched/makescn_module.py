from . import scndup
from ..catalog import ScanCatalog, StationCatalog
from ..util import f2str

import schedlib as s

import numpy as np

scan_catalog = ScanCatalog()
station_catalog = StationCatalog()
def makescn(last_scan_index, scan_index, j_scan, source_index, source_name, 
            approx_time, min_elevation, use_time):
    if s.schcon.debug:
        s.wlog(0, "MAKESCN starting.")

    scndup(scan_index - 1, j_scan - 1, False, "MAKESCN")
    
    scans = scan_catalog.direct_access_entries
    scans[scan_index - 1].srcnum = source_index
    scans[scan_index - 1].scnsrc = source_name.ljust(s.schc2b.scnsrc.itemsize)

    stations = station_catalog.used(use_direct_access=True)

    n_good = 0
    ok_sta = np.full(fill_value=False, dtype=bool, shape=(len(stations),))
    for i, station in enumerate(stations):
        if station.stascn[j_scan - 1]:
            last_time, available_time = s.stageo(scan_index, i + 1, approx_time,
                                                 last_scan_index[i], "MAKESCN")
            if (f2str(station.up1[scan_index - 1]) == "") and \
               (f2str(station.up2[scan_index - 1]) == "") and \
               (station.el1[scan_index - 1] > min_elevation) and \
               (station.el2[scan_index - 1] > min_elevation):
                ok_sta[i] = True
                n_good += 1
    
    for i, station in enumerate(stations):
        station.stascn[scan_index - 1] = station.stascn[scan_index - 1] and \
                                         ok_sta[i]

    if n_good >= 1:
        scans[scan_index - 1].startj = approx_time

        s.opttim(last_scan_index, last_scan_index, scan_index, True, use_time, 
                 False)
        n_good = s.scngeo(last_scan_index, scan_index)

    return n_good

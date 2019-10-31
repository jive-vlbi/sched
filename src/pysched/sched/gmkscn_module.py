from . import scndup
from .parameter import secpday
from ..catalog import StationCatalog, ScanCatalog
from ..util import f2str

import schedlib as s

import numpy as np

def gmkscn(last_scan_index, scan_index, j_scan, source_index, source_name,
           approx_time, min_elevation, keep_station, scan_stascn, g_mode):
    if s.schcon.debug:
        s.wlog(0, "GMKSCN starting.")

    scan_catalog = ScanCatalog()
    station_catalog = StationCatalog()
    
    scans = scan_catalog.direct_access_entries
    stations = station_catalog.used(use_direct_access=True)

    n_good = 0

    # go to python/zero indexing for scan index
    scan_index -= 1
    j_scan -= 1

    scndup(scan_index, j_scan, False, "GMKSCN")

    scan = scans[scan_index]

    scan.srcnum = source_index
    scan.scnsrc = source_name

    ok_sta = np.empty(dtype=bool, shape=(len(stations),))
    if g_mode == "SET":
        for i, station in enumerate(stations):
            ok_sta[i] = False
            if station.stascn[j_scan]:
                last_time, available_time = s.stageo(
                    scan_index + 1, i + 1, approx_time, last_scan_index[i], 
                    "GMKSCN")
                if (f2str(station.up1[scan_index]) == "") and \
                   (f2str(station.up2[scan_index]) == "") and \
                   (station.el1[scan_index] > min_elevation) and \
                   (station.el2[scan_index] > min_elevation):
                    ok_sta[i] = True
                    n_good += 1

        for i, station in enumerate(stations):
            station.stascn[scan_index] = station.stascn[j_scan] and ok_sta[i]

        if n_good >= 1:
            scan.startj = approx_time
            s.opttim(last_scan_index, last_scan_index, scan_index + 1, 
                     True, False, False)
            n_good = s.scngeo(last_scan_index, scan_index + 1)

        if (n_good >= scans[j_scan].opmian) and (scan_index + 1 > s.schn1.scan1):
            in_scan = [s for s in stations if s.stascn[scan_index]]
            in_scan.sort(key=lambda s: -s.tonsrc[scan_index])
            if len(in_scan) > 0:
                t_med = in_scan[min(2, len(in_scan) - 1)].tonsrc[scan_index]
            else:
                s.errlog("GMKSCN: INSCN zero.  Programming error")

            for i, station in enumerate(stations):
                if station.stascn[scan_index] and \
                   (i + 1 != keep_station) and \
                   (last_scan_index[i] != 0) and \
                   (station.tonsrc[scan_index] > 
                    t_med + s.schsou.geoslow / secpday):
                    if station.el1[scan_index] < s.schsou.geolowel:
                        n_good = 0
                        if s.schsou.geoprt >= 2:
                            s.wlog(0, "**gmkscn: Dropping scan - low el for "
                                   "slow ant {} {}  {} {}".format(
                                       i + 1, scan_index + 1, source_name, 
                                       station.el1[scan_index]))
                    else:
                        n_good = max(n_good - 1, 0)
                        station.stascn[scan_index] = False
                        ok_sta[i] = False
                        if s.schsou.geoprt >= 2:
                            s.wlog(0, "++gmkscn: Dropping station {}  Scan {} {}"
                                   "  Geosrc: {} for long slew.".format(
                                       i + 1, scan_index + 1, s.schn1.scan1, 
                                       source_name))

        scan_stascn[:] = [s.stascn[scan_index] for s in stations]

    elif g_mode == "FORCE":
        for i, station in enumerate(stations):
            station.stascn[scan_index] = scan_stascn[i]
        ok_sta[:] = scan_stascn[:ok_sta.shape[0]]
        n_good = np.count_nonzero(ok_sta)

        if n_good >= 1:
            scan.startj = approx_time
            s.opttim(last_scan_index, last_scan_index, scan_index + 1, 
                     True, False, False)
            n_good = s.scngeo(last_scan_index, scan_index + 1)
    else:
        s.errlog("GMKSCN: Bad GMODE.  Programming error.")

    return n_good, ok_sta, scan_stascn
          

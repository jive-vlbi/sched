from . import geomake, gmkscn
from .parameter import max_seg, secpday
from ..util import f2str
from ..catalog import StationCatalog, ScanCatalog

import schedlib as s

import numpy as np

geo_stascn = np.full(fill_value=False, shape=(max_seg, s.schn2a.stascn.shape[1]))
geo_startj = np.empty(dtype=float, shape=(max_seg,))

n_seg = 0

def addgeo(last_scan_index, scan_index, geo_opt, scans, stations):
    global j_scan, n_seg, seg_sources, geo_stascn, geo_startj

    if s.schcon.debug:
        s.wlog(0, "ADDGEO starting.")

    if geo_opt == 0:
        j_scan = scans[scan_index - 1].geoiscn
        seg_sources, n_seg, geo_stascn, geo_startj = geomake(
            last_scan_index, j_scan, scan_index, n_seg, geo_stascn, geo_startj,
            stations, scans)
        geo_opt = n_seg

    if geo_opt != 0:
        seg_index = n_seg - geo_opt
        scan_stascn = geo_stascn[seg_index, :]
        approx_time = geo_startj[seg_index]
        seg_source_index = seg_sources[seg_index]
        
        n_good, ok_sta, scan_stascn = gmkscn(
            last_scan_index, scan_index, j_scan, 
            s.schsou.geosrci[seg_source_index - 1], 
            s.schcsc.geosrc[seg_source_index - 1], 
            approx_time, scans[j_scan - 1].opminel, 0, scan_stascn, "FORCE")

        geo_opt -= 1
        scan = scans[scan_index - 1]
        scan.origen = 3
        if s.schsou.geoprt >= 0:
            if scan_index == s.schn1.scan1:
                s_gap = 0.
            else:
                s_gap = (scan.startj - scans[scan_index - 2].stopj) * secpday

            msg = "{:4d} {:8.0f} {:<12}".format(
                seg_source_index, s_gap, 
                f2str(s.schcsc.geosrc[seg_source_index - 1]))
            msg += " ".join(" {:4.0f} ".format(s.el1[scan_index - 1]) 
                            if s.stascn[scan_index - 1]
                            else "({:4.0f})".format(s.el1[scan_index - 1]) 
                            for s in stations[:20])
            s.wlog(0, msg)

    return geo_opt, True

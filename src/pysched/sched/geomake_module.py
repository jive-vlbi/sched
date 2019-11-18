from .parameter import max_seg, secpday
from . import makeseg, geochk
from ..util import f2str_array, f2str

import schedlib as s

import numpy as np

dummy_index = -12345

def geomake(last_scan_index, j_scan, scan_index, n_seg, geo_stascn, geo_startj,
            stations, scans):
    global dummy_index

    sigma = np.empty(shape=(len(stations) * 2,), dtype=float)
    best_sigma = np.empty(shape=(len(stations),), dtype=float)
    if s.schcon.debug or (s.schsou.geoprt >= 2):
        s.wlog(1, "GEOMAKE starting.")

    n_print_stations = min(20, len(stations))

    seg_sources = np.zeros(shape=(max_seg,), dtype=int)
    
    kept = False

    for _ in range(100):
        dummy_index, _ = s.ran5wrap(dummy_index)
    
    start_time = 0
    for station_index, station in enumerate(stations):
        if station.stascn[j_scan - 1]:
            last_station_scan_index = last_scan_index[station_index]
            if last_station_scan_index != 0:
                start_time = max(start_time, 
                                 scans[last_station_scan_index - 1].stopj)
            else:
                start_time = max(start_time, s.schn1.tfirst)

    scan = scans[scan_index - 1]
    end_time = start_time + scan.geolen
    if s.schsou.geoprt >= 2:
        s.wlog(0, "GEOMAKE START TIMES {} {} {}".format(
            start_time, scan.startj, end_time))

    scan = scans[j_scan - 1]
    if scan.opmian < 2:
        s.wlog(1, "GEOMAKE:  OPMINANT of {} too small for automatic insertion "
               "of geodetic sections.".format(scan.opmian))
        scan.opmian = len(stations) // 2
        s.wlog(1, "          Resetting to OPMINANT = {}".format(scan.opmian))
    
    ok_geo, use_geo, seg_elevation = geochk(
        j_scan, scan_index, start_time, end_time)
    
    best_quality = 9999
    for trial in range(1, s.schsou.geotries + 1):
        if s.schsou.geoprt >= 1:
            s.wlog(0, "GEOMAKE starting to construct trial segment {}.  "
                   "First scan: {}".format(trial, scan_index))

        last_scan, n_scan_seg, test_seg_sources, dummy_index, selection_type, \
            worst_station = makeseg(
                j_scan, scan_index, last_scan_index, ok_geo, use_geo, 
                seg_elevation, start_time, end_time, dummy_index, sigma,
                stations, scans)
        min_scans = min(np.count_nonzero(
            station.stascn[scan_index - 1: last_scan])
                        for station in stations)

        retain = (min_scans >= 3)

        if retain:
            if s.schsou.geoprt >= 2:
                n_prt = min(20, n_scan_seg)
                if n_scan_seg > n_prt:
                    print(" Showing first {} scans of {}".format(
                        n_prt, n_scan_seg))
                s.wlog(0, "GEOMAKE ---------------------------------- ")
                s.wlog(0, "GEOMAKE    FINISHED ONE SEQUENCE - Trial: {}  "
                       "Geosrcs: {}".format(trial, " ".join(
                           "{:4d}".format(s) for s in test_seg_sources[:n_prt]
                       )))

            print_debug = (s.schsou.geoprt >= 2)
            test_quality, sigma = s.geoqual(scan_index, scan_index, last_scan, 
                                            j_scan, 14.48, print_debug, sigma)

            if s.schsou.geoprt >= 0:
                s.wlog(0, " ")
                s.wlog(0, "GEOMAKE: Finished making trial segment. "
                       "{} Number of scans: {}  Quality: {:.2f}  "
                       "Previous best: {:.2f}".format(
                           trial, n_scan_seg, test_quality, best_quality))

                if n_scan_seg > 50:
                    s.wlog(1, "  Only first 50 scans printed")
                    n_prt = 50
                else:
                    n_prt = n_scan_seg
                
                s.wlog(0, "     Sources:   {}".format(" ".join(
                    "{:3d}".format(s) for s in test_seg_sources[:n_prt])))
                s.wlog(0, "     Priority:  {}".format(" ".join(
                    "{:3d}".format(use_geo[s - 1]) 
                    for s in test_seg_sources[:n_prt])))
                s.wlog(0, "     Sigmas by station: {}".format(" ".join(
                    "{:.1f}".format(s) for s in sigma[:n_print_stations])))

            if test_quality < best_quality:
                if s.schsou.geoprt == 0:
                    s.wlog(0, " ")
                
                s.wlog(0, "New best geodetic segment - Trial: {} "
                       "Number of scans & fewest scans/sta: {} {}  "
                       "Quality: {:.3f}".format(
                           trial, n_scan_seg, min_scans, test_quality))

                best_quality = test_quality
                seg_sources[:n_scan_seg] = test_seg_sources[:n_scan_seg]
                geo_startj[:n_scan_seg] = \
                    [scan.startj for scan in scans[scan_index - 1: 
                                                   scan_index + n_scan_seg - 1]]
                best_sigma[:] = sigma[:best_sigma.shape[0]]
                for station_index, station in enumerate(stations):
                    geo_stascn[:n_scan_seg, station_index] = station.stascn[
                        scan_index - 1: scan_index + n_scan_seg - 1]

                n_seg = n_scan_seg
                kept = True
                
        else:
            if s.schsou.geoprt >= 0:
                s.wlog(0, "GEOMAKE: Segment not kept.  "
                       "Not enough scans at some stations.")


        if ((s.schsou.geoprt >= 0) and kept) or (s.schsou.geoprt >= 1):
            if retain:
                s.wlog(0, "   Sigmas by station:                {}".format(
                    " ".join("{:.2f}".format(s) for s in sigma[:n_print_stations]
                         )))
            else:
                s.wlog(0, "  Rejected without testing.  "
                       "Some station(s) with too few scans.")

            s.wlog(0, " Num  Gap(s) Sel Source      Worst {}".format(
                "".join("{:^7}".format(station.stcode) 
                        for station in stations[:n_print_stations])))
            for i in range(n_scan_seg):
                j = scan_index + i - 1
                if (i == 0) and (scan_index == s.schn1.scan1):
                    s_gap = 0
                else:
                    s_gap = (scans[j].startj - scans[j - 1].stopj) * secpday

                seg_source_index = test_seg_sources[i]
                source = f2str(s.schcsc.geosrc[seg_source_index - 1])
                elevation_text = " ".join(
                    " {:>4.0f} ".format(station.el1[j]) if station.stascn[j] 
                    else "({:>4.0f})".format(station.el1[j]) 
                    for station in stations[:n_print_stations])
                s.wlog(0, "{:4d} {:>7.0f} {:<3} {:<12}  {} {}".format(
                    test_seg_sources[i], s_gap, selection_type[i], source, 
                    worst_station[i], elevation_text))
                
        kept = False
        
        if trial % 10 == 0:
            s.wlog(1, "Finished trial geodetic segment number {}".format(trial))

    # end of trial loop
    
    if s.schsou.geoprt >= 0:
        n_prt = min(30, n_scan_seg)
        s.wlog(0, "   ")
        if n_scan_seg > n_prt:
            s.wlog(0, " Showing first {} scans of {}".format(n_prt, n_scan_seg))
        else:
            s.wlog(0, " ")
        
        s.wlog(0, "GEOMAKE FINISHED: {} Selected geodetic sources: {}".format(
            n_seg, " ".join("{:4d}".format(s) for s in seg_sources[:n_prt])))

    if s.schsou.geoprt < 0:
        s.wlog(0, "                                     " + 
               " ".join("{:^8}".format(station.stcode) 
                        for station in stations[:n_print_stations]))

    sigmas_text = " ".join("{:.2f}".format(s) 
                          for s in best_sigma[:n_print_stations])
    s.wlog(1, " Quality: {:.2f}  Sigmas by station: {}".format(
        best_quality, sigmas_text))

    if s.schsou.geoprt >= 0:
        stations_text = "".join("{:^7}".format(station.stcode) 
                                for station in stations[:n_print_stations])
        s.wlog(0, " Num   Gap(s)  Source    " + stations_text)

    return seg_sources, n_seg, geo_stascn, geo_startj

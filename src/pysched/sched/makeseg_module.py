from . import gmkscn
from .parameter import max_seg, secpday
from ..catalog import StationCatalog
from ..util import f2str, bool2str

import schedlib as s

import numpy as np

import math

def makeseg(j_scan, scan_index, last_scan_index, ok_geo, use_geo, seg_elevation, 
            start_time, end_time, dummy_index, sigma, stations, scans):
    
    if s.schcon.debug:
        s.wlog(1, "MAKESEG starting")

    n_sta = len(stations)
    n_print = min(20, n_sta)

    max_geo = s.schsou.geosrci.shape[0]
    selected = np.zeros(dtype=int, shape=(max_geo,))

    t_src = np.zeros(dtype=int, shape=(max_seg,))
    selection_type = np.full(fill_value="", dtype=object, shape=(max_seg,))
    worst_station = np.full(fill_value="", dtype=object, shape=(max_seg,))
    in_scan = np.zeros(dtype=int, shape=(n_sta,))
    station_low = np.zeros(dtype=int, shape=(n_sta,))
    station_high = np.zeros(dtype=int, shape=(n_sta,))
    n_station_low = 0
    n_station_high = 0
    low_high_goal = 1
    last_high_station = 0
    last_high_sigma = 0

    # otherwise used uninitialized in log output ...
    high_station = 0
    high_sigma = 0
    save_sigma1 = np.copy(sigma)
    save_sigma2 = np.copy(sigma)
    save_sigma3 = np.copy(sigma)
    high_station1 = 0
    high_station2 = 0
    high_station3 = 0

    last_scan_index_copy = np.copy(last_scan_index)

    scans[scan_index - 1].startj = start_time

    if s.schsou.geoprt >= 1:
        s.wlog(0, "".join("{:^6}".format(s.stcode) for s in stations[:n_print]))

    seg_index = 0
    do_rand = True
    nt_seg = 0
    max_prio = 6

    scan_stascn = np.empty(dtype=bool, shape=(n_sta,))
    k_stascn = np.empty(dtype=bool, shape=(max_geo, n_sta))

    chance = np.empty(dtype=int, shape=(max_seg * 10,))
    test_time = np.empty(dtype=float, shape=(max_seg * 10,))

    while True:
        seg_index += 1

        if seg_index > max_seg:
            s.wlog(1, " ")
            s.wlog(1, " Trying to exceed SCHED''s limit of {} scans in an "
                   "inserted geodetic segment.".format(max_seg))
            s.errlog(" Shorten the requested segment or divide it into "
                     "multiple segments.")

        l_scan = scan_index + seg_index - 1

        scan_range = slice(scan_index - 1, l_scan - 1)
        
        # scans in segment per station where station is up
        n_stascn = np.array([np.count_nonzero(
            np.logical_and(s.stascn[scan_range] != 0,
                           np.logical_and(f2str(s.up1[scan_range]) == "",
                                          f2str(s.up2[scan_range]) == "")))
                             for s in stations], dtype=int)

        if s.schsou.geoprt >= 2:
            s.wlog(0, " ")
            s.wlog(0, "----MAKESEG:  Starting to work on next source: "
                   "ISEG: {}   DORAND: {}   MINNSS: {}   MAXPRIO: {}".format(
                       seg_index, bool2str(do_rand), min(n_stascn), max_prio))
        
        ii_scan = max(scan_index, l_scan - s.schsou.geoback + 1)
        s_scan1  = max(scan_index, l_scan - s.schsou.geosrep + 1)

        if (((n_station_low < n_sta) or (n_station_high < n_sta)) and do_rand) \
           or (min(n_stascn) <= 1):
            method = "RAND"

            n_near = 5
            n_chance = 0

            for geo_index in range(1, s.schsou.ngeo + 1):
                if (use_geo[geo_index - 1] <= max_prio) and \
                   (selected[geo_index - 1] < s_scan1):
                    if seg_index == 1:
                        approx_time = start_time
                    else:
                        approx_time = scans[l_scan - 2].stopj + 60 / secpday

                    if s.schsou.geoprt >= 2:
                        s.wlog(0, "MAKESEG random section - looking for nearby "
                               "source.  Considering: {}  SRC: {} {}  "
                               "USEGEO: {} at {:7.6f}".format(
                                   seg_index, geo_index, 
                                   f2str(s.schcsc.geosrc[geo_index - 1]), 
                                   use_geo[geo_index - 1], approx_time))

                    n_good, ok_sta, scan_stascn = gmkscn(
                        last_scan_index_copy, l_scan, j_scan, 
                        s.schsou.geosrci[geo_index - 1], 
                        f2str(s.schcsc.geosrc[geo_index - 1]), approx_time, 
                        scans[j_scan - 1].opminel, 0, scan_stascn, "SET")

                    k_stascn[geo_index - 1, :] = scan_stascn

                    n_new_low = 0
                    n_new_high = 0
                    need_low = 0
                    need_high = 0
                    for i, station in enumerate(stations):
                        if station_low[i] < low_high_goal:
                            need_low += 1
                        if station_high[i] < low_high_goal:
                            need_high += 1
                        if scan_stascn[i]:
                            average_el = (station.el1[l_scan - 1] + 
                                          station.el2[l_scan - 1]) / 2
                            if (average_el <= s.schsou.geolowel) and \
                               (station_low[i] < low_high_goal):
                                n_new_low += 1
                            if (average_el >= s.schsou.geohiel) and \
                               (station_high[i] < low_high_goal):
                                n_new_high += 1
                                
                    src_ok = (n_new_low + n_new_high > n_sta // 3) or \
                             (n_new_low + n_new_high >= 
                              max(1, (need_low + need_high) // 3))

                    sta_ok = True
                    new_c = False
                    for i, station in enumerate(stations):
                        temp_in_scan = in_scan[i]
                        if station.stascn[l_scan - 1]:
                            temp_in_scan += 1
                        if temp_in_scan < seg_index // 2:
                            sta_ok = False

                    if src_ok and sta_ok and \
                       (n_good >= scans[j_scan - 1].opmian):
                        if (n_chance < n_near) or (l_scan == s.schn1.scan1):
                            n_chance += 1
                            chance[n_chance - 1] = geo_index
                            test_time[n_chance - 1] = scans[l_scan - 1].startj
                            new_c = True
                        elif scans[l_scan - 1].startj < test_time[n_near - 1]:
                            chance[n_near - 1] = geo_index
                            test_time[n_near - 1] = scans[l_scan - 1].startj
                            new_c = True
                            
                        if s.schsou.geoprt >= 2:
                            if l_scan == s.schn1.scan1:
                                l_source_name = "First scan"
                                seperation_time = 0
                            else:
                                l_source_name = scans[l_scan - 2].scnsrc
                                seperation_time = \
                                    (scans[l_scan - 1].startj - 
                                     scans[l_scan - 2].stopj) * secpday
                            
                            s.wlog(0, "MAKESEG getting slew info {} {} "
                                   "Last source: {}  Testing: {}".format(
                                       geo_index, l_scan, l_source_name, 
                                       f2str(s.schcsc.geosrc[geo_index - 1])))

                            for i, station in enumerate(stations, 1):
                                if l_scan == s.schn1.scan1:
                                    l_stas = False
                                else:
                                    l_stas = station.stascn[l_scan - 2]

                                lsi = last_scan_index_copy[i - 1] - 1
                                if lsi < 0:
                                    el2 = 0
                                    az2 = 0
                                else:
                                    el2 = station.el2[lsi]
                                    az2 = station.az2[lsi]

                                s.wlog(0, " makeseg els, azs, tslew: {:4d} {:4d}"
                                       " {:7.1f} {:7.1f} {:7.1f} {:7.1f} {:7.1f}"
                                       " {} {} {:7.1f}".format(
                                           i, lsi + 1, 
                                           el2, station.el1[l_scan - 1], 
                                           az2, station.az1[l_scan - 1], 
                                           station.tslew[l_scan - 1] * secpday, 
                                           bool2str(l_stas), 
                                           bool2str(station.stascn[l_scan - 1]), 
                                           seperation_time))
                                
                        if new_c and (l_scan != s.schn1.scan1):
                            for ic in range(n_chance - 1, 0, -1):
                                if test_time[ic] < test_time[ic - 1]:
                                    # swap
                                    chance[ic - 1], chance[ic] = \
                                        chance[ic], chance[ic - 1]
                                    test_time[ic - 1], test_time[ic] = \
                                        test_time[ic], test_time[ic - 1]
                        
                    #if src_ok and sta_ok and (n_good >= scan[j_scan - 1].opmian)
                #if (use_geo[igeo] <= max_prio) and (selected[igeo] < s_scan1)
            #for igeo in range(1, ngeo + 1)
            
            if n_chance == 0:
                if s.schsou.geoprt >= 1:
                    s.wlog(0, "MAKESEG: No priority <= {} sources found for "
                           "random source additions.".format(max_prio))

                if 3 <= max_prio < 5:
                    if min(n_stascn) >= 2:
                        do_rand = False
                        if s.schsou.geoprt >= 1:
                            s.wlog(0, "         Select a source using a "
                                   "SecZ fit.")
                    else:
                        max_prio += 1
                        do_rand = True
                elif max_prio >= 5:
                    s.wlog(1, "Too few useful sources to choose from to "
                           "construct a geodetic segment. ")
                    s.wlog(1, "   Possible solutions include: ")
                    s.wlog(1, "     Add sources ")
                    s.wlog(1, "     Adjust GEOLOWEL and GEOHIEL so more "
                           "sources are considered useful.")
                    s.wlog(1, "     Try different OPMINANT or OPMINEL")
                    s.wlog(1, "    Pick a solution and try again.")
                else:
                    max_prio += 1
                    if s.schsou.geoprt >= 1:
                        s.wlog(0, "         Allow scans with low stations but "
                               "no very high elevation stations.")
                
                seg_index -= 1
                continue

            max_ic = 0
            for ic in range(1, n_chance + 1):
                if test_time[ic - 1] <= test_time[0] + 60 / secpday:
                    max_ic = ic
            
            dummy_index, result = s.ran5wrap(dummy_index)
            ic = int(1 + result * max_ic)
            
            if s.schsou.geoprt >= 2:
                s.wlog(0, "MAKESEG picked random source: {} {}".format(
                    ic, chance[ic - 1]))

            t_src[seg_index - 1] = chance[ic - 1]

            scan_stascn[:] = k_stascn[t_src[seg_index - 1] - 1, :]
                
            selection_type[seg_index - 1] = "NR"
            worst_station[seg_index - 1] = "--"

            if s.schsou.geoprt >= 2:
                t0 = int(scans[l_scan - 1].stopj)
                minpday = 24 * 60
                minutes = (scans[l_scan - 2].stopj - t0) * minpday
                s.wlog(0, "MAKESEQ slew times: {} {} {:.2f} {} {}".format(
                    t_src[seg_index - 1], ic, minutes, n_chance, max_ic))
                s.wlog(0, " Time below is minutes in day ")
                s.wlog(0, "                    Src    Time  Since Last stop")
                for is_ in range(n_chance):
                    s.wlog(0, "{:9d} {:9d} {:6} {:10.2f} {:9.2f}".format(
                        is_ + 1, chance[is_], 
                        f2str(s.schcsc.geosrc[chance[is_] - 1]),
                        (test_time[is_] - t0) * minpday, 
                        (test_time[is_] - scans[l_scan - 2].stopj) * minpday))

        #if (((n_station_low < n_sta) or (n_station_high < n_sta)) and do_rand) \
        #   or (min(n_stascn) <= 1):
        else:
            method = "FIT"
            bsl_qual = 1e9
            bf_qual = 1e9
            bf_qual_sl = 1e9
            t_src[seg_index - 1] = 0
            t_src1 = 0
            t_src2 = 0
            t_src3 = 0
            for geo_index in range(1, s.schsou.ngeo + 1):
                if ok_geo[geo_index - 1] and (selected[geo_index - 1] < s_scan1):
                    approx_time = scans[l_scan - 2].stopj + 60 / secpday
                    n_good, ok_sta, scan_stascn = gmkscn(
                        last_scan_index_copy, l_scan, j_scan, 
                        s.schsou.geosrci[geo_index - 1], 
                        f2str(s.schcsc.geosrc[geo_index - 1]), approx_time, 
                        scans[j_scan - 1].opminel, last_high_station,
                        scan_stascn, "SET")
                    
                    k_stascn[geo_index - 1, :] = scan_stascn

                    sta_ok = True

                    if last_high_station != 0:
                        if not scan_stascn[last_high_station - 1]:
                            sta_ok = False

                    if sta_ok:
                        for i, station in enumerate(stations):
                            temp_in_scan = in_scan[i]
                            if station.stascn[l_scan - 1]:
                                temp_in_scan += 1
                            if temp_in_scan < seg_index // 2:
                                sta_ok = False

                            if not station.stascn[l_scan - 1]:
                                stations_in_lookback = np.count_nonzero(
                                    station.stascn[ii_scan - 1: l_scan - 1])
                                if stations_in_lookback < 2:
                                    sta_ok = False
                    
                    if (n_good >= scans[j_scan - 1].opmian) and sta_ok and \
                       (scans[l_scan - 1].stopj <= end_time):
                        if t_src[seg_index - 1] == 0:
                            k_scan = ii_scan
                        else:
                            k_scan = l_scan

                        print_debug = (s.schsou.geoprt >= 2)
                        if print_debug:
                            s.wlog(0, " ")
                            s.wlog(0, "Makeseg about to test quality {} {} {} "
                                   "{} {} {}  geosrc: {} {}".format(
                                       seg_index, scan_index, ii_scan, k_scan, 
                                       l_scan, j_scan, geo_index, 
                                       f2str(s.schcsc.geosrc[geo_index - 1])))
                        
                        test_quality, sigma = s.geoqual(
                            ii_scan, k_scan, l_scan, j_scan, 14.48, print_debug, 
                            sigma)

                        if last_high_station >= 1:
                            f_qual = sigma[last_high_station - 1]
                        else:
                            f_qual = 0

                        sl_qual = math.sqrt(np.sum(np.square(sigma[:n_sta])) 
                                            / n_sta)
                        max_sigma = np.max(sigma[:n_sta])

                        slew_time = scans[l_scan - 1].startj - \
                                    scans[l_scan - 2].stopj
                        penalty = s.schsou.geoslew * slew_time / (30 / secpday)

                        sl_qual += penalty
                        if f_qual != 0:
                            f_qual_sl = f_qual + penalty
                        else:
                            f_qual_sl = 0

                        if s.schsou.geoprt >= 2:
                            s.wlog(0, "Makeseg quality - LSCN: {} {}  Source: "
                                   "{} {}  Quality: {:.2f} {:.2f} {:.2f} {} "
                                   "{:.2f}".format(
                                       l_scan, seg_index, geo_index,  
                                       f2str(s.schcsc.geosrc[geo_index - 1]), 
                                       f_qual_sl, f_qual, sl_qual, 
                                       last_high_station, max_sigma))

                        if (f_qual_sl != 0) and (f_qual_sl < bf_qual_sl):
                            bf_qual_sl = f_qual_sl
                            t_src1 = geo_index
                            high_station1 = np.argmax(sigma[:n_sta]) + 1
                            high_sigma1 = sigma[high_station1 - 1]
                            save_sigma1 = np.copy(sigma)
                            if s.schsou.geoprt >= 2:
                                s.wlog(0, "makeseg: Got new best sigma with "
                                       "slew penalty:  {}  {} {:.2f} {} {:.2f}".\
                                       format(
                                           geo_index, 
                                           f2str(s.schcsc.geosrc[geo_index - 1]),
                                           f_qual_sl, high_station1, 
                                           high_sigma1))
                            
                        if (f_qual != 0) and (f_qual < bf_qual):
                            bf_qual = f_qual
                            t_src2 = geo_index
                            high_station2 = np.argmax(sigma[:n_sta]) + 1
                            high_sigma2 = sigma[high_station2 - 1]
                            save_sigma2 = np.copy(sigma)
                            if s.schsou.geoprt >= 2:
                                s.wlog(0, "makeseg: Got new best sigma without "
                                       "slew penalty:  {}  {} {:.2f} {} {:.2f}".\
                                       format(
                                           geo_index, 
                                           f2str(s.schcsc.geosrc[geo_index - 1]),
                                           f_qual, high_station2, 
                                           high_sigma2))

                        if sl_qual < bsl_qual:
                            bsl_qual = sl_qual
                            t_src3 = geo_index
                            high_station3 = np.argmax(sigma[:n_sta]) + 1
                            high_sigma3 = sigma[high_station3 - 1]
                            save_sigma3 = np.copy(sigma)
                            if s.schsou.geoprt >= 2:
                                s.wlog(0, "makeseg: Got new best RMS sigma with "
                                       "slew penalty:  {}  {} {:.2f} {} {:.2f}".\
                                       format(
                                           geo_index, 
                                           f2str(s.schcsc.geosrc[geo_index - 1]),
                                           sl_qual, high_station3, 
                                           high_sigma3))

                    #if (n_good >= scans[j_scan - 1]) and sta_ok and \
                    #   (scans[l_scan - 1] <= end_time)
                #if ok_geo[geo_index - 1] and (selected[geo_index - 1] < s_scan1)
            #for geo_index in range(1, s.schsou.ngeo + 1):
            
            if last_high_station != 0:
                if (t_src1 != 0) and \
                   ((last_high_sigma - save_sigma1[last_high_station - 1]) > 
                    (last_high_sigma - save_sigma2[last_high_station - 1]) 
                    * 0.7) and \
                   (last_high_sigma / save_sigma1[last_high_station - 1] > 1.1):
                    t_src[seg_index - 1] = t_src1
                    selection_type[seg_index - 1] = "SP"
                    worst_station[seg_index - 1] = \
                        stations[high_station1 - 1].stcode

                    high_station = high_station1
                    high_sigma = high_sigma1
                    save_sigma = np.copy(save_sigma1)
                    last_type = "SIGP"
                    if s.schsou.geoprt >= 1:
                        s.wlog(0, "MAKESEG: {}  Choice used slew penalty. "
                               "Last: {:.2f}  Options (sig/sta/src): "
                               "{:.2f}/{}/{} {:.2f}/{}/{} {:.2f}/{}/{}".format(
                                   seg_index, last_high_sigma, 
                                   save_sigma1[last_high_station - 1], 
                                   high_station1, t_src1, 
                                   save_sigma2[last_high_station - 1], 
                                   high_station2, t_src2, 
                                   save_sigma3[last_high_station - 1], 
                                   high_station3, t_src3))
                    
                elif (t_src2 != 0) and \
                   ((last_high_sigma / save_sigma2[last_high_station - 1] > 1.25)
                    or ((last_type == "RMS") and 
                        (high_station3 == last_high_station) and
                        (high_sigma3 > high_sigma2))):
                    t_src[seg_index - 1] = t_src2
                    selection_type[seg_index - 1] = "S"
                    worst_station[seg_index - 1] = \
                        stations[high_station2 - 1].stcode
                    
                    high_station = high_station2
                    high_sigma = high_sigma2
                    save_sigma = np.copy(save_sigma2)
                    last_type = "SIG"
                    if s.schsou.geoprt >= 1:
                        s.wlog(0, "MAKESEG: {}  Choice used sigma only. "
                               "Last: {:.2f}  Options (sig/sta/src): "
                               "{:.2f}/{}/{} {:.2f}/{}/{} {:.2f}/{}/{}".format(
                                   seg_index, last_high_sigma, 
                                   save_sigma1[last_high_station - 1], 
                                   high_station1, t_src1, 
                                   save_sigma2[last_high_station - 1], 
                                   high_station2, t_src2, 
                                   save_sigma3[last_high_station - 1], 
                                   high_station3, t_src3))
            # if last_high_station != 0
    
            if (t_src3 != 0) and (t_src[seg_index - 1] == 0):
                t_src[seg_index - 1] = t_src3
                selection_type[seg_index - 1] = "RMS"
                worst_station[seg_index - 1] = stations[high_station3 - 1].stcode
                
                high_station = high_station3
                high_sigma = high_sigma3
                save_sigma = np.copy(save_sigma3)
                last_type = "RMS"
                if s.schsou.geoprt >= 1:
                    s.wlog(0, "MAKESEG: {}  Choice used RMS sigma.    "
                           "Last: {:.2f}  Options (sig/sta/src): "
                           "{:.2f}/{}/{} {:.2f}/{}/{} {:.2f}/{}/{}".format(
                               seg_index, last_high_sigma,
                               save_sigma1[last_high_station - 1], 
                               high_station1, t_src1, 
                               save_sigma2[last_high_station - 1], 
                               high_station2, t_src2, 
                               save_sigma3[last_high_station - 1], 
                               high_station3, t_src3))
                
            if t_src[seg_index - 1] != 0:
                scan_stascn[:] = k_stascn[t_src[seg_index - 1] - 1, :]
        
        #end of else 
        #if (((n_station_low < n_sta) or (n_station_high < n_sta)) and do_rand)
        #   or (min(n_stascn) <= 1):
        
        if t_src[seg_index - 1] > 0:
            if s.schsou.geoprt >= 2:
                s.wlog(0, "Makeseg got the next source - seg: {}  "
                       "Geosrc: {}  {} tapprox: {:.2f}  stascn: {}".format(
                           seg_index, t_src[seg_index - 1], 
                           f2str(s.schcsc.geosrc[t_src[seg_index - 1] - 1]), 
                           approx_time, 
                           " ".join(bool2str(e) for e in scan_stascn[:n_sta])))

            n_good, ok_sta, scan_stascn = gmkscn(
                last_scan_index_copy, l_scan, j_scan, 
                s.schsou.geosrci[t_src[seg_index - 1] - 1], 
                f2str(s.schcsc.geosrc[t_src[seg_index - 1] - 1]), approx_time, 
                scans[j_scan - 1].opminel, 0, scan_stascn, "FORCE")
            
            for i, station in enumerate(stations):
                if station.stascn[l_scan - 1]:
                    in_scan[i] += 1
                    last_scan_index_copy[i] = l_scan
                    average_el = (station.el1[l_scan - 1] + 
                                  station.el2[l_scan - 1]) / 2
                    if average_el <= s.schsou.geolowel:
                        station_low[i] += 1
                    if average_el >= s.schsou.geohiel:
                        station_high[i] += 1
                        
            n_station_low = np.count_nonzero(station_low >= low_high_goal)
            n_station_high = np.count_nonzero(station_high >= low_high_goal)
            
            selected[t_src[seg_index - 1] - 1] = l_scan

            if s.schsou.geoprt >= 1:
                msg = "Makeseg source: {} {}  {}".format(
                    seg_index, t_src[seg_index - 1], 
                    f2str(s.schcsc.geosrc[t_src[seg_index - 1] - 1]))
                if method == "FIT":
                    msg += " High: {}  Sigmas: {}".format(
                        high_station, 
                        " ".join("{:5.1f}".format(e) 
                                 for e in save_sigma[:n_print]))
                s.wlog(0, msg)

            last_high_station = high_station
            last_high_sigma = high_sigma

            if scans[l_scan - 1].stopj > \
               start_time + 0.7 * (end_time - start_time):
                do_rand = False
            else:
                do_rand = True

            max_prio = 2
            continue

        else:
            if s.schsou.geoprt >= 2:
                s.wlog(0, "makeseg:  No source found that fits")
                s.wlog(0, "          Ending sequence.")

            seg_index -= 1
            nt_seg = seg_index
            l_scan = scan_index + nt_seg - 1
            
            # this is the only exit point (apart from s.errlog calls)

            return l_scan, nt_seg, t_src, dummy_index, selection_type, \
                worst_station

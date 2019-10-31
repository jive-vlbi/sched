from .parameter import secpday, onesec
from . import scndup
from ..catalog import StationCatalog
from ..util import f2str, bool2str

import schedlib as s

import numpy as np

def addpeak(last_scan_index, scans, stations, setups, peak_groups, 
            peak_opt, insert_adjust, scan_index):
    if s.schcon.debug:
        s.wlog(0, "ADDPEAK starting")

    scan = scans[scan_index - 1]

    if (not s.schcon.autopeak) or ((scan.point >= -1) and (peak_opt == 0)):
        return peak_opt, insert_adjust

    assert(s.schpeakn.npkgrp > 0), "ADDPEAK: Want automatic pointing, but "\
        "there are no groups"

    t_gap = np.empty(dtype=float, shape=(len(stations),))
    try_group = np.full(fill_value=False, shape=(len(peak_groups),))
    try_group2 = np.full(fill_value=False, shape=(len(peak_groups),))
    # last_*_scan objects are passed to FORTRAN, so require a fixed size,
    # only len(stations) is actually used
    last_target_scan = np.empty(dtype=int, shape=(StationCatalog.maxsta,))
    last_pointing_scan = np.empty(dtype=int, shape=(StationCatalog.maxsta,))
    sr_ok = np.empty(dtype=bool, shape=(s.schpeakn.pksrnum.shape[0],))
    sr_ok2 = np.empty(dtype=bool, shape=(s.schpeakn.pksrnum.shape[0],))
    slew_max = np.empty(dtype=float, shape=(s.schpeakn.pksrnum.shape[0],))
    slew_min = np.empty(dtype=float, shape=(s.schpeakn.pksrnum.shape[0],))
    if peak_opt == 0:
        if s.schcon.pkwatch:
            s.wlog(0, " ")
            year, day1, start = s.timej(scan.startj)
            stime = f2str(s.tformwrp(start, "T", 0, 2, 2, "::@"))
            s.wlog(0, "ADDPEAK: Trying to add pointing scan for target scan "
                   "{} {} {}".format(scan_index, scan.scnsrc, stime))

        try_group = np.full(shape=(s.schpeakn.npkgrp,), fill_value=False)
        try_group2 = np.full(shape=(s.schpeakn.npkgrp,), fill_value=False)
        n_up = np.full(shape=(s.schpeakn.npkgrp,), fill_value=0)
        m_settle = np.full(shape=(s.schpeakn.npkgrp,), fill_value=0.0)
        
        n_good = 0
        for station in stations:
            group_index = station.pkgroup
            if station.stascn[scan_index - 1] and (group_index != 0) and \
               ((f2str(station.up1[scan_index - 1]) == "") or 
                (f2str(station.up2[scan_index - 1]) == "")):
                n_good += 1
                n_up[group_index - 1] += 1

        if n_good == 0:
            if s.schcon.pkwatch:
                s.wlog(0, "ADDPEAK:  Source down or below OPMINEL at all "
                       "stations.")
            return peak_opt, insert_adjust

        for station_index, station in enumerate(stations):
            if station.stascn[scan_index - 1] and (station.pkgroup > 0):
                setup = setups[station.nsetup[scan_index - 1] - 1]
                group_index = station.pkgroup
                freq_test = setup.freqref[0]
                peak_group = peak_groups[group_index - 1]
                if (peak_group.pkminfq < freq_test):
                    if (last_scan_index[station_index] != 0):
                        last_scan = scans[last_scan_index[station_index] - 1]
                        t_gap[station_index] = scan.startj - last_scan.stopj
                        lastptg = (last_scan.point in {0, group_index}) and \
                                  (t_gap[station_index] < 600. / secpday)
                        if (not lastptg) and (n_up[group_index - 1] > 0):
                            if t_gap[station_index] > peak_group.pkdwell:
                                try_group[group_index - 1] = True
                            if t_gap[station_index] > 2 * peak_group.pkdwell:
                                try_group2[group_index - 1] = True
                    else:
                        t_gap[station_index] = 10000
                        try_group[group_index - 1] = n_up[group_index - 1] > 0
                        try_group2[group_index - 1] = n_up[group_index - 1] > 0

            last_target_scan[station_index] = last_scan_index[station_index]
            last_pointing_scan[station_index] = last_scan_index[station_index]

        n_try = np.count_nonzero(try_group) + np.count_nonzero(try_group2)
        
        if s.schcon.pkwatch:
            if n_try >= 1:
                for group_index, peak_group in enumerate(peak_groups):
                    if try_group2[group_index]:
                        s.wlog(0, "ADDPEAK: Pointing group {}:  Will try to "
                               "add double pointing scans.".format(
                                   group_index + 1))
                    elif try_group[group_index]:
                        s.wlog(0, "ADDPEAK: Pointing group {}:  Will try to "
                               "add one pointing scan.".format(
                                   group_index + 1))
                    else:
                        if n_up[group_index - 1] > 0:
                            s.wlog(0, "ADDPEAK: Pointing group {}:  No "
                                   "stations up in scan.".format(
                                       group_index + 1))
                        else:
                            s.wlog(0, "ADDPEAK: Pointing group {}:  Gap too "
                                   "short to add pointing scans.".format(
                                       group_index + 1))
            else:
                s.wlog(0, "ADDPEAK: Insufficient time to add pointing or "
                       "last scan was pointing.")

        if n_try == 0:
            return peak_opt, insert_adjust

        keep_origen = scan.origen
        scan.origen = 4
        for k_scan in range(scan_index, scan_index + n_try):
            scndup(k_scan, scan_index - 1, True, "ADDPEAK")

        n_added = 0
        m_scan = scan_index + n_try
        for group_index, peak_group in enumerate(peak_groups, 1):
            if try_group[group_index - 1]:
                if s.schcon.pkwatch:
                    s.wlog(0, "ADDPEAK:  Working on pointing group {}".format(
                        group_index))
                    tab_head = True

                k_scan = scan_index + n_added
                got_station = False
                for station in stations:
                    station.stascn[k_scan - 1] = \
                        station.stascn[m_scan - 1] and \
                        (station.pkgroup == group_index) and \
                        ((f2str(station.up1[m_scan - 1]) == "") or 
                         (f2str(station.up2[m_scan - 1]) == ""))
                    if station.stascn[k_scan - 1]:
                        got_station = True

                if got_station:
                    for p_src in range(1, peak_group.npksrc + 1):
                        stop_k = scans[m_scan - 1].startj - 20.0 * onesec
                        start_k = stop_k - peak_group.pkdwell
                        k_src = peak_group.pksrnum[p_src - 1]
                        s.srinsert(k_scan, k_src, peak_group.pksrc[p_src - 1],
                                   start_k, stop_k, last_scan_index, m_scan)

                        sr_ok[p_src - 1] = True
                        sr_ok2[p_src - 1] = True
                        watch_it = False
                        min_el = 100
                        max_el = 0
                        min_slew = 1000
                        max_slew = 0
                        min_tot = 10000
                        for station in stations:
                            if station.stascn[k_scan - 1] and \
                               (station.el1[k_scan - 1] < peak_group.pkminel):
                                sr_ok[p_src - 1] = False
                                sr_ok2[p_src - 1] = False

                            if s.schcon.pkwatch:
                                if station.stascn[k_scan - 1] and \
                                   (f2str(station.up1[k_scan - 1]) == "") and \
                                   (f2str(station.up2[k_scan - 1]) == ""):
                                    watch_it = True

                                if station.stascn[k_scan -1]:
                                    min_el = min(min_el, 
                                                 station.el1[k_scan - 1])
                                    max_el = max(max_el,
                                                 station.el1[k_scan - 1])
                                    min_slew = min(
                                        min_slew, 
                                        station.tslew[m_scan - 1] * secpday)
                                    max_slew = max(
                                        max_slew,
                                        station.tslew[m_scan - 1] * secpday)
                    
                        if sr_ok[p_src - 1]:
                            for station_index, station in enumerate(stations):
                                if station.stascn[k_scan - 1]:
                                    tot_time = peak_group.pkdwell + \
                                        station.tslew[k_scan - 1] + \
                                        station.tslew[m_scan - 1]
                                    if t_gap[station_index] < tot_time:
                                        sr_ok[p_src - 1] = False
                                    min_tot = min(min_tot, tot_time * secpday)

                                    tot_time2 = 2 * peak_group.pkdwell + \
                                                station.tslew[k_scan - 1] + \
                                                station.tslew[m_scan - 1] + \
                                                station.tsettle / secpday
                                    if t_gap[station_index] < tot_time2:
                                        sr_ok2[p_src - 1] = False

                                    m_settle[group_index - 1] = \
                                        max(m_settle[group_index - 1],
                                            station.tsettle)
                        
                        slew_max[p_src - 1] = 0
                        if sr_ok[p_src - 1]:
                            for station in stations:
                                if station.stascn[k_scan - 1]:
                                    slew_max[p_src - 1] = max(
                                        slew_max[p_src - 1], 
                                        station.tslew[m_scan - 1])

                        if s.schcon.pkwatch and watch_it:
                            if tab_head:
                                s.wlog(0, " Num   Source      Slew times (sec) "
                                       "Elevations (deg)  Scans OK  Min T")
                                tab_head = False

                            s.wlog(0, "{:>6} {:<12}"
                                   "{:>6.0f} to {:<6.0f} "
                                   "{:>6.0f} to {:<6.0f}"
                                   "    {} {}   {:>7.0f}".format(
                                       p_src, peak_group.pksrc[p_src - 1], 
                                       min_slew, max_slew, min_el, max_el, 
                                       bool2str(sr_ok[p_src - 1]), 
                                       bool2str(sr_ok2[p_src - 1]), 
                                       float(min_tot)))
                            
                    use_p_src = 0
                    slew_min = 1
                    for p_src in range(1, peak_group.npksrc + 1):
                        if sr_ok[p_src - 1]:
                            if (0 < slew_max[p_src - 1]  < slew_min):
                                slew_min = slew_max[p_src - 1]
                                use_p_src = p_src

                    if use_p_src != 0:
                        p_src = use_p_src
                        k_src = peak_group.pksrnum[p_src - 1]

                        add2 = try_group2[group_index - 1] and sr_ok2[p_src - 1]
                        
                        j_scan = k_scan
                        ok_scan = 0
                        stop_k = scans[m_scan - 1].startj - slew_min
                        start_k = stop_k - peak_group.pkdwell

                        if add2:
                            n_added += 2
                            ok_scan = 2

                            stop2 = start_k - m_settle[group_index - 1] / secpday
                            start2 = stop2 - peak_group.pkdwell
                            
                            s.srinsert(k_scan, k_src, 
                                       peak_group.pksrc[p_src - 1], start2,
                                       stop2, last_scan_index, m_scan)
                            scans[k_scan - 1].point = group_index
                            
                            j_scan = k_scan + 1
                            for station_index, station in enumerate(stations):
                                if station.stascn[k_scan - 1]:
                                    last_pointing_scan[station_index] = j_scan

                        else:
                            j_scan = k_scan
                            n_added += 1
                            ok_scan = 1

                        s.srinsert(j_scan, k_src, peak_group.pksrc[p_src - 1],
                                   start_k, stop_k, last_pointing_scan, m_scan)
                        scans[j_scan - 1].point = group_index
                        
                        if s.schcon.pkwatch:
                            s.wlog(0, "ADDPEAK: Adding {} scan(s) on {} for "
                                   "pointing group {}".format(
                                       ok_scan, peak_group.pksrc[p_src - 1],
                                       group_index))
                        
                        for station_index, station in enumerate(stations):
                            if station.stascn[j_scan - 1]:
                                last_target_scan[station_index] = j_scan

                    else:
                        if s.schcon.pkwatch:
                            s.wlog(0, "ADDPEAK: No pointing sources available "
                                   "for pointing group {}".format(group_index))

                else:
                    if s.schcon.pkwatch:
                        s.wlog(0, "ADDPEAK:  No stations for group {} in this "
                               "scan. ".format(group_index))

            # end of if try_group[group_index]
        # end of loop of peak groups

        k_scan = scan_index + n_added
        if k_scan != m_scan:
            scndup(k_scan - 1, m_scan - 1, True, "ADDPEAK2")
        scans[k_scan - 1].origen = keep_origen

        n_good = s.scngeo(last_target_scan, k_scan)
        scans[k_scan - 1].point = -999

        if n_added > 0:
            peak_opt = n_added
        else:
            peak_opt = 0

    else: # peak_opt != 0
        if peak_opt > 0:
            peak_opt -= 1

    if peak_opt >= 1:
        insert_adjust = False

    return peak_opt, insert_adjust

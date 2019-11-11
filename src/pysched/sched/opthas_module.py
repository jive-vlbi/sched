from .parameter import secpday, sidereal
from ..catalog import ScanCatalog, StationCatalog, SourceCatalog
from ..util import f2str
from . import scndup

import schedlib as s

import numpy as np

import math

scan_catalog = ScanCatalog()
station_catalog = StationCatalog()

max_ins = ScanCatalog.maxscan // 2
scan_used = np.empty(dtype=int, shape=(max_ins,))
ha_min = np.empty(dtype=float, shape=(max_ins,))
ha_max = np.empty(dtype=float, shape=(max_ins,))
ha_begin = np.empty(dtype=float, shape=(max_ins,))
ha_end = np.empty(dtype=float, shape=(max_ins,))
t_ha_min = np.empty(dtype=float, shape=(max_ins,))
t_ha_max = np.empty(dtype=float, shape=(max_ins,))
t_use = np.empty(dtype=float, shape=(max_ins,))
t_avail = np.empty(dtype=float, shape=(max_ins,))
source_used = np.empty(dtype=int, shape=(max_ins,))
source_look = np.empty(dtype=int, shape=(max_ins,))
scan_weight = np.empty(dtype=float, shape=(max_ins,))
op_ha_t = np.empty(dtype=float, shape=(max_ins,))
op_ha_wid_t = np.empty(dtype=float, shape=(max_ins,))

def opthas(last_scan_index, k_scan, scan_index):
    global n_out, scan_used, dur_total, ref_station_index, \
        ha_min, ha_max, ha_begin, ha_end, t_ha_min, t_ha_max, t_use, t_avail, \
        source_used, source_look, \
        skip_time, skip_inc, n_skip, sd_total, \
        op_ha_t, op_ha_wid_t


    scans = scan_catalog.direct_access_entries
    stations = station_catalog.used(use_direct_access=True)
    
    if k_scan == 1:
        # initialize for multiple calls to this function
        n_out = 0
        skip_time = 0
        n_skip = 0
        skip_inc = 5 * 60 / secpday
        if s.schn1.nscans > max_ins:
            s.errlog("OPHAS: Too many requested scans for OPTMODE=HAS.  Max: {} "
                     "Requested: {}".format(max_ins, s.schn1.nscans))
        if s.schcon.opdur <= 0:
            s.errlog("OPHAS: Please specify OPDUR for OPTMODE=HAS")

        min_duration = min(scan.dur for scan in scans[:s.schn1.nscans])
        if (s.schcon.opdur * sidereal - min_duration) > 1:
            s.errlog("OPHAS: OPTMODE=HAS not meant for observations over "
                     "24 hours sidereal + min scan dur.")

        ht_stop = scans[0].startj + s.schcon.opdur

        _, ref_station_index, _ = s.stano(f2str(s.schsco.ophasta))
        source_catalog = SourceCatalog()
        source_used = np.zeros(shape=(len(source_catalog.entries),), dtype=int)
        source_look = np.zeros(shape=(s.schn1.nscans,), dtype=int)
        dur_total = 0
        sd_total = 0
        for i, scan in enumerate(scans[:s.schn1.nscans]):
            source_index = scan.srcnum
            source_used[source_index - 1] += 1
            source_look[i] = source_used[source_index - 1]
            scan_used[i] = 0
            dur_total += scan.dur

        if dur_total > s.schcon.opdur:
            s.wlog(1, "OPTHAS: ** The total time in requested scans of "
                   "{} hours".format(dur_total * 24))
            s.wlog(1, "           exceeds the experiment duration (OPDUR) of "
                   "{} hours.".format(s.schcon.opdur * 24))
            s.wlog(1, "           Not all scans can be scheduled.")

        ref_station = stations[ref_station_index - 1]
        for j_scan, scan in enumerate(scans[:s.schn1.nscans]):
            ha_min[j_scan], ha_max[j_scan], t_ha_min[j_scan], t_ha_max[j_scan] \
                = s.halim(j_scan + 1, ref_station_index)
            
            ref_station.ha1[j_scan], ref_station.el1[j_scan], \
                ref_station.az1[j_scan], ref_station.lst1[j_scan], \
                ref_station.pa1[j_scan] = s.schgeo(
                    j_scan + 1, ref_station_index, scans[0].startj)
            ref_station.ha2[j_scan], ref_station.el2[j_scan], \
                ref_station.az2[j_scan], ref_station.lst2[j_scan], \
                ref_station.pa2[j_scan] = s.schgeo(
                    j_scan + 1, ref_station_index, ht_stop - scan.dur)

            ha_begin[j_scan] = ref_station.ha1[j_scan]
            ha_end[j_scan] = ref_station.ha2[j_scan]

            t_avail[j_scan], t_use[j_scan] = s.haavai(
                t_ha_min[j_scan], t_ha_max[j_scan], 
                scans[0].startj, ht_stop - scan.dur,
                source_used[scan.srcnum - 1], source_look[j_scan])
            
            if t_avail[j_scan] == -1:
                s.wlog(1, " ")
                s.wlog(1, " ****** {} is never above OPMINEL and the horizon at "
                       "OPMINANT stations at once.".format(scan.scnsrc))
                s.errlog("Leave out the source or change OPMINANT or OPMINEL")

            if scan.opha == 0:
                op_ha_t[j_scan] = t_use[j_scan]
            else:
                op_ha_t[j_scan] = (scan.opha - ha_begin[j_scan]) \
                                  / sidereal + scans[0].startj
                if op_ha_t[j_scan] < scans[0].startj:
                    op_ha_t[j_scan] +=  24 / sidereal
                    if op_ha_t[j_scan] > scans[0].startj + s.schcon.opdur:
                        s.wlog(1, "OPHAS: Requested hour angle outside range: "
                               "scan, source, OPHA: {} {} {}".format(
                                   j_scan + 1, scan.scnsrc, scan.opha))

            if scan.ophawid == 0:
                op_ha_wid_t[j_scan] = 0.7 * t_avail[j_scan] \
                                      / source_used[scan.srcnum - 1]
            else:
                op_ha_wid_t[j_scan] = scan.ophawid / secpday
    else:
        ref_station = stations[ref_station_index - 1]
        
    if n_out > s.schn1.nscans:
        done = True

    else:
        done = False
        found_scan = False
        while not found_scan:
            if k_scan == 1:
                approx_time = scans[0].startj + skip_time
                adjust = False
            elif skip_time != 0:
                approx_time = scans[scan_index - 2].stopj + skip_time
                adjust = False
                if approx_time > scans[0].startj + s.schcon.opdur:
                    done = True
                    break
            else:
                approx_time = scans[scan_index - 2].stopj
                adjust = True

            weight_max = 0
            weight_max_scan = -1
            if s.schcon.opprtlev >= 3:
                s.wlog(0, "              Scan   Source     New scan time "
                       "Opt scan time  TIMEWT  SLEWWT  RISEWT   SETWT   SCNWT "
                       "    dT      WID  SlewSec")

            for j_scan, scan in enumerate(scans[:s.schn1.nscans]):
                if scan_used[j_scan] == 0:
                    scan_weight[j_scan] = 0
                    ref_station.ha1[j_scan], ref_station.el1[j_scan], \
                        ref_station.az1[j_scan], ref_station.lst1[j_scan], \
                        ref_station.pa1[j_scan] = s.schgeo(
                            j_scan + 1, ref_station_index, approx_time)
                    if (ref_station.ha1[j_scan] >= ha_min[j_scan]) and \
                       (ref_station.ha1[j_scan] <= ha_max[j_scan]):
                        at_source = 0
                        for i, station in enumerate(stations):
                            if station.stascn[j_scan]:
                                _, station.tonsrc[j_scan] = s.stageo(
                                    j_scan + 1, i + 1, approx_time, 
                                    last_scan_index[i], "OPTHAS")
                                if (k_scan > 1) and \
                                   (f2str(station.up1[j_scan]) == ""):
                                    at_source = max(at_source, 
                                                    station.tonsrc[j_scan])

                        if k_scan > 1:
                            op_slew = (at_source - scans[scan_index - 2].stopj) \
                                      * secpday
                        else:
                            op_slew = 0

                        source_separation = 1
                        if k_scan > 1:
                            for last_scan in scans[s.schn1.scan1 - 1:
                                                   s.schn1.scanl]:
                                if scan.scnsrc == last_scan.scnsrc:
                                    time_space = approx_time - last_scan.stopj
                                    source_separation = min(source_separation,
                                                            time_space)
                                    
                        if (source_separation < (scan.opminsep * 
                                                 op_ha_wid_t[j_scan])) or \
                            (abs(approx_time - op_ha_t[j_scan]) > 
                             scan.ophmaxdt / secpday):
                            scan_weight[j_scan] = 0
                        else:
                            time_weight = scan.ophawt * \
                                      (0.5 + (1 / math.pi) * math.atan(
                                          1.5 * (approx_time - op_ha_t[j_scan])
                                          / op_ha_wid_t[j_scan]))

                            slew_weight = 0
                            if (scan.scnsrc != scans[scan_index - 2].scnsrc) \
                               and (scan.opslewti > 0):
                                slew_weight = max(
                                    0, scan.opslewwt * (1 - op_slew / 
                                                        scan.opslewti))

                            rise_weight = 0
                            set_weight = 0
                            if scan.ophlimti > 0:
                                rise_weight = max(
                                    0, scan.ophlimwt * (1 - abs(
                                        ref_station.ha1[j_scan] - ha_min[j_scan])
                                                        * 3600 / scan.ophlimti))
                                set_weight = max(
                                    0, scan.ophlimwt * (1 - abs(
                                        ref_station.ha1[j_scan] - ha_max[j_scan])
                                                        * 3600 / scan.ophlimti))

                            scan_weight[j_scan] = time_weight + slew_weight \
                                                  + rise_weight + set_weight
                            if scan_weight[j_scan] > weight_max:
                                weight_max = scan_weight[j_scan]
                                weight_max_scan = j_scan

                            if s.schcon.opprtlev >= 3:
                                year, day1, time_rad = s.timej(approx_time)
                                start_time_text = f2str(s.tformwrp(
                                    time_rad, "T", 0, 2, 2, "::@"))
                                year, day2, time_rad = s.timej(op_ha_t[j_scan])
                                opt_time_text = f2str(s.tformwrp(
                                    time_rad, "T", 0, 2, 2, "::@"))
                                s.wlog(0, " SCAN WEIGHT {:>5d} {:<12} {:>4d} {} "
                                       "{:>4d} {} {:>7.2f} {:>7.2f} {:>7.2f} "
                                       "{:>7.2f} {:>7.2f} {:>7.2f} {:>7.2f} "
                                       "{:>7.2f}".format(
                                           j_scan + 1, scan.scnsrc, 
                                           day1, start_time_text, 
                                           day2, opt_time_text, 
                                           time_weight, slew_weight, 
                                           rise_weight, set_weight, 
                                           scan_weight[j_scan], 
                                           (approx_time - op_ha_t[j_scan]) * 24, 
                                           op_ha_wid_t[j_scan] * 24, op_slew))

            if weight_max_scan >= 0:
                n_out += 1
                scndup(scan_index - 1, weight_max_scan, False, "OPTHAS")
                new_scan = scans[scan_index - 1]
                new_scan.startj = approx_time
                new_scan.stopj = new_scan.startj + new_scan.dur
                scan_used[weight_max_scan] = scan_index
                skip_time = 0
                sd_total += new_scan.dur
                
                if s.schcon.opprtlev >= 2:
                    year, day, time_rad = s.timej(approx_time)
                    time_text = f2str(s.tformwrp(time_rad, "T", 0, 2, 2, "::@"))
                    s.wlog(0, "USE SCAN {:>5d} {} {:>5d} {}   HA={:>10.2f}  "
                           "Weight: {:10.3f}".format(
                               weight_max_scan + 1, 
                               scans[weight_max_scan].scnsrc,
                               day, time_text, ref_station.ha1[weight_max_scan],
                               weight_max))
                found_scan = True
            else:
                skip_time += skip_inc
                n_skip += 1
                if s.schcon.opprtlev >= 1:
                    year, day, time_rad = s.timej(approx_time)
                    time_text = f2str(s.tformwrp(time_rad, "T", 0, 2, 2, "::@"))
                    s.wlog(0, "OPTHAS:  Skipping {:>6.2f} min at {} {:>7.2f} "
                           "min after scan {:>5d} because no scans are "
                           "available.".format(skip_inc * secpday / 60,
                                               time_text, 
                                               skip_time * secpday / 60,
                                               scan_index))

        if found_scan and (s.schcon.opdur > 0) and \
           (new_scan.stopj > scans[0].startj + s.schcon.opdur):
            s.wlog(1, "OPTHAS:  Schedule stopped because requested total "
                   "duration reached")
            done = True

    if done:
        s.wlog(0, " ")
        s.wlog(0, "OPTHAS:  HAS mode schedule made.")

        s.wlog(0, " ")
        if s.schcon.opprtlev <= 0:
            s.wlog(0, "         OPPRTLEV={}:  Minimum information printed.".\
                   format(s.schcon.opprtlev))
        elif s.schcon.opprtlev == 1:
            s.wlog(0, "         OPPRTLEV=1:  Include summary of fate of each "
                   "input scan.")
        elif s.schcon.opprtlev == 2:
            s.wlog(0, "         OPPRTLEV=2:  Minimum feedback as scans chosen "
                   "plus fate of input scans.")
        elif s.schcon.opprtlev >= 3:
            s.wlog(0, "         OPPRTLEV={}:  Details of each scan choice plus "
                   "summaries.".format(s.schcon.opprtlev))

        s.wlog(0, "         Requested scans: {}".format(s.schn1.nscans))
        s.wlog(0, "         Scheduled scans: {}".format(n_out))
        s.wlog(0, "         Sum of requested durations: {:>7.3f} hr.".format(
            dur_total * 24))
        s.wlog(0, "         Sum of scheduled durations: {:>7.3f} hr.".format(
            sd_total * 24))

        year, day, time_rad = s.timej(scans[s.schn1.scan1 - 1].startj)
        time_text = f2str(s.tformwrp(time_rad, "T", 0, 2, 2, "::@"))
        s.wlog(0, "         Start day and time: {:>5d} {}".format(
            day, time_text))

        year, day, time_rad = s.timej(scans[s.schn1.scanl - 1].stopj)
        time_text = f2str(s.tformwrp(time_rad, "T", 0, 2, 2, "::@"))
        s.wlog(0, "         Stop day and time and total duration (hr): {:>5d} {}"
               " {:>12.3f}".format(day, time_text, 
                                   (scans[s.schn1.scanl - 1].stopj - 
                                    scans[s.schn1.scan1 - 1].startj) * 24))

        s.wlog(0, "         Requested duration (hours) {:>8.2f}".format(
            s.schcon.opdur * 24))

        s.wlog(0, "         Maximum allowed deviation from optimum time "
               "(hr; scan 1) {:>8.3f}".format(scans[0].ophmaxdt / 3600))

        s.wlog(0, "         Number of {:>7.3f} minutes skips because of no scan "
               "available: {}".format(skip_inc * secpday / 60, n_skip))
        
        s.wlog(0, "         Total time in skips: {:>8.3f} minutes.  "
               "(Some may be after last scan)".format(
                   skip_inc * n_skip * secpday / 60))
        
        if s.schcon.opprtlev >= 1:
            s.wlog(0, " ")
            s.wlog(0, "OPTHAS:  Input scan information.  Columns are:")
            s.wlog(0, "         Output scan number and time ")
            s.wlog(0, "         Reference station hour angle.  "
                   "for scheduled scan.")
            s.wlog(0, "         Allowed reference station hour angle range "
                   "(for scan start).")
            s.wlog(0, "         Reference station hour angles for beginning "
                   "and end of observations.")
            s.wlog(0, "         Total hours this source is available.")
            s.wlog(0, "         The reference station is: {}".format(
                ref_station.station))
            s.wlog(0, "  Input    Source    Output      Time          HA        "
                   "Time        HA      HA      HA      HA  Available     "
                   "Rise         Set")
            s.wlog(0, "   Scan               Scan   Day    hms       "
                   "Scan        Opt       Min     Max    Start    End    Hours")

            for j, scan in enumerate(scans[:s.schn1.nscans]):
                if scan_used[j] != 0:
                    year, day1, time_rad = s.timej(scans[scan_used[j] - 1].startj)
                    start_time_text = f2str(
                        s.tformwrp(time_rad, "T", 0, 2, 2, "::@"))
                    ha_p = ref_station.ha1[scan_used[j] - 1]
                else:
                    day1 = 0
                    start_time_text = " "
                    ha_p = 0

                year, day2, time_rad = s.timej(op_ha_t[j])
                opt_time_text = f2str(s.tformwrp(time_rad, "T", 0, 2, 2, "::@"))
                year, day_rise, time_rad = s.timej(t_ha_min[j])
                rise_time_text = f2str(s.tformwrp(time_rad, "T", 0, 2, 2, "::@"))
                year, day_set, time_rad = s.timej(t_ha_max[j])
                set_time_text = f2str(s.tformwrp(time_rad, "T", 0, 2, 2, "::@"))
                s.wlog(0, "{:>5d}   {:<12} {:>5d} {:>5d}  {:<8} {:>8.3f} {:>4d}"
                       "  {} {:>7.3f} {:>7.3f} {:>7.3f} {:>7.3f} {:>7.3f} {:>4d}"
                       " {} {:>4d} {}".format(
                           j + 1, scan.scnsrc, scan_used[j], day1, 
                           start_time_text, ha_p, day2, opt_time_text, ha_min[j],
                           ha_max[j], ha_begin[j], ha_end[j], t_avail[j] * 24, 
                           day_rise, rise_time_text, day_set, set_time_text))

        s.wlog(1, "OPTHAS: There were {} requested scans and "
               "{} Scheduled scans.".format(s.schn1.nscans, n_out))
        s.wlog(1, "OPTHAS: See the sched.runlog for details.")

    return adjust, True, done

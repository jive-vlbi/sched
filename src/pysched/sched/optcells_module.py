from . import scndup
from ..catalog import ScanCatalog, StationCatalog
from ..util import f2str

import schedlib as s

import numpy as np

import math
import itertools

max_input_scans = 1000
cell_time = np.empty(dtype=float, shape=(3, 3, StationCatalog.maxsta))
last_j_scan = 0
az_coff = 60
el_cell = [20, 45]
az_cell = [120, 240]
def optcells(last_scan_index, k_scan, scan_index):
    global cell_time, last_j_scan, weight_station

    if s.schcon.debug:
        s.wlog(1, "Starting OPTCELLS")

    done = False
    keep = True
    adjust = True
    
    stations = StationCatalog().used(use_direct_access=True)
    scans = ScanCatalog().direct_access_entries
    
    if k_scan == 1:
        if s.schn1.nscans > max_input_scans:
            s.errlog(" OPTCELLS:  Too many input scans for cell optimizer "
                     "- max: {}".format(max_input_scans))
        
        if s.schcon.opdur <= 0:
            s.errlog(" OPTCELLS: For OPTMODE=CELLS, OPDUR must be given.")

        adjust = False
        scans[scan_index - 1].startj = scans[0].startj
        approx_time = scans[scan_index - 1].startj
        last_j_scan = 0

        cell_time.fill(scans[0].startj - 0.5 / 24)
        baseline = np.zeros(dtype=float, shape=(len(stations), len(stations)))
        for (i, station1), (j, station2) in itertools.combinations(
                enumerate(stations), 2):
            baseline[i,j] = math.sqrt((station1.xpos - station2.xpos) ** 2 +
                                      (station1.ypos - station2.ypos) ** 2 +
                                      (station1.zpos - station2.zpos) ** 2)
        baseline += baseline.T
        mean_base = np.sum(baseline) / (len(stations) * (len(stations) - 1))
        weight_station = np.square(np.sum(baseline, axis=1) / 
                                   ((len(stations) - 1) * mean_base))
    else:
        approx_time = scans[scan_index - 2].stopj
    
    got_station = np.zeros(dtype=int, shape=(s.schn1.nscans,))
    ok_station = np.full(fill_value=False, dtype=bool, 
                         shape=(s.schn1.nscans, len(stations)))
    el_cell_index = np.empty(dtype=int, shape=(s.schn1.nscans, len(stations)))
    az_cell_index = np.empty(dtype=int, shape=(s.schn1.nscans, len(stations)))
    time_source = np.empty(dtype=float, shape=(s.schn1.nscans,))
    points = np.zeros(dtype=float, shape=(s.schn1.nscans,))
    for j_scan, scan in enumerate(scans[:s.schn1.nscans]):
        if j_scan + 1 != last_j_scan:
            for i, station in enumerate(stations):
                if station.stascn[j_scan]:
                    last_time, station.tonsrc[j_scan] = s.stageo(
                        j_scan + 1, i + 1, approx_time, last_scan_index[i],
                        "OPTCELLS")
                    if (f2str(station.up1[j_scan]) == "") and \
                       (f2str(station.up2[j_scan]) == "") and \
                       (station.el1[j_scan] > scan.opminel) and \
                       (station.el2[j_scan] > scan.opminel):
                        ok_station[j_scan, i] = True
                        got_station[j_scan] += 1

                        az_test = (station.az1[j_scan] + az_coff) % 360
                        
                        if station.el1[j_scan] < el_cell[0]:
                            el_cell_index[j_scan, i] = 1
                        elif station.el1[j_scan] < el_cell[1]:
                            el_cell_index[j_scan, i] = 2
                        else:
                            el_cell_index[j_scan, i] = 3

                        if el_cell_index[j_scan, i] == 3:
                            az_cell_index[j_scan, i] = 1
                        else:
                            if az_test < az_cell[0]:
                                az_cell_index[j_scan, i] = 1
                            elif az_test < az_cell[1]:
                                az_cell_index[j_scan, i] = 2
                            else:
                                az_cell_index[j_scan, i] = 3
            
            time_source[j_scan] = 1e10                
            all_0 = True
            for i, station in enumerate(stations):
                if ok_station[j_scan, i] and (last_scan_index[i] != 0):
                    all_0 = False
                    time_source[j_scan] = min(time_source[j_scan], 
                                              station.tonsrc[j_scan])
                    if s.schcon.opnosub and (scan_index > s.schn1.scan1):
                        time_source[j_scan] = max(time_source[j_scan], 
                                                  scans[scan_index - 2].stopj)
            if all_0:
                time_source[j_scan] = scans[0].startj

            if got_station[j_scan] >= scan.opmian:
                for i, station in enumerate(stations):
                    if ok_station[j_scan, i]:
                        eci = el_cell_index[j_scan, i]
                        aci = az_cell_index[j_scan, i]
                        
                        weight_multiplier = 1
                        if eci == 1:
                            for iaz in range(1, 4):
                                if iaz != aci:
                                    pt_add = 1440 * \
                                             (time_source[j_scan] - 
                                              cell_time[eci - 1, iaz - 1, i])
                                    weight_multiplier += pt_add ** 2 / \
                                        (s.schcon.optlowt ** 2 + pt_add ** 2)

                        lc_time = cell_time[eci - 1, aci - 1, i]
                        
                        pt_add = 1440 * (time_source[j_scan] - lc_time)
                        points[j_scan] += weight_station[i] * pt_add * \
                                          weight_multiplier
    
    max_points = np.max(points)

    time1 = 1e10
    for j_scan, scan in enumerate(scans[:s.schn1.nscans]):
        if got_station[j_scan] >= scan.opmian:
            time1 = min(time1, time_source[j_scan])

    for j_scan, scan in enumerate(scans[:s.schn1.nscans]):
        if got_station[j_scan] >= scan.opmian:
            sl_adj = 1440 * (time_source[j_scan] - time1)
            weight_multiplier = s.schcon.optslew ** 2 / (sl_adj ** 2 +
                                                         s.schcon.optslew ** 2)
            points *= weight_multiplier

    look_back = 6
    station_skip = np.zeros(dtype=int, shape=(len(stations),))
    if scan_index > s.schn1.scan1 + 2:
        i_skip1 = max(scan_index - look_back, s.schn1.scan1)
        for l, l_scan in enumerate(scans[i_skip1 - 1: scan_index - 1]):
            for i, station in enumerate(stations):
                if not (station.stascn[l] and (station.el1[l] > l_scan.opminel)):
                    station_skip[i] += 1

    for j, scan in enumerate(scans[:s.schn1.nscans]):
        max_skip = 0
        for i, station in enumerate(stations):
            if not (station.stascn[j_scan] and 
                    (station.el1[j_scan] > scan.opminel)):
                max_skip = max(max_skip, station_skip[i])

        if max_skip > look_back // 3:
            points[j_scan] *= 0.2

    max_scan = np.argmax(points)
    max_apt = points[max_scan]
    if max_apt > -1e10:
        j_scan = max_scan + 1
    else:
        s.errlog("OPTCELLS: Something weird went wrong with POINTS.")

    scndup(scan_index - 1, j_scan - 1, False, "OPTCELLS")
    scan = scans[scan_index - 1]
    scan.startj = time_source[j_scan - 1]
    scan.stopj = scan.startj + scan.dur

    last_j_scan = j_scan

    year, day, time_rad = s.timej(scan.startj)
    time_text = f2str(s.tformwrp(time_rad, "T", 0, 2, 2, "::@"))
    s.wlog(0, " Source, time, maxpts, maxapt: {:<12} {:>4d} {} "
           "{:>9.3f} {:>9.3f}".format(
               scan.scnsrc, day, time_text, max_points, max_apt))

    for i, station in enumerate(stations):
        if ok_station[j_scan - 1, i]:
            cell_time[el_cell_index[j_scan - 1, i] - 1, 
                      az_cell_index[j_scan - 1, i] - 1,
                      i] = scan.startj
            station.stascn[scan_index - 1] = True
        else:
            station.stascn[scan_index - 1] = False

    return adjust, keep, done

from .sched.parameter import secpday

import schedlib as s

# 22 mins of 2 Gbps recording (allow factor 1.008 for headers).
max_disk_unit = 22.*60. * 2.016 * (1024./1000.) / 8.
gap_seconds = 10
minimum_scan_seconds = 15
minimum_mode_change_gap_seconds = 40
maximum_tsys_gap_seconds = 15 * 60

def apply_tape_offset(scans, scan_offset, stations, setups):
    for scan_index, scan in enumerate(scans, scan_offset):
        stations_in_scan = [station for station in stations
                            if station.stascn[scan_index]]
        tape_starts = {station.tpstart[scan_index] 
                       for station in stations_in_scan}
        if len(tape_starts) == 0:
            continue
        if len(tape_starts) > 1:
            # -1 and +1: from FORTRAN index <-> python index
            min_pause = max(
                station.tpstart[scan_index]
                / setup[station.nsetup[scan_index]-1].speedup 
                for station in stations_in_scan)
            s.prtscn(scan_index + 1, "VXSCHK")
            s.errlog("Station tape starts differ, set MINPAUSE to {}s to "
                     "produce VEX file!".format(round(min_pause * secpday)))
        
        tape_start = tape_starts.pop()
        scan.startj -= tape_start
        
def is_field_system_station(station):
    return (station.control == "VEX") \
        and (not station.station.startswith("VLA"))

def check_recording_sizes(scans, scan_offset, stations):
    """
    Print warnings for large continuous recordings.
    """
    # gather the scan indices that trigger the warnings
    scan_warnings = set()
    for station in (s for s in stations
                    if is_field_system_station(s) 
                    and s.disk != "LBADR"
                    and s.usedisk):
        previous_scan_index = None
        previous_gbytes = 0
        for scan_index, scan in ((i, s) 
                                 for i, s in enumerate(scans, scan_offset)
                                 if station.stascn[i]):
            if previous_scan_index is not None:
                previous_scan = scans[previous_scan_index - scan_offset]
                if (scan.startj - previous_scan.stopj) * secpday > gap_seconds:
                    previous_gbytes = station.gbytes[previous_scan_index]
            
            gbytes = station.gbytes[scan_index]
            if gbytes - previous_gbytes > max_disk_unit:
                scan_warnings.add(scan_index)
            
            previous_scan_index = scan_index

    # print messages, do not print a new message for consecutive scans
    previous_scan_index = -42 # any value that triggers the check below
    for scan_index in sorted(scan_warnings):
        if scan_index != previous_scan_index + 1:
            s.wlog(0, "The scan detailed below has exceeded the limit for "
                   "continuous recording. Insert a gap before this scan, or "
                   "reduce its length if necessary:")
            s.prtscn(scan_index + 1, "VXSCH")
            s.wlog(0, " ")

        previous_scan_index = scan_index
        
    if len(scan_warnings) > 0:
        s.wrtmsg(1, "VXSCH", "warnbank")


def check_minimum_scan_duration(scans, scan_offset, stations):
    """
    Returns whether any scan failed the minimum duration check.
    """
    # only check certain stations, mimic SCHED here
    stations = [s for s in stations
                if is_field_system_station(s) 
                and s.disk != "LBADR"
                and s.usedisk]
    any_warning = False
    for scan_index, scan in enumerate(scans, scan_offset):
        if any(station.stascn[scan_index] for station in stations) \
           and ((scan.stopj - scan.startj) * secpday < minimum_scan_seconds):
            stations_string = ", ".join(station.station for station in stations
                                        if station.stascn[scan_index])
            s.prtscn(scan_index + 1, "VXSCHK")
            s.wlog(1, "WARNING: Scan length < {l}s for station(s) {s}.\n"
                   "         Currently FS supports minimal "
                   "scan length of {l}s".format(s=stations_string,
                                                l=minimum_scan_seconds))
            any_warning = True
    return any_warning

def check_scan_overlap(scans, scan_offset, stations):
    """
    Returns whether any scan failed the scan overlap check.
    """
    any_warning = False
    scan_pairs = zip(scans[:-1], scans[1:])
    for scan_index, (scan, next_scan) in enumerate(scan_pairs, scan_offset):
        if any(station.stascn[scan_index] and station.stascn[scan_index + 1]
               for station in stations) \
            and (next_scan.startj < scan.stopj):
            stations_string = ", ".join(station.station for station in stations
                                        if station.stascn[scan_index]
                                        and station.stascn[scan_index + 1])
            # print warning for next scan, +1 for next, +1 for FORTRAN indexing
            s.prtscn(scan_index + 2, "VXSCHK")
            s.wlog(1, "WARNING: Tape early start failed for stations(s) {}.".\
                   format(s=stations_string))
            s.wlog(1, "         Early tape starts only work if there are "
                   "sufficient gaps.")
            any_warning = True
    return any_warning

def check_mode_change(scans, scan_offset, stations, scan_mode):
    """
    scan_mode: {index in scans: mode name}
    Returns whether any scan failed the mode change gap check.
    """
    stations = [s for s in stations if is_field_system_station(s)]
    any_warning = False
    scan_pairs = zip(scans[:-1], scans[1:])
    for scan_index, (scan, next_scan) in enumerate(scan_pairs, scan_offset):
        if any(station.stascn[scan_index] and station.stascn[scan_index + 1]
               for station in stations) \
            and (scan_mode[scan_index - scan_offset] != 
                 scan_mode[scan_index - scan_offset + 1]) \
            and ((next_scan.startj - scan.stopj) * secpday 
                 < minimum_mode_change_gap_seconds):
            stations_string = ", ".join(station.station for station in stations
                                        if station.stascn[scan_index]
                                        and station.stascn[scan_index + 1])
            # print warning for next scan, +1 for next, +1 for FORTRAN indexing
            s.prtscn(scan_index + 2, "VXSCHK")
            s.wlog(1, "WARNING: Mode setup <= {l}s for station(s) {s}.\n"
                   "         FS Stations once needed {l}s for any mode "
                   "change, incl frequency shift.".format(
                       s=stations_string, l=minimum_mode_change_gap_seconds))
            s.wlog(1, "         This may no longer be true but the new value "
                   "is not yet clear.")
            any_warning = True
    return any_warning

def check_tsys(scans, scan_offset, stations, setups, frequency_setups):
    # TSCAL in the frequency setup will override the TSCAL of the station
    # (if present)

    tsys_warnings = []
    tsys_off_source_warnings = []
    for station in stations:
        last_stop = 0 # any MJD that will trigger a gap to the next scan
        n_tsys = 0
        n_tsys_on = 0
        last_tsys = scans[0].startj
        last_tsys_on_source = scans[0].startj
        warn_tsys_off_source = False
        max_tsys_gap = 0
        for scan_index, scan in enumerate(scans, scan_offset):
            if not station.stascn[scan_index]:
                continue
            scan_tscal = station.tscal
            # -1: FORTRAN -> python indexing
            setup = setups[station.nsetup[scan_index] - 1]
            # get the frequency setup index from the first channel,
            # this assumes they are all equal (as the SCHED code should enforce)
            if (setup.ifreqnum[0] >= 1):
                frequency_tscal = frequency_setups[setup.ifreqnum[0] -1].tscal
                if frequency_tscal is not None:
                    scan_tscal = frequency_tscal
            if scan_tscal == "CONT":
                # update last tsys measurement variables
                last_tsys = scan.stopj
                if station.tonsrc[scan_index] < scan.stopj:
                    last_tsys_on_source = scan.stopj
                continue
            
            # scan tscal is GAP, check interval between (on source) measurements
            if (scan.startj - last_stop) * secpday > gap_seconds:
                # a tsys measurement in this scan
                n_tsys += 1
                last_tsys = scan.startj
                on_source = round((station.tonsrc[scan_index] - scan.startj)
                                  * secpday)
                if on_source <= 0:
                    last_tsys_on_source = scan.startj
                    n_tsys_on += 1
                elif (scan.stopj - last_tsys_on_source) * secpday \
                     > maximum_tsys_gap_seconds:
                    warn_tsys_off_source = True

            tsys_gap = round((scan.stopj - last_tsys) * secpday)
            if tsys_gap > maximum_tsys_gap_seconds:
                max_tsys_gap = max(max_tsys_gap, tsys_gap)
                
            last_stop = scan.stopj

        if max_tsys_gap > 0:
            tsys_warnings.append(
                "{:<8} has {:>4} Tsys measurements. Maximum interval = {:>4} "
                "minutes.".format(station.station, n_tsys, 
                                  round(float(max_tsys_gap) / 60)))

        if warn_tsys_off_source:
            tsys_off_source_warnings.append(
                "{:<8}: only {:>4} out of {:>4} Tsys measurements "
                "are on-source".format(station.station, n_tsys_on, n_tsys))

    if len(tsys_warnings) > 0:
        for text in tsys_warnings:
            s.wlog(1, text)
        s.wlog(1, 
               "Tsys calibration at most MkIV stations is taken during "
               "every gap in recording,\n"
               "but these appear over {l} min apart for the stations listed "
               "above!\n"
               "This can be improved by inserting gaps at regular "
               "intervals.\n"
               "Note this is not an issue for Westerbork or Arecibo.".format(
                   l=round(maximum_tsys_gap_seconds/60)))
        s.wrtmsg(0, "VXSCH", "tsysgap")

    if len(tsys_off_source_warnings) > 0:
        for text in tsys_off_source_warnings:
            s.wlog(1, text)
        s.wlog(1, "Stations listed above are affected by slewing during "
               "Tsys calibration")
        s.wrtmsg(0, "VXSCH", "tsysoffsrc")

        
        

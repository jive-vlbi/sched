from .parameter import onesec, secpday, raddeg
from ..catalog import ScanCatalog, StationCatalog, SetupFileCatalog, \
    SetupCatalog, FrequencyCatalog, FrequencySetCatalog, SourceCatalog
from ..util import f2str
from ..vex import sched_version as vex_version
from ..version import pysched_version
from .output_files import write_cover, format_float, write_freq_line, \
    round_seconds, sun_warning, write_setup, write_sources, source_alias, \
    write_solar_corona_warnings

import schedlib as s

from astropy.time import Time
from astropy.coordinates import SkyCoord
import numpy

import os
import os.path
import math
from collections import OrderedDict
import itertools

def schsum(restart):
    if s.schcon.debug:
        s.wlog(0, "SCHSUM: Starting.")

    sum_filename = f"{f2str(s.schc1.expcode).lower()}.sum"
    if s.schcon.debug:
        s.wlog(0, f"SUMOPE: Opening {sum_filename}")

    if restart or s.schcon.overwrit:
        open_mode = "w"
    else:
        open_mode = "x"

    try:
        sum_file = open(sum_filename, open_mode)
    except FileExistsError:
        s.wlog(1, f"SUMOPE: {sum_filename} already exists.")
        s.errlog("SUMOPE: You need to delete old output files or use OVERWRIT.")
    except Exception as e:
        s.errlog(f" Open problem: {e}")

    with sum_file:
        s.wlog(1, f"SUMOPE:  Writing summary file {sum_filename}")
        
        scan_catalog = ScanCatalog()
        scan_catalog.read()
        scans = scan_catalog.used()
        
        station_catalog = StationCatalog()
        station_catalog.read()
        station_catalog.read_scheduled_attributes()
        stations = station_catalog.used()
        
        setup_file_catalog = SetupFileCatalog()
        setup_files = setup_file_catalog.read()

        setup_catalog = SetupCatalog()
        setups = setup_catalog.read()

        frequency_catalog = FrequencyCatalog()
        frequencies = frequency_catalog.read()

        frequency_set_catalog = FrequencySetCatalog()
        frequency_sets = frequency_set_catalog.read()

        source_catalog = SourceCatalog()
        sources = source_catalog.read()

        mjd = int(scans[0].startj)
        start_date = Time(mjd, format='mjd').datetime
        day_of_year = start_date.strftime("%-j")
        day_of_month = start_date.strftime("%-d")
        print_date = start_date.strftime(
            "Day {: >3} is %a  {: >2} %b %Y   MJD  {}          ").format(
                day_of_year, day_of_month, mjd)
    
        write_intro(sum_file, scans, print_date)

        write_preempt(sum_file, scans, scan_catalog.scan_offset, stations,
                      restart)

        write_correlator(sum_file, scans, scan_catalog.scan_offset, setup_files)

        write_stations(sum_file, stations, scans)

        write_summary(sum_file, scans, scan_catalog.scan_offset, stations,
                      setup_files)

        write_setups(sum_file, setups, stations, frequencies, frequency_sets,
                    scans, scan_catalog.scan_offset)

        write_scans(sum_file, scans, scan_catalog.scan_offset, stations,
                    setup_files, sources, print_date)

        write_record_summary(sum_file, stations, scans,
                             scan_catalog.scan_offset)

        write_sources(sum_file, sources, setup_files, True)

        write_solar_corona_warnings(sum_file, scans, scan_catalog.scan_offset,
                                    sources, setup_files)

        write_separations(sum_file, sources)

        write_versions(sum_file)
        
        
def write_intro(sum_file, scans, print_date):
    exp_code = f2str(s.schc1.expcode)
    experiment = f2str(s.schc1.expt)
    
    sum_file.write(f"""
                    SUMMARY FILE FOR PROJECT: {exp_code: <8}

COVER INFORMATION: 

  Experiment: {experiment}
  Exp. Code:  {exp_code: <8}
  Start {print_date}
"""[1:])

    write_cover(sum_file, "  ", True)

minimum_preempt_time = 1.6666 / 24
max_block = 4 / 24
def write_preempt(sum_file, scans, scan_offset, stations, restart):
    if s.schcon.debug:
        s.wlog(0, "PROTECT starting")

    for index, scan in enumerate(scans, scan_offset + 1):
        if scan.preempt not in {"OK", "NO", "EXTRA"}:
            s.errlog(f"PROTECT:  Invalid input PREEMPT={scan.preempt} on scan "
                     f"{index}. Must be OK, NO or EXTRA.")

    # If the the VLBA is not in use and no PREEMPT=EXTRA scans were specified,
    # get out of here silently.  This routine will only cause confusion.
    if not s.schn1.gotvlba and not s.schn1.fuzzy:
        return

    pt_mk = [station for station in stations 
             if station.station in {"VLBA_PT", "VLBA_MK"}]
    if len(pt_mk) == 0:
        s.wlog(1, "PROTECT:  No PT or MK, so will not check that times "
               "preemptable for USNO are present.")
        if not s.schn1.fuzzy:
            return

    pt_mk_in_scan = [any(station.stascn[scan_index] for station in pt_mk)
                     for scan_index in range(scan_offset, s.schn1.scanl)]

    preempt_filename = f"{f2str(s.schc1.expcode).lower()}.preempt"

    if restart or s.schcon.overwrit:
        open_mode = "w"
    else:
        open_mode = "x"

    try:
        preempt_file = open(preempt_filename, open_mode)
    except FileExistsError:
        s.wlog(1, f"PROTECT: {preempt_filename} already exists.")
        s.errlog("PROTECT: You need to delete old output files or "
                 "use OVERWRIT.")
    except Exception as e:
        s.errlog(f" PROTECT open problem: {e}")

    with preempt_file:
        first_core_scan_index = next(
            (scan_index for scan_index, scan in enumerate(scans)
             if scan.preempt != "EXTRA"),
            None)
        last_core_scan_index = next(
            (len(scans) - scan_index - 1
             for scan_index, scan in enumerate(reversed(scans))
             if scan.preempt != "EXTRA"),
            None)
        
        if first_core_scan_index is None:
            s.errlog("PROTECT:  You must have some scans with PREEMPT "
                     "not set to EXTRA.")
        
        for scan_index, scan in enumerate(
                scans[first_core_scan_index : last_core_scan_index + 1],
                first_core_scan_index):
            if scan.preempt == "EXTRA":
                s.wlog(1, "PROTECT: PREEMPT=EXTRA is only allowed in blocks "
                       "at the start and end.")
                s.wlog(1, f"Scan {scan_index + scan_offset} is in the core and "
                       "is EXTRA.")
                s.errlog("Please change PREEMPT for the offending scan.")


        # Collect the first and last times of protected data (the core), 
        # and the times of the first and last scans of the core 
        # (not scan.preempt=EXTRA).  The overall experiment first and 
        # last times are already in TFIRST and TEND.
        core_start_time = min(scan.startj for scan in scans[
            first_core_scan_index : last_core_scan_index + 1])
        core_end_time   = max(scan.stopj for scan in scans[
            first_core_scan_index : last_core_scan_index + 1])

        protected_scans = [scan for scan_index, scan in enumerate(scans)
                           if ((scan.preempt == "NO") and
                               pt_mk_in_scan[scan_index])]
        if len(protected_scans) == 0:
            protected_start = s.schn1.tend
            protected_end   = s.schn1.tfirst
        else:
            protected_start = min(scan.startj for scan in protected_scans)
            protected_end   = max(scan.stopj  for scan in protected_scans)

        # Now look for blocks of preemptable times in a manner that works
        # with scans not in time order.
        preempt_ranges = []
        
        # First see if there is a block at the start of non-zero length.
        if protected_start > s.schn1.tfirst:
            preempt_ranges.append((s.schn1.tfirst, protected_start))

        # Now look for other good periods based on scan times.
        # Assume that any available block other than the first (which
        # has already been specified) will start with the end of
        # a protected scan.  Then see if such a block is long enough.
        # If not, ignore it.  If so, add it to the list.  Note that
        # there will be no further attempts to start a block in that
        # gap because, by definition, there aren't any. 
        # It is just possible, in very weird circumstances, that 
        # finding the open periods this way will result in blocks out 
        # of time order.  In this process, ignore scans without PT or MK.
        # Don't do this for the last scan (in terms of time) as it 
        # cannot start a valid interval.
        for scan_index, scan in enumerate(scans):
            if ((scan.preempt == "NO") and pt_mk_in_scan[scan_index] and
                scan.stopj != s.schn1.tend):
                preempt_start = scan.stopj
                preempt_end = s.schn1.tend
                add_range = True
                for inner_scan_index, inner_scan in enumerate(scans):
                    if ((inner_scan_index != scan_index) and
                        (inner_scan.preempt == "NO") and
                        pt_mk_in_scan[inner_scan_index] and
                        (inner_scan.stopj > preempt_start)):
                        if (inner_scan.startj <
                            (preempt_start + minimum_preempt_time)):
                            add_range = False
                            break
                        preempt_end = min(inner_scan.startj, preempt_end)
                if add_range:
                    preempt_ranges.append((preempt_start, preempt_end))

        if len(preempt_ranges) == 0:
            add_end = (abs(protected_end - s.schn1.tend) > onesec)
        else:
            add_end = ((abs(preempt_ranges[-1][1] - s.schn1.tend) > onesec) and
                       (abs(protected_end - s.schn1.tend) > onesec))

        if add_end:
            preempt_ranges.append((protected_end, s.schn1.tend))

        def write_files(text):
            sum_file.write(text)
            preempt_file.write(text)

        def time_range_text(start, end):
            start = round_seconds(Time(start, format="mjd").datetime)
            end = round_seconds(Time(end, format="mjd").datetime)
            start_day_of_year = start.strftime("%-j")
            start_time = start.strftime("%H:%M:%S")
            end_day_of_year = end.strftime("%-j")
            end_time = end.strftime("%H:%M:%S")
            return f"({start_day_of_year:0>3}) {start_time}"\
                f"  to  ({end_day_of_year:0>3}) {end_time}"

        sum_file.write(" \n \n")
        write_files("""
ALLOWED PREEMPTION TIMES
 
 Dynamically scheduled VLBA projects can have optional scans at                 
 the start and end designated by PREEMPT='EXTRA'.  These can be                 
 used or not used to help with efficient meshing with previous                  
 and following projects.  Only the core time range will be used                 
 for the initial dynamic project selection.  DOSCANScan be used to              
 restrict the scan range sent to the .vex, .oms, crd., sch., and                
 .flag files.                                                                   
 
 The schedule can also designate which scans can be preempted at                
 PT and MK for daily USNO EOP observations using PREEMPT='OK'                   
 or 'NO'.  For details, see the manual discussion of PREEMPT.                   
 
"""[1:])

        # Deal with the case where the entire project is protected.  Complain,
        # but then be forgiving if the project is short.  This information also
        #  goes to the screen and logfile.
        ignore_warning = False
        if len(preempt_ranges) == 0:
            text = (" *******\n"
                    " ******* This schedule has no times that can be "
                    "preempted for EOP observations. ")
            write_files(text + "\n")
            s.wlog(1, text)
            
            if (s.schn1.tend - s.schn1.tfirst) <= max_block:
                #  Deal with short projects.
                text = f"""
 ******* But the project is less than {max_block * 24:5.1f} hours long so this may not be a     
 ******* problem unless 2 such runs are scheduled back to back.                 
 ******* If that happens, your protection requests may be ignored.              
 *******                                                                        """[1:]
                write_files(text + "\n")
                for line in text.split("\n"):
                    s.wlog(1, line.rstrip())
            else:
                # Now flag the case of a long project with no preemption times.
                ignore_warning = True

        else:
            write_files(" Time ranges available for USNO daily EOP "
                        "observations at PT and MK:            \n")
            for index, (preempt_start, preempt_end) in \
                enumerate(preempt_ranges, 1):
                write_files(f"{index: >3}    "
                            f"{time_range_text(preempt_start, preempt_end)}\n")

            # Check for adequate options.  Look for, and complain about, 
            # protected periods of MAX_BLOCK hours or more.  This assumes
            # time order, but that should be the case given how the blocks
            # were derived.

            too_long = False
            if (preempt_ranges[0][0] - s.schn1.tfirst) > max_block:
                too_long = True
            if (s.schn1.tend - preempt_ranges[-1][1]) > max_block:
                too_long = Ture
            if any(next_range[0] - prev_range[1] > max_block
                   for next_range, prev_range in
                   zip(preempt_ranges[:-1], preempt_ranges[1:])):
                too_long = True

            if too_long:
                text = f"""
 *******
 ******* There is at least one block of over {max_block * 24:5.1f} hr
 ******* when your PREEMPT specification does not allow {mininum_preempt_time * 24:5.1f} hr
 ******* EOP observations on one baseline (PT-MK usually) to be inserted.
 ******* See details in the .sum or .preempt file."""[1:]
                write_files(text + "\n")
                s.wlog(1, text)
                ignore_warning = True

        if ignore_warning:
            text = f"""
 ******* Your requests to protect scans will be ignored.
 ******* See the SCHED manual info on parameter PREEMPT.
 *******"""[1:]
            write_files(text + "\n")
            s.wlog(1, text)

        write_files(""" 
 Project times summary:                                                         
""")
            
        if s.schn1.tfirst < core_start_time:
            write_files(" Extra scans at start:        "
                        f"{time_range_text(s.schn1.tfirst, core_start_time)}"
                        "                \n")
        write_files(" Core start and stop:         "
                    f"{time_range_text(core_start_time, core_end_time)}"
                    "                \n")
        if s.schn1.tend > core_end_time:
            write_files(" Extra scans at end:          "
                        f"{time_range_text(core_end_time, s.schn1.tend)}"
                        "                \n")
        sum_file.write(" \n")

        if s.schn1.doscans[0] != 0:
            start_scan = scans[s.schn1.doscans[0] - scan_offset - 1]
            end_scan   = scans[s.schn1.doscans[1] - scan_offset - 1]
            write_files(f"""
 DOSCANS was set to only pass scans {s.schn1.doscans[0]: >5} to {s.schn1.doscans[1]: >5}                              
 DOSCANS time range:          {time_range_text(start_scan.startj, end_scan.stopj)}                
"""[1:])

        
def write_correlator(sum_file, scans, scan_offset, setup_files):
    if s.schn1.notape:
        sum_file.write("\nNo recordings.  Correlation requests ignored.\n")
        return

    sum_file.write(f"\n{f2str(s.schsco.corstuff[0])}\n")

    if s.schsou.npair > 0:
        sum_file.write(f"\n  THIS PROJECT USES MULTIPLE PHASE CENTERS.\n")

        if s.schco.corfft < 4000:
            s.wlog(1, "CORLST:   WARNING - your number of channels in "
                   "the correlator FFT may not ")
            s.wlog(1, "          be adequate for multiple phase centers.")
            s.wlog(1, "          You have specified CORCHAN(2) = "
                   f"{s.schco.corfft}")
            s.wlog(1, "          More typical is several thousand to avoid "
                   "smearing on offset phase centers.")
            s.wlog(1, "          Please consider CORCHAN(2) carefully.")
            s.wlog(1, "          See the SCHED manual sections on multiple "
                   "phase centers and on parameter CORCHAN.")
            sum_file.write(
                "    Worry about possible inadequate FFT channels.\n")

    paragraph_line_number = [1, 10, 15, 23]
    text = "\n\n".join("\n".join(f2str(line)
                                 for line in s.schsco.corstuff[start:end])
                       for start, end in zip(paragraph_line_number[:-1],
                                             paragraph_line_number[1:]))
    sum_file.write(f"\n{text}\n")

    experiment_scans = [t for t in enumerate(scans, scan_offset)]
    core_scans = [t for t in experiment_scans if t[1].preempt != "EXTRA"]
    do_scans = [t for t in experiment_scans
                if s.schn1.doscans[0] <= (t[0] + 1) <= s.schn1.doscans[1]]
    number_of_stations = {scan_index: sum(s.schn2a.stascn[scan_index, :])
                          for scan_index, _ in experiment_scans}
    number_of_baselines = {scan_index: n * (n - 1) // 2 + n
                           for scan_index, n in number_of_stations.items()}
    scan_setup_file = {scan_index: setup_files[scan.setnum - 1]
                       for scan_index, scan in experiment_scans}
    if (s.schco.coravg == 0) or s.schcon.noset:
        data_rate = {scan_index: 0 for scan_index, _ in experiment_scans}
    else:
        n_pols = 2 if s.schco.corpol else 1
        data_rate = {scan_index: 4 * n * (n + 1) *
                     scan_setup_file[scan_index].mschn * s.schco.corchan *
                     n_pols * scan_setup_file[scan_index].fspeed /
                     s.schco.coravg
                     for scan_index, n in number_of_stations.items()}
    
    scan_time = {scan_index: (scan.stopj - scan.startj) * secpday
                 for scan_index, scan in experiment_scans}

    sum_file.write("\n\n\nDERIVED INFORMATION FOR CORRELATION: \n")
    text_lines = """
                                               
  Elapsed time for project (hours):           
  Total time in scheduled scans (hours):      
  Total time in recording scans (hours):      
  Total baseline hours (recording scans):     
  Projected max correlator output rate (kB/s):
  Projected correlator output data size (MB): """[1:].split("\n")
    def add_column(header, scans):
        elapsed_time = max(scan.stopj for _, scan in scans) - \
            min(scan.startj for _, scan in scans)
        in_scan_time = sum(scan_time[scan_index] for scan_index, _ in scans)
        recorded_time = sum(
            scan_time[scan_index]
            for scan_index, scan in scans
            if (not scan.norec) and (number_of_stations[scan_index] > 0))
        baseline_time = sum(
            scan_time[scan_index] * number_of_baselines[scan_index]
            for scan_index, scan in scans
            if (not scan.norec) and (number_of_stations[scan_index] > 0))
        max_data_rate = max(
            data_rate[scan_index]
            for scan_index, scan in scans
            if (not scan.norec) and (number_of_stations[scan_index] > 0))
        data_size = sum(
            data_rate[scan_index] * scan_time[scan_index] /
            scan_setup_file[scan_index].fspeed
            for scan_index, scan in scans
            if (not scan.norec) and (number_of_stations[scan_index] > 0) and
            (scan_setup_file[scan_index].fspeed > 0))
        text_lines[0] += header.rjust(10)
        text_lines[1] += f"{elapsed_time * 24    :10.2f}"
        text_lines[2] += f"{in_scan_time / 3600  :10.2f}"
        text_lines[3] += f"{recorded_time / 3600 :10.2f}"
        text_lines[4] += f"{baseline_time / 3600 :10.2f}"
        text_lines[5] += f"{max_data_rate / 1000 :10.1f}"
        text_lines[6] += f"{data_size / 1e6      :10.1f}"

    s.schn5.datasize = sum(
        data_rate[scan_index] * scan_time[scan_index] /
        scan_setup_file[scan_index].fspeed
        for scan_index, scan in core_scans
        if (not scan.norec) and (number_of_stations[scan_index] > 0) and
        (scan_setup_file[scan_index].fspeed > 0))
    s.schn5.maxdr = max(
        data_rate[scan_index]
        for scan_index, scan in core_scans
        if (not scan.norec) and (number_of_stations[scan_index] > 0))
    if s.schn1.fuzzy:
        add_column("Core ", core_scans)
    add_column("All scans", experiment_scans)
    if s.schn1.doscans[0] > 0:
        add_column("DOSCANS", do_scans)

    sum_file.write("\n".join(text_lines))
    
    sum_file.write("""
 NOTES:  Above numbers assume the same correlator parameters are used for all data.
         The 'baseline hours' include the autocorrelations (for data size calculations).
         The correlator output data rate is for real-time processing.
         Above numbers assume the same correlator parameters are used for all data.
         The recordings for a scan are assumed to start at the scan START minus PRESTART.
         Some stations (VLA, VLBA/MARK5C) wait for good data so the estimates may be high.""")
    if s.schn1.fuzzy:
        sum_file.write("""
         'Core' are scans with PREEMPT not 'EXTRA'.""")
    if s.schn1.doscans[0] > 0:
        sum_file.write("""
         Only scans in the DOSCANS range will be written to the Vex and other files.""")
    if any((scan.icent != 0) and (not scan.norec) and
           (number_of_stations[scan_index] > 0)
           for scan_index, scan in experiment_scans):
        sum_file.write("""
         Multiple phase center processing requested.
     Correlator output numbers are for first file only.""")
    sum_file.write("\n")
        
    data_size = sum(
        data_rate[scan_index] * scan_time[scan_index] /
        scan_setup_file[scan_index].fspeed
        for scan_index, scan in core_scans
        if (not scan.norec) and (number_of_stations[scan_index] > 0) and
        (scan_setup_file[scan_index].fspeed > 0))
    if data_size > 16e9:
        sum_file.write("  NOTE:  Output data set size large.  \n")

    if s.schcon.debug:
        s.wlog(0, "CORSOC starting.")
    if f2str(s.schsco.correl).startswith("FXCORR"):
        # checks for the old Socorro hardware correlator
        most_channels = 0
        max_filter_rate = 0
        for scan_index, scan in experiment_scans:
            ns = number_of_stations[scan_index]
            if (ns > 0) and not scan.norec:
                filter_rate = 0.131 / s.schco.corchan * ns * (ns + 1) / 2 * \
                    2048 * s.schco.corchan / max(256, s.schco.corchan) * \
                    scan_setup_file[scan_index].fspeed
                max_filter_rate = max(max_filter_rate, filter_rate)
                most_channels = max(most_channels,
                                    scan_setup_file[scan_index].mschn)

        warn = False
        if (s.schn5.maxdr > 1000000) and (not s.schn5.twohead):
            sum_file.write("""
 **** WARNING ****
    Projected correlator output data rate exceeds VLBA correlator
    limit of 1000 kbytes per second.
"""[1:])
            s.wlog(1, "CORSOC:  **** WARNING ****")
            s.wlog(1, "    Correlator output data rate limit exceeded.  "
                   "See summary file.")
            warn = True
        elif (s.schn5.maxdr > 2e6) and s.schn5.twohead:
            sum_file.write("""
 **** WARNING **** Projected correlator output data rate exceeds VLBA correlator
                   limit of 1000 kbytes per second per pass.
                   Assuming 2 pass processing for this wide band observation.
"""[1:])
            s.wlog(1, "CORSOC: **** WARNING ****")
            s.wlog(1, "    Correlator output data rate limit exceeded.  "
                   "See summary.")
            warn = True

        if max_filter_rate > 41188:
            sum_file.write(f"""
 ******** WARNING:  You have exceeded the FIR transfer rate limit of 41188 
                    channels per tic.  Your projected rate is {max_filter_rate}
"""[1:])
            if (s.schco.coravg != s.schco.corav2) and (s.schco.corav2 != 0):
                sum_file.write("""
                    This projection does not take into account your secondary averaging time.
"""[1:])
            if most_channels > 1:
                sum_file.write("""
                    This cannot be fixed by processing IF channels separately.
"""[1:])
            sum_file.write("""
                    Please contact someone in Socorro if you don't understand this.
"""[1:])
            warn = True

        if warn and s.schn5.twohead:
            sum_file.write("""
     If you are doing two pass processing (eg 512 Mbps),
     the output bit rate and FIR rate limits are twice the above values.
"""[1:])

    recording_scans = [scan for scan_index, scan in experiment_scans
                       if (number_of_stations[scan_index] > 0) and
                       not scan.norec]
    overlap = any(scan1.stopj > scan2.startj for scan1, scan2 in zip(
        recording_scans[:-1], recording_scans[1:]))
    if overlap:
        sum_file.write("""
 **** WARNING *****
      Overlapping scans were detected.  Either there was subarraying
      or some stations were scheduled separtely from others.
      If the latter, the output data rates and volume are underestimated
      because the number of baselines is underestimated.



""")


def write_stations(sum_file, stations, scans):
    sum_file.write("""


STATIONS USED IN SCHEDULE:

   Station  Code   Latitude Longitude  Elevation       X            Y            Z       Axis
                                                                                       Offset

""")
    ax_warn = False
    for station in stations:
        sum_file.write(
            f"   {station.station: <8}  {station.stcode: <3}"
            f"{station.lat/raddeg:10.5f}{station.long_bn/raddeg:11.5f}"
            f"{station.elev:8.0f}.  {station.xpos:13.3f}{station.ypos:13.3f}"
            f"{station.zpos:13.3f}{station.axoff:7.3f}\n")
        if (station.axoff == 0) and not (station.station.startswith("VLA")):
            ax_warn = True

    if ax_warn:
        s.wlog(1, "STALST:  WARNING - A station has zero axis offset, "
               "which is unlikely.")
        s.wlog(1, "         Accurate positions, including axis offsets are "
               "needed for correlation.")
        s.wlog(1, "         Is your station location information adequate?")
        s.wlog(1, "         See the summary file station list to see which "
               "stations are suspect.")

    mjd = int(scans[0].startj)
    sum_file.write(f"""

   Plate tectonic motion adjustments for MJD {mjd:6.0f}
   Station  Code      Station motions (m/yr)             Adjusted positions 
                     X       Y       Z     MJD0        X            Y            Z 
""")
    for station in stations:
        years = (mjd - station.mjdrate) / 365.25
        sum_file.write(
            f"   {station.station: <8}  {station.stcode: <3}"
            f"{station.dxpos:8.4f}{station.dypos:8.4f}{station.dzpos:8.4f}"
            f"{station.mjdrate: >7} "
            f"{station.xpos + station.dxpos * years:13.3f}"
            f"{station.ypos + station.dypos * years:13.3f}"
            f"{station.zpos + station.dzpos * years:13.3f}\n")

    if len(stations) > 1:
        stations_text = "   ".join(station.stcode.ljust(3)
                                    for station in stations[:30]).rstrip()
        sum_file.write(f"""


BASELINE LENGTHS (km)

          {stations_text}

""")
        def baseline_length(station1, station2):
            return round(math.sqrt(
                ((station1.xpos - station2.xpos) / 1000) ** 2 +
                ((station1.ypos - station2.ypos) / 1000) ** 2 +
                ((station1.zpos - station2.zpos) / 1000) ** 2))
        for row_station in stations:
            sum_file.write(f"   {row_station.stcode: <3}")
            sum_file.write("".join(f"{baseline_length(row_station, c):6d}"
                                    for c in stations))
            sum_file.write("\n")

    if not (s.schn1.vlaonly or s.schn1.notape):
        unique_minpause = list(OrderedDict(
            (scan.minpause, None) for scan in scans).keys())
        unique_prestart = list(OrderedDict(
            (scan.prestart, None) for scan in scans).keys())
        sum_file.write("""

RECORDING SYSTEM AND CALIBRATION INFORMATION:

  List of scan-dependent controls seen for recording timing.
""")
        sum_file.write("   MINPAUSE:")
        sum_file.write("".join(f"{mp / onesec:5.0f}."
                                for mp in unique_minpause[:5]))
        if len(unique_minpause) > 5:
            sum_file.write(" more")
        sum_file.write("\n")
    
        sum_file.write("   PRESTART:")
        sum_file.write("".join(f"{ps / onesec:5.0f}."
                                for ps in unique_prestart[:5]))
        if len(unique_prestart) > 5:
            sum_file.write(" more")
        sum_file.write("\n")

        disk_stations = [station for station in stations if station.usedisk]
        if len(disk_stations) > 0:
            sum_file.write("""
  DISKS - Stations potentially recording on disks.
   Station    Drive type   DAR     NBBC    Tsys
""")
            for station in disk_stations:
                sum_file.write(f"   {station.station: <8}     "
                               f"{station.disk: <6}     "
                               f"{station.dar: <5}{station.nbbc:6d}     "
                               f"{station.tscal: <4}\n")
                
        other_stations = [station for station in stations
                          if not station.usedisk]
        if len(other_stations) > 0:
            sum_file.write("""
  OTHER - Stations with other recording systems.
   Station    Drive type
""")
            for station in other_stations:
                sum_file.write(f"   {station.station: <8}     "
                               f"{station.recorder: <6}\n")

def write_summary(sum_file, scans, scan_offset, stations, setup_files):
    scheduled_scans = sum(
        1 if any(s.schn2a.stascn[scan_index, :]) else 0
        for scan_index in range(scan_offset, scan_offset + len(scans)))
    sum_file.write(f"""



EXPERIMENT SUMMARY:
  Number of input scans:     {s.schn1.nscans:6d}
  Number of scans scheduled: {scheduled_scans:6d}
  Number of stations:        {len(stations):6d}
  Number of sources input:   {s.schn1.nsrc:6d}
  Number of setup files used:{len(setup_files):6d}
""")

    do_reconfigure = any(station.dar == "VLBA" for station in stations)
    if not s.schn1.notape:
        sum_file.write("""

STATION SCAN SUMMARIES:""")

        header_line = f"""
  Station  Control   Scans   Scan   Record  Record   Gbytes{'     Formatter       Sync' if do_reconfigure else ''}
                            Hours   Hours   Scans{'              Reconfigures    Hours' if do_reconfigure else ''}
"""
        def write_station_line(station, scans, scan_hours, record_hours,
                               record_scans, gbytes):
            sum_file.write(
                f"  {station.station: <8}   "
                f"{station.control: <5} "
                f"{scans:6d}{scan_hours:8.2f}{record_hours:8.2f}"
                f"{record_scans:8d}  {gbytes:7.0f}.")
            if do_reconfigure:
                if not station.dar.startswith("VLBA"):
                    sum_file.write(
                        f"{station.nreconf[0]:13d}"
                        f"{station.ttsync * 24:12.2f}")
                else:
                    sum_file.write(
                        f"{station.nreconf[0]:9d}/{station.nreconf[1]:3d}"
                        f"{station.ttsync * 24:12.2f}")
            sum_file.write("\n")
        
        if s.schn1.fuzzy:
            sum_file.write("""

  Summary for core scans (PREEMPT not set to 'EXTRA'): """)

        sum_file.write(header_line)
        for station in stations:
            write_station_line(station, station.nstsc, station.scnhr,
                               station.tphr, station.tpscn, station.tgbytes)

        if s.schn1.fuzzy:
            sum_file.write("""
  Summary for all scans (including PREEMPT = 'EXTRA'):""")

            sum_file.write(header_line)
            for station in stations:
                write_station_line(station, station.enstsc, station.escnhr,
                                   station.etphr, station.etpscn,
                                   station.egbytes)

        if s.schn1.doscans[0] > 0:
            sum_file.write("""
  Summary for all scans selected by DOSCANS:""")

            sum_file.write(header_line)
            for station in stations:
                write_station_line(station, station.dnstsc, station.dscnhr,
                                   station.dtphr, station.dtpscn,
                                   station.dgbytes)
    else:
        # no recording
        sum_file.write("""
STATION SCAN SUMMARIES: 
  Station  Control   Scans 
""")
        for station in stations:
            sum_file.write(f"  {station.station: <8}   "
                           f"{station.control: <5} "
                           f"{station.nstsc:6d}\n")

    sum_file.write("""
  Notes on the station scan summaries: 

    "Record Scans" are periods of recording with no gap.  The Mark5A disk systems 
    have a limit of 1024 such scans.  There are often multiple projects on a disk pack.
    If using MARK5A, try to keep above about 6 GB per record scan by using MINPAUSE 
    and PRESTART to prevent short gaps.  However, also try to prevent record scans 
    of more than an hour to minimize risk to data from playback problems.
    MARK5C will have one observe scan per record scan and there is no limit to the number.

""")

    if not s.schn1.notape and do_reconfigure:
        sum_file.write("""
    "Sync Hours" is on-source, in-scan time lost during correlation to resyncing
    recordings.  Resyncs follow tape stoppages and formatter reconfigures.

    For VLBA DAR stations, total reconfigures and reconfigures during recording are shown.
    Reconfigures during recording can cause slow correlator sync.
    Any reconfigure can slow sync at JIVE.
"""[1:])

    early_tape_starts = sum(sum(
        station.tpstart[scan_offset: scan_offset + len(scans)] != 0)
                            for station in stations)
    if early_tape_starts > 0:
        sum_file.write(f"""
    Recording started before scan start time {early_tape_starts:5d} times.  See PRESTART and MINPAUSE.
    They may have been kept running through short scan gaps.
    The number can be large because each station/scan combination counts.
""")

    sum_file.write("\n\n\n")

def write_setups(sum_file, setups, stations, frequencies, frequency_sets,
                 scans, scan_offset):
    sum_file.write("""

 SETUP FILES:
""")
    
    if s.schcon.noset:
        sum_file.write("""
NOSETUP specified.  No setups used. 
        Cannot write telescope control files. 
""")
        return

    duplicates = OrderedDict()
    for setup_index, setup in enumerate(setups, 1):
        for previous_setup_index in duplicates.keys():
            station = stations[s.schsta.ischsta[setup.isetsta - 1] - 1]
            previous_setup = setups[previous_setup_index - 1]
            previous_station = stations[s.schsta.ischsta[
                previous_setup.isetsta - 1] - 1]
            if s.sameset(setup_index, previous_setup_index) and \
               ((not setup.recused and not previous_setup.recused) or
                (station.usedisk == previous_station.usedisk)):
                duplicates[previous_setup_index].append(setup_index)
                # write directly into Fortran memory, as this is the only write,
                # so writing through the catalog would be a lot of overhead
                s.setn1.listks[setup_index - 1] = previous_setup_index
                setup.listks = previous_setup_index
                break
        else:
            duplicates[setup_index] = []

    def setup_station_text(setup_index):
        return f"{setup_index:4d}:{setups[setup_index - 1].setsta[0]: <7}"
    
    for setup_index, duplicate_indices in duplicates.items():
        if len(duplicate_indices) > 0:
            sum_file.write(f"""

The following setup groups are the same as group{setup_index:4d} below.
""")
            line_elements = 6
            start_index = 0
            while start_index < len(duplicate_indices):
                sum_file.write("".join(
                    setup_station_text(i)
                    for i in duplicate_indices[
                            start_index:start_index + line_elements]) + "\n")
                start_index += line_elements

        write_setup(sum_file, setups, setup_index, frequencies, stations,
                    frequency_sets, scans, scan_offset)

def write_scans(sum_file, scans, scan_offset, stations, setup_files, sources,
                print_date):
    if s.schcon.debug:
        s.wlog(0, "SUMSCN: Starting.")
        
    last_sum_item_index = next(
        (index for index, item in reversed(list(enumerate(s.schsco.sumitem)))
         if f2str(item) != ""), None)

    if last_sum_item_index is None:
        return

    form_feed = chr(12)
    
    def format_time(mjd):
        dt = round_seconds(Time(mjd, format="mjd").datetime)
        day_of_year = dt.strftime("%-j")
        time = dt.strftime("%H:%M:%S")
        return f"{day_of_year: >3} {time}"

    # only fork the sun warning to standard output once per source
    forked_sources = set()
    
    # print two sum items per pass
    for item_pass in range(last_sum_item_index // 2 + 1):
        item1 = f2str(s.schsco.sumitem[item_pass * 2])
        item2 = f2str(s.schsco.sumitem[item_pass * 2 + 1])

        item_descriptor1, _, label_flag1 = s.sumdesc(item1)
        item_descriptor2, _, label_flag2 = s.sumdesc(item2)
        item_descriptor1 = f2str(item_descriptor1)
        item_descriptor2 = f2str(item_descriptor2)

        # print 20 stations on one line
        for station_pass in range((len(stations) - 1) // 20 + 1):
            station_start = station_pass * 20
            station_end = min((station_pass + 1) * 20, len(stations))

            line_counter = 0

            for scan_index, scan in enumerate(scans, scan_offset):
                if line_counter > s.schcon.linepg:
                    line_counter = 0

                if line_counter == 0:
                    sum_file.write(f"""

\f SCAN SUMMARY for experiment {f2str(s.schc1.expcode): <8}{f2str(s.schc1.expt)}
""")
                    line_counter = 2

                    if s.schn1.doscans[0] > 0:
                        sum_file.write(
                            "     DOSCANS specified.  'X' in col 1 means scan "
                            "will not be in VEX and other output files.\n")

                    if s.schn1.gotpreem:
                        sum_file.write(
                            "     Symbol after SCAN number (based on PREEMPT): "
                            "'+' EXTRA scan.  '-' Do not preempt for USNO.\n")
                        line_counter += 1
                        if s.schn1.doscans[0]  == 0:
                            sum_file.write(
                                "       DOSCANS not specified so the 'EXTRA' "
                                "scans will not be written to the VEX, crd, "
                                "and other files.\n")
                            line_counter += 1

                    if label_flag1 or label_flag2:
                        sum_file.write(
                            "     Flags: D=>Down, H=>Below Horizon, R=>Rises, "
                            "S=>Sets, W=>Slew too long, t=>Tape Chg.\n")
                        line_counter += 1

                    stations_text = "   ".join(
                        s.stcode.ljust(3)
                        for s in stations[station_start:station_end]).rstrip()
                    sum_file.write(f"""
     Top item is:    {item_descriptor1}
     Bottom item is: {item_descriptor2}
     TYPE top: -=> normal scan, P=>Pointing or Ta;  bottom: -=>recording, N=>not recording.
     {print_date}

SCAN  DAY START UT  SOURCE     TYPE  STATIONS    t => tape change
           STOP UT                    {stations_text}

"""[1:])
                    line_counter += 5

                    last_set_number = 0

                pre_symbol = {"EXTRA": "+",
                              "NO"   : "-"}.get(scan.preempt, " ")
                if scan.annot != "":
                    sum_file.write(f" ---------- {scan.annot} ----------\n \n")
                    line_counter += 2

                skipped = not any(station.stascn[scan_index]
                                  for station in stations)
                if skipped and (len(stations) > 1):
                    sum_file.write(
                        f"{scan_index + 1:4d}{pre_symbol}  Skipping scan on "
                        f"{scan.scnsrc}\n")
                    line_counter += 1
                elif skipped and (len(stations) == 1):
                    sum_file.write(
                        f"{scan_index + 1:4d}{pre_symbol}  Skipping scan on "
                        f"{scan.scnsrc}  "
                        f"at El={stations[0].el1[scan_index]:8.2f}\n")
                    line_counter += 1
                else:
                    line1 = (f"{scan_index + 1:4d}{pre_symbol} "
                             f"{format_time(scan.startj)} {scan.scnsrc: <12}")

                    if scan.setnum != last_set_number:
                        # print the setfile in 12 characters if it changed
                        print_set_file = setup_files[scan.setnum - 1].setfile
                        if len(print_set_file) >= 12:
                            # although 12 characters are printed,
                            # SCHED cuts the extension from 12 chars or more
                            base, ext = os.path.splitext(print_set_file)
                            if ext in {".set", ".SET"}:
                                print_set_file = base
                        print_set_file = print_set_file[-12:]
                        last_set_number = scan.setnum
                    else:
                        print_set_file = "-"
                    line2 = (f"{scan.scantag: <4}  {format_time(scan.stopj)} "
                             f"{print_set_file: <12}")
                    
                    if ((scan.point >= 0) or scan.pntvlba or scan.tanvlba or
                        (scan.dopeak > 0) or scan.dopn3db or
                        (scan.vlamode == "IR")):
                        line1 += "  P "
                    else:
                        line1 += "  - "

                    if scan.norec or not s.schn1.vlbitp:
                        line2 += "  N "
                    else:
                        line2 += "  - "

                    line1 += "".join(
                        f2str(s.sumdat(item1, scan_index + 1,
                                       station_index + 1)).ljust(6)
                        for station_index in range(station_start, station_end))
                    line2 += "".join(
                        f2str(s.sumdat(item2, scan_index + 1,
                                       station_index + 1)).ljust(6)
                        for station_index in range(station_start, station_end))

                    if ((s.schn1.doscans[0] == 0) or
                        (s.schn1.doscans[0] <= scan_index + 1
                          <= s.schn1.doscans[1])):
                        pre = ""
                    else:
                        pre = "X "

                    sum_file.write(f"{pre}{line1.rstrip()}\n")
                    sum_file.write(f"{pre}{line2.rstrip()}\n")
                    line_counter += 2

                    if not item1.startswith("TAPE"):
                        sun_warning_thresholds = [
                            s.sunwarn for s in stations[station_start:
                                                        station_end]
                            if s.stascn[scan_index]]
                        if len(sun_warning_thresholds) > 0:
                            line_counter += sun_warning(
                                sum_file, scan, sources,
                                max(sun_warning_thresholds), forked_sources)
                sum_file.write("\n")
                line_counter += 1
                    
def write_record_summary(sum_file, stations, scans, scan_offset):
    if s.schn1.vlbitp:
        sum_file.write("\f\n")
    if not s.schn1.vlbitp or s.schcon.noset:
        return

    if s.schcon.debug:
        s.wlog(0, "TPSUM starting")

    if s.schn1.fuzzy and s.schn1.doscans[0] == 0:
        sum_file.write("""
 TIME RANGE OF RECORDINGS and TOTAL BYTES:
    'extras' have PREEMPT=EXTRA, 'core' has other PREEMPT settings.

              Start extras   Core start time.   Core end time.    End extras       Core     Total
 Station      Day    Time      Day    Time       Day    Time      Day    Time     GBytes    GBytes 
                 (UT)             (UT)               (UT)             (UT)
""")
    elif s.schn1.fuzzy and s.schn1.doscans[0] != 0:
        sum_file.write("""
 TIME RANGE OF RECORDINGS and TOTAL BYTES:
    DOSCANS specified.  'core' have PREEMPT not set to EXTRA. 

             Start DOSCANS   Core start time.   Core end time.    End DOSCANS      Core    DOSCANS
 Station      Day    Time      Day    Time       Day    Time      Day    Time     GBytes    GBytes 
                 (UT)             (UT)               (UT)             (UT)
""")
    elif s.schn1.doscans[0] != 0:
        sum_file.write("""
 TIME RANGE OF RECORDINGS and TOTAL BYTES:
    DOSCANS specified.  Summary is for that range.

              DOSCANS start time.  DOSCANS end time.     DOSCANS
 Station          Day    Time         Day    Time        GBytes
                     (UT)                 (UT)
""")
    else:
        sum_file.write("""
 TIME RANGE OF RECORDINGS and TOTAL BYTES:

                Obs. start time.     Obs. end time.      Total 
 Station          Day    Time         Day    Time        GBytes
                     (UT)                 (UT)
""")

    def format_time(mjd):
        dt = round_seconds(Time(mjd, format="mjd").datetime)
        day_of_year = dt.strftime("%-j")
        time = dt.strftime("%H:%M:%S")
        return f"{day_of_year: >3}  {time}"

    recording_scans = [(scan_index, scan)
                       for scan_index, scan in enumerate(scans, scan_offset)
                       if not scan.norec]
    core_scans      = [(scan_index, scan)
                       for scan_index, scan in recording_scans
                       if scan.preempt != "EXTRA"]
    for station in stations:
        station_recording_scans = [(scan_index, scan)
                                   for scan_index, scan in recording_scans
                                   if station.stascn[scan_index]]
        station_core_scans      = [(scan_index, scan)
                                   for scan_index, scan in core_scans
                                   if station.stascn[scan_index]]
        if len(station_core_scans) > 0:
            core_start = station_core_scans[0][1].startj
            core_stop = station_core_scans[-1][1].stopj
            if s.schn1.fuzzy:
                gbytes = (station.egbytes if s.schn1.doscans[0] == 0 else 
                          station.dgbytes)
                extra_start = next(scan.startj
                                   for scan_index, scan in
                                   station_recording_scans
                                   if ((s.schn1.doscans[0] == 0) or
                                       (s.schn1.doscans[0] <= scan_index + 1)))
                extra_stop = next(scan.stopj
                                  for scan_index, scan in
                                  reversed(station_recording_scans)
                                  if ((s.schn1.doscans[0] == 0) or
                                      (scan_index + 1 <= s.schn1.doscans[1])))

                sum_file.write(
                    f"  {station.station: <11}{format_time(extra_start)}   "
                    f"{format_time(core_start)}   {format_time(core_stop)}   "
                    f"{format_time(extra_stop)}{station.tgbytes:8.0f}."
                    f"{gbytes:9.0f}.\n")
            else:
                gbytes = (station.tgbytes if s.schn1.doscans[0] == 0 else 
                          station.dgbytes)
                sum_file.write(
                    f"  {station.station: <15}{format_time(core_start)}       "
                    f"{format_time(core_stop)}     {gbytes:8.1f}\n")
        else:
            sum_file.write(f"  {station.station: <8}No recorded data.\n")

    sum_file.write("\n")

def write_separations(sum_file, sources):
    if len(sources) < 2:
        return

    coordinates = [(source_index, SkyCoord(source.rap, source.decp, unit="rad"))
                   for source_index, source in enumerate(sources)
                   if source.sused]
    separations = [
        (coordinate1.separation(coordinate2).degree, index1, index2)
        for (index1, coordinate1), (index2, coordinate2)
        in itertools.combinations(coordinates, 2)]

    # find the highest separation threshold from a pre-defined list
    # that has 50 or fewer source pairs
    if len(separations) <= 50:
        threshold = 30
    else:
        min_threshold = numpy.partition(separations, 50, 0)[50][0]
        threshold = next((level for level in (30, 20, 15, 10, 7.5, 5, 3)
                          if level <= min_threshold), 1)

    sum_file.write(f"""
 SOURCE SEPARATIONS

 Source separations in degrees for pairs closer than {threshold:6.2f} degrees.

"""[1:])
    for separation, index1, index2 in separations:
        source1 = sources[index1]
        source2 = sources[index2]
        if separation <= threshold:
            alias1 = source_alias(source1)
            alias2 = source_alias(source2)
            sum_file.write(f"     {alias1: <12}  {alias2: <12}  "
                           f"{separation:8.4f}\n")
    

    
def write_versions(sum_file):
    sum_file.write(f"""

CATALOGS, FILES, and CODE VERSIONS

Catalogs: 
  Station:   {f2str(s.schcst.stafile)}
                 Version:  {f2str(s.schcst.stver)}
  Location:  {f2str(s.schcst.locafile)}
                 Version:  {f2str(s.schcst.locaver)}
  Frequency: {f2str(s.schsco.freqfile)}
                 Version:  {f2str(s.schsco.freqver)}
  Source:   {f2str(s.schcsc.srcfile)}
                 Version:  {f2str(s.schcsc.srver)}
  Source2:  {f2str(s.schcsc.srcfile2)}
                 Version:  {f2str(s.schcsc.srver2)}


Other external files: 
  Ephemeris: {f2str(s.schsco.ephfile)}
  PEAKFILE:  {f2str(s.schsco.peakfile)}
  MSGFILE:   {f2str(s.schsco.msgfile)}
""")

    for sat_index in range(s.schsat.nsat):
        sum_file.write(f"  SATNAME:   {f2str(s.schcat.satname)}\n")
        sat_file = f2str(s.schcat.satfile[sat_index])
        if not sat_file.startswtih("NONE"):
            sum_file.write(f"     SATFILE:   {sat_file}\n")
        tle_file = f2str(s.schcat.tlefile[sat_index])
        if not tle_file.startswtih("NONE"):
            sum_file.write(f"     TLEFILE:   {tle_file}\n")
        sum_file.write(
            f"     KERFILE:   {f2str(s.schcat.kerfile[sat_index])}\n")
                    
    sum_file.write(f"""

Code versions: 
  pySCHED version:           {pysched_version}
  Based on SCHED version:    {s.vern.vernum:.2f}  {f2str(s.verc.version)}
  Version of VEX standard:   1.5/2.0
  Version of VEX code:       {vex_version}
  Version of ephemeris code: 1.01
""")

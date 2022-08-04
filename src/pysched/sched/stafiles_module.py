from . import parameter
from .parameter import secpday, onesec
from ..catalog import ScanCatalog, StationCatalog, FrequencySetCatalog, \
    SetupCatalog, SourceCatalog, FrequencyCatalog, SetupFileCatalog
from ..util import f2str
from .output_files import write_cover, format_float, write_freq_line, \
    round_seconds, sun_warning, write_setup, write_sources

import schedlib as s

from astropy.time import Time

from collections import OrderedDict

class LastState:
    def __init__(self, max_channels):
        self.first = True
        self.number_of_channels = 0
        self.freq_set_index = -1
        
        self.lo_sum =   [0 for _ in range(max_channels)]
        self.bbc_freq = [0 for _ in range(max_channels)]
        self.bbc_bw =   [0 for _ in range(max_channels)]

        self.gbytes = 0

        self.stop = 0
        
        self.line_index = 999
        self.page_index = 1

        self.day_of_year = -999

def write_schedule(sch_file, state, station, scan, scan_index, frequency_sets,
                   setups, used_setups, sources):
    if s.schcon.debug and ((scan_index        <= 2) or
                           (s.schn1.scanl - 3 <= scan_index)):
        s.wlog(0, f"STAFILES: Starting loop for scan {scan_index + 1:5d}")

    setup_index = station.nsetup[scan_index] - 1
    if setup_index not in used_setups:
        used_setups.append(setup_index)

    if s.schcon.debug and state.first:
        s.wlog(0, "PRTSCH: Making operator schedule file for "
               f"{station.station}")

    if state.first:
        sch_file.write(f"""
  COVER INFORMATION 

 Station:    {station.station: <8}  (Code {station.stcode: <3}) 
 Experiment: {f2str(s.schc1.expt)}
 Exp. Code:  {f2str(s.schc1.expcode): <8}

"""[1:])
        write_cover(sch_file, "", False)

    time_early = round(
        (scan.startj - station.tonsrc[scan_index]) * secpday)
    if ((station.up1[scan_index] == "") and
        (station.up2[scan_index] == "")):
        if time_early >= 0:
            dwell_time = round(
                (scan.stopj - scan.startj) * secpday)
        else:
            dwell_time = round(
                (scan.stopj - station.tonsrc[scan_index]) *
                secpday)
    else:
        dwell_time = 0

    freq_set_index = station.fseti[scan_index] - 1
    write_frequency = False
    write_bandwidth = False
    if freq_set_index != state.freq_set_index:
        lo_sum, bbc_freq, bbc_bw, crd_n, crd_f, crd_bw, \
            crd_s, crd_lo_sum, cr_set_c = s.fsfreqwr(freq_set_index + 1)
        setup_index = frequency_sets[freq_set_index].fsetks - 1
        number_of_channels = setups[setup_index].nchan
        if number_of_channels != state.number_of_channels:
            write_frequency = True
            write_bandwidth = True
        if (lo_sum[:number_of_channels] !=
            state.lo_sum[:number_of_channels]).any():
            write_frequency = True
        if (bbc_bw[:number_of_channels] !=
            state.bbc_bw[:number_of_channels]).any():
            write_bandwidth = True

        state.freq_set_index = freq_set_index
        state.number_of_channels = number_of_channels
        state.lo_sum   = lo_sum
        state.bbc_freq = bbc_freq
        state.bbc_bw   = bbc_bw

        frequency_lines = 1 + (number_of_channels - 1) // 6
    else:
        frequency_lines = 0

    start = Time(scan.startj, format="mjd").datetime
    stop  = Time(scan.stopj , format="mjd").datetime
    start_day_of_year = start.timetuple().tm_yday
    stop_day_of_year  = stop.timetuple().tm_yday
    
    next_line_index = state.line_index + 2
    if stop_day_of_year != state.day_of_year:
        next_line_index += 2
    if scan.annot != "":
        next_line_index += 2
    if write_frequency or write_bandwidth:
        next_line_index += 1
    if write_frequency:
        next_line_index += 2 * frequency_lines
    if write_bandwidth:
        next_line_index += frequency_lines

    if next_line_index > s.schcon.linepg:
        # next page
        state.line_index = 8
        state.page_index += 1

        first_line = (f"\fSchedule for {station.station: <8}  "
                      f"(Code {station.stcode: <3})")
        sch_file.write(f"""
{first_line: <69}Page {state.page_index:>3d}
               {f2str(s.schc1.expt)}
  UP:  D => Below limits;  H => Below horizon mask;  W => still slewing at end;  blank => Up.
  Early: Seconds between end of slew and start.   Dwell: On source seconds. 
  Disk: GBytes recorded to this point.
  TPStart:  Recording start time.  Frequencies are LO sum (band edge).
  SYNC: Time correlator is expected to sync up.
----------------------------------------------------------------------------------------
Start UT  Source               Start / Stop                 Early    Disk   TPStart
Stop UT                  LST      EL    AZ   HA  UP   ParA  Dwell   GBytes    SYNC
----------------------------------------------------------------------------------------
"""[1:])
        state.day_of_year = -999

    if stop_day_of_year != state.day_of_year:
        state.day_of_year = stop_day_of_year
        if start_day_of_year == stop_day_of_year:
            day_of_year = stop.strftime("%-j")
            day_of_month = stop.strftime("%-d")
            sch_file.write(stop.strftime("""
 --- %a  {: >2} %b %Y   Day {: >3} ---
""").format(day_of_month, day_of_year))
        else:
            start_doy = start.strftime("%-j")
            start_dom = start.strftime("%-d")
            stop_doy  = stop.strftime("%-j")
            stop_dom  = stop.strftime("%-d")

            sch_file.write(start.strftime("""
 --- Start: %a  {: >2} %b %Y   Day {: >3}""").format(start_dom, start_doy))
            sch_file.write(stop.strftime(
                " -- Stop: %a  {: >2} %b %Y   Day {: >3} ---\n").format(
                    stop_dom, stop_doy))
        state.line_index += 2

    if scan.annot != "":
        sch_file.write(f"""
 ---------- {scan.annot} ----------
""")
        state.line_index += 2

    if write_frequency or write_bandwidth:
        sch_file.write("\n")

    decimal_point_at = format_float(state.lo_sum[0]).index(".")
    if write_frequency:
        state.line_index += write_freq_line(
            sch_file, " Next scan frequencies:",
            state.lo_sum[:state.number_of_channels], decimal_point_at)
        state.line_index += write_freq_line(
            sch_file, " Next BBC frequencies: ",
            state.bbc_freq[:state.number_of_channels], decimal_point_at)
    if write_bandwidth:
        state.line_index += write_freq_line(
            sch_file, " Next scan bandwidths: ",
            state.bbc_bw[:state.number_of_channels], decimal_point_at)

    def format_time(mjd):
        dt = round_seconds(Time(mjd, format="mjd").datetime)
        return dt.strftime("%H %M %S")

    if (len(scan.scnsrc) > 8) and (sources[scan.srcnum - 1].sour8 != ""):
        short_name = "=" + sources[scan.srcnum - 1].sour8
    else:
        short_name = "---"
        
    if len(scan.scnsrc) <= 11:
        print_source = " " + scan.scnsrc
    else:
        print_source = scan.scnsrc
    if station.usedisk:
        gbytes_start = state.gbytes
        gbytes_stop  = round(station.gbytes[scan_index])
    else:
        gbytes_start = 0
        gbytes_stop  = 0
    state.gbytes = gbytes_stop
    if scan.norec:
        ctstart = "Stopped "
        ctcorr  = ""
    else:
        if station.useonsrc:
            dstart = max(scan.startj - station.tpstart[scan_index],
                         station.tonsrc[scan_index])
        else:
            dstart = scan.startj - station.tpstart[scan_index]

        if abs(state.stop - dstart) < onesec:
            ctstart = "No stop"
        else:
            ctstart = format_time(dstart)

        ctcorr = format_time(station.tcorr[scan_index])
        state.stop = scan.stopj
    
    sch_file.write("\n")
    sch_file.write(
        f"{format_time(scan.startj)} {print_source: <14}"
        f"{f2str(s.tformwrp(station.lst1[scan_index], 'T', 0, 2, 2, '  @'))}"
        f"{station.el1[scan_index]:6.1f}{station.az1[scan_index]:6.1f}"
        f"{station.ha1[scan_index]:5.1f}  "
        f"{station.up1[scan_index]: <1}{station.pa1[scan_index]:7.1f}"
        f"{time_early:6d} {gbytes_start:8d}   {ctstart: <8}\n")
    sch_file.write(
        f"{format_time(scan.stopj)}  {short_name: <13}"
        f"{f2str(s.tformwrp(station.lst2[scan_index], 'T', 0, 2, 2, '  @'))}"
        f"{station.el2[scan_index]:6.1f}{station.az2[scan_index]:6.1f}"
        f"{station.ha2[scan_index]:5.1f}  "
        f"{station.up2[scan_index]: <1}{station.pa2[scan_index]:7.1f}"
        f"{dwell_time:6d} {gbytes_stop:8d}   {ctcorr: <8}\n")

    state.line_index += 3

    state.line_index += sun_warning(sch_file, scan, sources, station.sunwarn)

def stafiles():
    if s.schcon.debug:
        s.wlog(0, "STAFILES: Station loop starting.")

    station_catalog = StationCatalog()
    station_catalog.read()
    station_catalog.read_scheduled_attributes()
    stations = station_catalog.used()

    scan_catalog = ScanCatalog()
    scan_catalog.read()
    scans = scan_catalog.used()
    scan_offset = scan_catalog.scan_offset

    frequency_set_catalog = FrequencySetCatalog()
    frequency_sets = frequency_set_catalog.read()
        
    setup_catalog = SetupCatalog()
    setups = setup_catalog.read()

    setup_file_catalog = SetupFileCatalog()
    setup_files = setup_file_catalog.read()

    source_catalog = SourceCatalog()
    sources = source_catalog.read()

    frequency_catalog = FrequencyCatalog()
    frequencies = frequency_catalog.read()

    for station_index, station in enumerate(stations):
        used_setups = []
        # Call the FORTRAN routine to open 3 files, the sch, crd and obs files.
        # Hack: we close/reopen the sch file ourself, so we can write to it
        # from python, while calling the FORTRAN routines writing to the crd
        # and obs files.
        opened_crd_file, opened_obs_file = s.fileopen(station_index + 1)
        s.closewrp(parameter.iprt)

        with open(f"{f2str(s.schc1.expcode)}sch.{station.stcode}".lower(),
                  "w") as sch_file:
        
            state = LastState(setup_catalog.max_channels)
            
            for scan_index, scan in enumerate(scans, scan_offset):
                if station.stascn[scan_index]:

                    write_schedule(sch_file, state, station, scan, scan_index,
                                   frequency_sets, setups, used_setups, sources)
                        

                    s.crdwrt(scan_index + 1, station_index + 1, state.first)
                    state.first = False

            s.crdwrt(-999, station_index + 1, False)
        
            if opened_crd_file:
                s.closewrp(parameter.iuvba)
            if opened_obs_file:
                s.closewrp(parameter.iuloc)
            if s.schcon.debug:
                s.wlog(0, "STAFILES: Files closed.")

            sch_file.write("""

SETUP FILE INFORMATION:
   NOTE: If DOPPLER, FREQ, or BW were used, see the individual scans for the final BBC settings.
""")
            for setup_index in used_setups:
                write_setup(sch_file, setups, setup_index + 1, frequencies,
                            stations, frequency_sets, scans, scan_offset)

            write_sources(sch_file, sources, setup_files, False)

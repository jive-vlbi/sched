from ..util import f2str

import schedlib as s

import numpy
import fortranformat

from datetime import timedelta
import itertools
import math

def round_seconds(x):
    if x.microsecond >= 500000:
        x += timedelta(seconds=1)
    return x.replace(microsecond=0)

def write_cover(output_file, lines_align, write_no_cover):
    cover_lines = [f"{lines_align}{f2str(line)}" if f2str(line) != ""
                   else f"{lines_align} "
                   for line in s.schsco.cover]
    for empty_line_index in (12, 11, 7, 3, 2):
        cover_lines.insert(empty_line_index, "")
    cover = "\n".join(cover_lines)
    output_file.write(f"\n{cover}\n\n")

    if s.schcon.coverlet:
        try:
            cover_scratch_file = open("schedcov.tmp", "r")
        except Exception as e:
            s.wlog(1, str(e))
            s.errlog("WRTCOV: Cannot open cover letter scratch file.  "
                     "This should not happen.")
        with cover_scratch_file:
            output_file.write("\nCOVER LETTER:\n\n")
            for line in cover_scratch_file.readlines():
                output_file.write(line.rstrip() + "\n")
    elif write_no_cover:
        output_file.write("\nNo cover letter provided.\n")
    
def format_float(item):
    text = f"{item:.8f}".rstrip("0")
    after_decimal = len(text) - text.index(".") - 1
    if after_decimal < 2:
        text += "0" * (2 - after_decimal)
    return text

def write_freq_line(output_file, header, items, decimal_point_at):
    output_file.write(header)
    characters = len(header)
    lines = 0
    for n, item in enumerate(items):
        text = format_float(item)
        decimal = text.index(".")
        text = (2 + (decimal_point_at - decimal)) * " " + text
        if ((characters + len(text) > 103) or ((n % 8 == 0) and (n > 0))):
            output_file.write("\n")
            lines += 1
            output_file.write(" " * len(header))
            characters = len(header)
        output_file.write(text)
        characters += len(text)
    output_file.write("\n")
    lines += 1
    return lines

def source_alias(source):
    return next(alias
                for csused, alias in zip(reversed(source.csused),
                                         reversed(source.source))
                if csused != "")

def sun_warning(output_file, scan, sources, threshold, forked_sources=None):
    """
    forked_sources: A set of source names, for which the warning already has
                    been printed. If the source name is in this set 
                    (or it is None), do not fork to standard output and .runlog.
                    Add the source name to the set if it is printed to 
                    standard output.
    """
    line_counter = 0
    source = sources[scan.srcnum - 1]
    sun_distance = source.sundis
    if sun_distance < threshold:
        text = (f"        Source only {sun_distance:6.1f} degrees from "
                "the Sun.\n")
        output_file.write(text)
        line_counter += 1
        
        alias = source_alias(source)
        if (forked_sources is not None) and (alias not in forked_sources):
            s.wlog(1, f"SUNWARN: {alias} only {sun_distance:.1f} degrees "
                   "from the Sun.")
            forked_sources.add(alias)

    if scan.ivlaphs not in {0, scan.srcnum}:
        source = sources[scan.ivlaphs - 1]
        sun_distance = source.sundis
        if sun_distance < threshold:
            text = (f"         VLA phasing source only {sun_distance:6.1f} "
                    "degrees from the Sun.")
            output_file.write(text + "\n")
            line_counter += 1

            alias = source_alias(source)
            if (forked_sources is not None) and (alias not in forked_sources):
                s.wlog(1, f"SUNWARN: VLA phasing source {alias} only "
                       f"{sun_distance:.1f} degrees from the Sun.")
                forked_sources.add(alias)

    return line_counter

def write_setup(output_file, setups, setup_index, frequencies, stations,
                frequency_sets, scans, scan_offset):
    if s.schcon.debug:
        s.wlog(0, f"PRTSET: Starting - setup, unit: {setup_index}")

    setup = setups[setup_index - 1]
    output_file.write(f"""

 ======== Setup file: {setup.setname}
""")

    freq_indices = [channel.ifreqnum for channel in setup.channel
                    if channel.ifreqnum >= 1]
    if len(freq_indices) > 0:
        output_file.write(
            f"   Matching groups in {f2str(s.schsco.freqfile)}:\n")
        unique_indices = sorted(
            numpy.unique(freq_indices, return_index=True)[1])
        for freq_index_index in unique_indices:
            freq = frequencies[freq_indices[freq_index_index] - 1]
            output_file.write((
                f"     {freq.frname: <13}   {freq.frnote}").rstrip() + "\n")

        if setup.bestover < setup.totbw:
            output_file.write(f"""
    ****** Only {setup.bestover:7.2f} of {setup.totbw:7.2f} MHz total bandwidth is within the IFs
           in the setup file before any FREQ or DOPPLER shifts.
"""[1:])

    else:    
        output_file.write("""
 --- WARNING ---  This group does not match an entry in the frequency catalog.
                  This might be ok because the catalog is not complete.
                  But be very careful to be sure that the setup is correct.
"""[1:])

    if not s.schn1.vlaonly:
        output_file.write(f"""
   Setup group: {setup_index:4d}         Station: {setup.setsta[0]: <8}          Total bit rate: {round(setup.totbps):5d}
   Format: {setup.format: <8}          Bits per sample: {setup.bits[0]:1d}         Sample rate:{setup.samprate:7.3f}
""")

        output_file.write(f"   Number of channels:{setup.nchan:3d}"
                          f"    DBE type: {setup.dbe: <8}")
        if not s.schn1.notape and setup.recused:
            output_file.write("         Speedup factor: "
                              f"{setup.speedup:6.2f}\n")
        else:
            output_file.write("\n")

        if not s.schn1.notape:
            station = stations[s.schsta.ischsta[setup.isetsta - 1] - 1]
            if (setup.format != "NONE") and station.usedisk:
                output_file.write("""
   Disk used to record data.
""")

            if not setup.recused:
                output_file.write("""
   Setup not used for recording data.
""")

    if any(channel.corinv != 0 for channel in setup.channel):
        output_file.write("""
   Frequencies shifted and sidebands inverted because RDBE_PFB can only
   do LSB.  Use a correlator, such as DiFX, that can invert sidebands.
""")

    if s.schcon.debug:
        s.wlog(0, "PRTFREQ starting. {setup.nchan:7d}{len(frequencies):7d}"
               f"{s.setn2b.pcalfr1.shape[0]:7d}")

    if setup.nchan > 0:
        line_elements = 8
        def write_attribute(header, attribute, format_):
            start_index = 0
            while start_index < setup.nchan:
                output_file.write(
                    header +
                    "".join(format_.format(getattr(channel, attribute))
                            for channel in setup.channel[
                                    start_index:start_index + line_elements]
                    ) +
                    "\n")
                header = " " * len(header)
                start_index += line_elements

        output_file.write("\n")
        write_attribute("   1st LO=", "firstlo", "{:10.2f}")
        write_attribute("   Net SB=", "netside", "{: >10}")
        write_attribute("   IF SB =", "side1",   "{: >10}")
        write_attribute("   Pol.  =", "pol",     "{: >9} ")
        write_attribute("   BBC   =", "bbc",     "{:10d}")
        write_attribute("   BBC SB=", "sidebd",  "{: >10}")
        write_attribute("   IF    =", "ifchan",  "        {: <2}")
    else:
        s.wlog(0, "PRTFREQ:  No channels!  Program problem.")

    if setup.setsta[0].startswith("VLBA"):
        output_file.write("\n   VLBA FE=     " +
                       "     ".join(f"{fe: <4}" for fe in setup.fe) +
                       "\n")
        if ((math.modf(setup.synth[0] * 10)[0] > 1e-6) or
            (math.modf(setup.synth[1] * 10)[0] > 1e-6)):
            output_file.write(f"   VLBA Synth={setup.synth[0]:12.5f}"
                              f"{setup.synth[1]:12.5f}{setup.synth[2]:9.1f}\n")
        else:
            output_file.write(f"   VLBA Synth={setup.synth[0]:6.1f}"
                              f"{setup.synth[1]:9.1f}{setup.synth[2]:9.1f}\n")

    if len(frequency_sets) > 0:
        output_file.write("""
  The following frequency sets based on these setups were used.
""")
        if setup.setsta[0].startswith("VLBA"):
            output_file.write("""
     See the crd files for VLBA legacy system setups and pcal detection details.
"""[1:])

        for freq_set_index, freq_set in enumerate(frequency_sets):
            freq_setup_index = freq_set.fsetks - 1
            freq_setup = setups[freq_setup_index]
            if ((freq_setup.listks == setup.listks) and
                (freq_set.fssame - 1 == freq_set_index)):
                lo_sum, bbc_freq, bbc_bw, crd_n, crd_f, crd_bw, \
                    crd_s, crd_lo_sum, cr_set_c = \
                        s.fsfreqwr(freq_set_index + 1)
                scan = scans[freq_set.fsetscn - scan_offset - 1]
                if (scan.freq[0] != 0) or (scan.bw[0] != 0):
                    text = f"  Frequency Set:{freq_set_index + 1:4d}  "\
                        "Based on FREQ, BW, and/or DOPPLER in schedule."
                    result = f2str(
                        s.chkifwrp(setup_index, lo_sum, bbc_bw))
                    if result != "OK":
                        s.wlog(1, f"""
PRTFREQ:  **** Warning - in frequency set {freq_set_index + 1} some requested frequencies are
          outside the available IFs.  See summary file for details."""[1:])
                else:
                    text = f"   Frequency Set:{freq_set_index + 1:4d}  "\
                        "Setup file default."
                    result = "OK"

                if scan.gotcrd:
                    text += "  Used with CRDFREQ or CRDDOP."

                output_file.write(f"\n{text}  "
                                  f"Used with PCAL = {freq_set.fspcal}\n")

                if result != "OK":
                    output_file.write(f"\n   **** Warning {result}\n")

                if setup.nchan > 0:
                    decimal_point_at = format_float(lo_sum[0]).index(".")
                    write_freq_line(
                        output_file, "   LO sum= ", lo_sum[:setup.nchan],
                        decimal_point_at)
                    write_freq_line(
                        output_file, "   BBC fr= ", bbc_freq[:setup.nchan],
                        decimal_point_at)
                    write_freq_line(
                        output_file, "   Bandwd= ", bbc_bw[:setup.nchan],
                        decimal_point_at)
                else:
                    s.wlog(0, "PRTFREQ:  Still no channels")

                if ((station.control == "VLBA") and
                    (setup.dbe.startswith("RDBE"))):
                    output_file.write(
                        f"   VLBA legacy crd files using {crd_n:2d} "
                        "channels based on RDBE channels: " +
                        "".join(f"{c:3d}" for c in cr_set_c[:crd_n]) +
                        "\n")
                    output_file.write("   CRD fr= " +
                                      "".join(f"{f:10.2f}"
                                              for f in crd_f[:crd_n]) +
                                      "\n")
                    output_file.write("   CRD bw= " +
                                      "".join(f"{bw:10.2f}"
                                              for bw in crd_bw[:crd_n]) +
                                      "\n")

                header = "    Matching frequency sets:"
                output_file.write(header)
                matching_setups = [
                    i for i, s in enumerate(frequency_sets)
                    if s.fssame == freq_set_index + 1]
                max_print = (129 - len(header) // 4)
                output_file.write("".join(
                    f"{i + 1:4d}" for i in matching_setups[:max_print])
                                  + "\n")

    else:
        text = "PRTFREQ:  No frequency sets!  Program problem."
        s.wlog(1, text)
        output_file.write(text + "\n")

    if not s.schn1.notape and setup.recused:
        output_file.write("\n   Track assignments are: \n")
        for i in range(setup.tapemode):
            output_file.write(f"    track{i + 1}= " +
                              ",".join(f"{t:3d}"
                                       for t in setup.track[i, :setup.nchan]) +
                              "\n")
        output_file.write(f"    barrel={setup.barrel} \n")

    if s.schcon.debug:
        s.wlog(0, "PRTSET: Done with setup.")
    
def write_sources(output_file, sources, setup_files, first_call):
    if s.schcon.debug:
        s.wlog(1, "SRCLST starting.")

    position_source = {
        'i': "From catalog imbedded in main SCHED input file.",
        "1": f2str(s.schcsc.srcfile),
        "3": f2str(s.schcsc.srcfile2),
        "P": "Planet.  Position is for scan 1, center Earth.",
        "S": "Satellite. Position is for scan 1, center Earth.",
        "2": f2str(s.schpeakc.psrcfile)}
    velocity_definition = {
        "R": "radio definition",
        "O": "optical definition",
        "Z": "redshift"}
    velocity_reference = {
        "L": "LSR",
        "H": "heliocentric",
        "G": "geocentric"}
    def source_write(headline, first_call, dummy_warn, sources):
        if s.schcon.debug:
            s.wlog(1, "SRCWRT starting.")

        if first_call:
            output_file.write(f"""\f{headline}
     Catalog positions marked with *. 
     Precession of date coordinates is based on stop time of first scan.
     Names used in schedule marked with *. 
     Observation date used in B1950/J2000 coordinate conversion (PRECDATE):{s.schcon.precdate:10.3f}
     No adjustments are made for rates (DRA, DDEC).
""")
        else:
            output_file.write(f"\n{headline}\n")

        if dummy_warn:
            output_file.write("""
     An unused dummy source of a scan that becomes a geodetic segment will show up here.
""")

        output_file.write("""
   Source                         Source position (RA/Dec)                        Error
                        (B1950)             (J2000)             (Date)            (mas)

""")
        for source_index, source in sources:
            column1 = [f" {csused:1} {alias: <12}"
                       for csused, alias in zip(source.csused, source.source)
                       if alias != ""]
            
            ra1950 = f2str(s.tformwrp(source.ra1950, "T", 0, 2, 9, "  "))
            ra2000 = f2str(s.tformwrp(source.ra2000, "T", 0, 2, 9, "  "))
            rap    = f2str(s.tformwrp(source.rap,    "T", 0, 2, 9, "  "))

            dec1950 = f2str(s.tformwrp(source.d1950, " ", 1, 2, 8, "  "))
            dec2000 = f2str(s.tformwrp(source.d2000, " ", 1, 2, 8, "  "))
            decp    = f2str(s.tformwrp(source.decp,  " ", 1, 2, 8, "  "))
            
            column2 = [
                f"{source.c1950:1} {ra1950: <16}  "
                f"{source.c2000:1} {ra2000: <16}  "
                f"{source.cdate:1} {rap: <16}  {source.raerr:8.2f}",
                f"{source.c1950:1}{dec1950: <16}   "
                f"{source.c2000:1}{dec2000: <16}   "
                f"{source.cdate:1}{decp: <16}   {source.decerr:8.2f}"]

            try:
                column2.append(position_source[source.whichcat])
            except KeyError:
                s.errlog("SRCLST: Programming problem.  WHICHCAT not set.")

            if source.remark != "":
                column2.append(source.remark)

            if source.didndop > 0:
                try:
                    text = ("Doppler based on "
                            f"{velocity_reference[source.velref]}")
                except KeyError:
                    s.errlog("SRCLST: Bad VELREF - program problem.")
                    
                try:
                    column2.append(
                        f"{text} frame and "
                        f"{velocity_definition[source.veldef]}.  Velocities:")
                except KeyError:
                    s.errlog("SRCLST: Bad VELDEF - program problem.")

                column2.append(
                    "".join(f"{vlsr:9.2f}"
                            for vlsr in source.vlsr[:min(source.didndop, 8)]))
                if source.didndop > 8:
                    column2.append(
                        "".join(
                            f"{vlsr:9.2f}"
                            for vlsr in source.vlsr[8:min(source.didndop, 16)]))
            elif source.dopped:
                column2.append("Doppler based on other sources.")

            proper_motion_used = ((source.pmra, source.pmdec, source.paralax) !=
                                  (0,           0,            0))
            if proper_motion_used:
                ra  = f2str(s.tformwrp(source.racat,  "T", 0, 2, 9, "  "))
                dec = f2str(s.tformwrp(source.deccat, " ", 1, 2, 8, "  "))
                
                column2.extend(
                    ["Proper motion used.  Reference epoch: "
                     f"{source.epocht:10.5f}",
                     
                     f"   At epoch: RA = {ra}    Dec = {dec}",
                     
                     f"   Rates: RA = {source.pmra * 1000:8.2f} mas/yr   "
                     f"Dec = {source.pmdec * 1000:8.2f} mas/yr",
                     
                     f"   Paralax: {source.paralax * 1000:9.4f} mas."])

            if (source.dra, source.ddec) != (0, 0):
                if proper_motion_used:
                    text = "Planetary motion (includes proper motion)."
                else:
                    text = "Planetary motion used."

                formatter = fortranformat.FortranRecordWriter("(E12.5)")
                column2.extend([
                    f"{text}  Ref. MJD: {source.pmtime:12.4f}",
                    f"   Rates: RA = {formatter.write([source.dra])} s/day   "
                    f"Dec = {formatter.write([source.ddec])} arcsec/day"])

            if source.whichcat == "P":
                column2.append(f"EPHFILE: {f2str(s.schsco.ephfile)}")
            elif source.whichcat == "S":
                column2.append(
                    f"KERFILE: {f2str(s.schcat.kerfile[source.satn - 1])}")
                sat_file = f2str(s.schcat.satfile[source.satn - 1])
                if not sat_file.startswtih("NONE"):
                    column2.append(f"SATFILE: {sat_file}")
                tle_file = f2str(s.schcat.tlefile[source.satn - 1])
                if not tle_file.startswtih("NONE"):
                    column2.append(f"TLEFILE: {tle_file}")

            for pair_index in range(s.schsou.npair):
                if (s.schsou.pairsrc[pair_index] - 1) == source_index:
                    center = f2str(s.schcsc.ctrname[s.schsou.paircent[
                        pair_index] - 1])
                    column2.append(
                        f" Pointing center for phase center group: {center}")
                    
            for pair_index in range(s.schsou.npair):
                pair_center = s.schsou.paircent[pair_index] - 1
                number_centers = s.schsou.ncsrc[pair_center]
                for center_source_index in s.schsou.ctrsrci[:number_centers,
                                                            pair_center]:
                    if (center_source_index - 1) == source_index:
                        center = f2str(s.schcsc.ctrname[pair_center])
                        column2.append(" Member of phase center group: "
                                       f"{center}")
                    
            for index, (c1, c2) in enumerate(itertools.zip_longest(
                    column1, column2, fillvalue="")):
                if index < 2:
                    output_file.write(f"{c1: <19}{c2}\n")
                else:
                    output_file.write(f"{c1: <21}{c2}".rstrip() + "\n")

            output_file.write("\n")

        output_file.write("""

SOURCE SCAN SUMMARY FOR SOURCES LISTED ABOVE

     Scan hours are for recording scans only. 
     Baseline hours are only counted for scans above horizon at both ends.
""")
    
        if s.schn1.fuzzy:
            output_file.write("     'Core' scans are those for which PREEMPT "
                              "is not 'EXTRA'.\n")
        if s.schn1.doscans[0] != 0:
            output_file.write("""
     The 'DOSCANS' columns are for scans in the range selected by DOSCANS
     Those are the scans sent to the Vex and other files
"""[1:])

        if s.schn1.fuzzy:
            headline_ext2 = "Scan  Baseline    Scan  Baseline"
            if s.schn1.doscans[0] == 0:
                headline_ext1 = " Core scans  (Hours)  All scans"
            else:
                headline_ext1 = " Core scans  (Hours) DOSCANS"
        else:
            headline_ext2 = " Scan  Baseline"
            if s.schn1.doscans[0] == 0:
                headline_ext1 = "Observing hours"
            else:
                headline_ext1 = " DOSCANS hours"
        output_file.write(f"""
  Source       Setup file             Frequency sets                  {headline_ext1}
                                   (duplicates not shown)             {headline_ext2}
"""[1:])

        for source_index, source in sources:
            print_source = next(alias for csused, alias in
                                zip(reversed(source.csused),
                                    reversed(source.source))
                                if csused == "*")
            for setup_file_index, setup_file in enumerate(setup_files):
                t_scan, t_base, e_scan, e_base, kf_sets = s.sbhwrap(
                    source_index + 1, setup_file_index + 1)
                if e_scan > 0:
                    set_file = setup_file.setfile[-21:]
                    kf_sets = f2str(kf_sets)
                    if len(kf_sets) > 28:
                        kf_sets = kf_sets[:28] + " +"
                    if s.schn1.fuzzy:
                        output_file.write(
                            f"  {print_source[:11]: <11}{set_file: <21}"
                            f"{kf_sets: <32}"
                            f"{t_scan * 24:8.3f}{t_base * 24:10.3f}"
                            f"{e_scan * 24:8.3f}{e_base * 24:10.3f}\n")
                    else:
                        output_file.write(
                            f"  {print_source[:11]: <11}{set_file: <21}"
                            f"{kf_sets: <32}"
                            f"{e_scan * 24:8.3f}{e_base * 24:10.3f}\n")
                    print_source = ""

    
    record_sources = [(i, s) for i, s in enumerate(sources)
                      if s.usedrec]
    used_sources   = [(i, s) for i, s in enumerate(sources)
                      if not s.usedrec and s.sused]
    phase_sources  = [(i, s) for i, s in enumerate(sources)
                      if not s.sused   and s.usedphs]

    if len(record_sources) > 0:
        source_write(" POSITIONS OF SOURCES USED IN RECORDING SCANS",
                     first_call, False, record_sources)
        first_call = False
    if len(used_sources) > 0:
        source_write(" POSITIONS OF ADDITIONAL SOURCES USED ONLY IN "
                     "NON-RECORDING SCANS",
                     first_call, True, used_sources)
        first_call = False
    if len(phase_sources) > 0:
        source_write(" POSITIONS OFADDITIONAL SOURCES USED ONLY FOR "
                     "PHASE CENTERS",
                     first_call, False, phase_sources)


    output_file.write("""

EFFECT OF SOLAR CORONA

  The solar corona can cause unstable phases for sources too close to the Sun.
  SCHED provides warnings at individual scans for distances less than 10 degrees.
  The distance from the Sun to each source in this schedule is:
    Source         Sun distance (deg) 
""")
    for source in sources:
        if source.sused:
            for csused, alias in zip(source.csused, source.source):
                if csused != "":
                    output_file.write(
                        f"   {alias: <12}    {source.sundis:8.1f}\n")

    output_file.write("""
  Barry Clark estimates from predictions by Ketan Desai of IPM scattering sizes 
  that the Sun will cause amplitude reductions on the  longest VLBA baselines 
  at a solar distance of 60deg F^(-0.6) where F is in GHz. 
  For common VLBI bands, this is: 
       327 MHz        117. deg 
       610 MHz         81. deg 
       1.6 GHz         45. deg 
       2.3 GHz         36. deg 
       5.0 GHz         23. deg 
       8.4 GHz         17. deg 
      15.0 GHz         12. deg 
      22.0 GHz          9. deg 
      43.0 GHz          6. deg 

""")

def write_solar_corona_warnings(output_file, scans, scan_offset, sources,
                                setup_files):
    text = """
SOLAR CORONA AFFECTED SCANS

 Source       Separation Threshold Frequency
"""[1:]
    source_freq_warnings = set()
    for scan_index, scan in enumerate(scans, scan_offset):
        setup_file = setup_files[scan.setnum - 1]
        if setup_file.mschn > 0:
            min_freq = min(setup_file.sffreq[:setup_file.mschn])

            threshold = 60 * (min_freq / 1000) ** -0.6
            source = sources[scan.srcnum - 1]
            if source.sundis < threshold:
                alias = source_alias(source)
                if (alias, min_freq) not in source_freq_warnings:
                    text += (f" {alias: <12} {source.sundis:<10.1f} "
                             f"{threshold:<9.1f} {min_freq:<.2f}\n")
                    source_freq_warnings.add((alias, min_freq))

    if len(source_freq_warnings) > 0:
        output_file.write(text + "\n")
        for line in text.split("\n"):
            if line != "":
                s.wlog(1, line)
        s.wlog(1, "See the summary file for more details about the effects "
               "of the solar corona.")

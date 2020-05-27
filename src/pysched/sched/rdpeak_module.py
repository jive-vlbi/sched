from ..catalog import PeakCatalog, SetupFileCatalog
from . import rfreq, schdefs
from .. import util, key

import schedlib as s

# two versions of the actual function, one that opens a file 
# and one that just uses the current keyin iterator

def rdpeak_open(stdin):
    if (s.schcon.debug):
        s.wlog(0, "RDPEAK starting")

    s.schpeakn.dopoint = s.schcon.autopeak or \
                         (s.schn2a.point[:s.schn1.nscans] >= 0).any()
    peakfile = bytes(s.schsco.peakfile).decode().strip()
    if (peakfile.upper() == "NONE") or not s.schpeakn.dopoint:
        return
    try:
        f = open(peakfile, "r")
    except Exception as e:
        s.wlog(1, str(e))
        s.wlog(1, "RDPEAK: Automatic insertion and/or conversion of scans "
               "was requested.")
        s.wlog(1, "        PEAKFILE needed, but could not be opened.")
        s.wlog(1, "        Problem is with file: ")
        s.wlog(1, peakfile)
        s.wlog(1, "        Note that file peak.cmd could be used")
        s.wlog(1, "        It is with the standard catalogs.")
        s.errlog(" Fix PEAKFILE or do not invoke AUTOPEAK or POINT.")
    with f:
        rdpeak_implementation(key.KeyfileIterator(f), stdin)

def rdpeak(input_iterator, stdin):
    if (s.schcon.debug):
        s.wlog(0, "RDPEAK starting")

    s.schpeakn.dopoint = True
    rdpeak_implementation(input_iterator, stdin)

first_call = True
def rdpeak_implementation(input_iterator, stdin):
    global first_call
    if first_call:
        s.schpeakn.pkgroup.fill(0)
        s.schpeakn.npkgrp = 0

    first_call = False

    input_name = "Program input" if input_iterator.input_ is stdin \
                 else input_iterator.input_.name
    
    state_defaults = {
        "srcfile":  [schdefs("refpointing"),          
                           util.noop],
        "setup":    ["",   util.expand_file_name],
        "setupl":   ["",   util.expand_file_name],
        "minfreq":  [60e3, util.noop],
        "minel":    [30.,  util.noop],
        "dwell":    [60.,  util.multiply_by(1 / 86400.)],
        "linename": ["",   util.upper],
        "vlamode":  ["",   util.upper],
    }
    record_defaults = {
        "stations": [[],   util.foreach(util.upper)],
        "sources":  [[],   util.foreach(util.upper)],
        "lineinit": [None, util.noop],
        "endpeak":  [None, util.noop],
    }
    input_iterator.set_defaults(record_defaults, state_defaults)
    
    # map from catalog entry attributes to keyin keywords
    attribute_to_key = {
        "pklines": "linename",
        "pvlamode": "vlamode",
        "pkdwell": "dwell",
        "pkminfq": "minfreq",
        "pkminel": "minel",
        "psetfile": "setup",
        "plsetfil": "setupl",
        "pksta": "stations",
        "pksrc": "sources",
    }

    catalog = PeakCatalog()
    start = int(s.schpeakn.npkgrp)
    index = start
    for record in input_iterator:
        values, present = util.merge_record_with_defaults(
            record, record_defaults, state_defaults)
        if "endpeak" in present:
            break
        if "lineinit" in present:
            rfreq(input_iterator)
            continue

        if index >= catalog.maxpeak:
            s.errlog("RDPEAK:  Too many reference pointing groups (Max {}) "
                     "in: {}".format(catalog.maxpeak, input_name))

        entry = catalog.entries[index]
        entry.set_keyin_values(values, attribute_to_key)

        if entry.plsetfil == "":
            entry.plsetfil = entry.psetfile
            s.wlog(1, "RDPEAK:  WARNING:  Your peak command file does not "
                   "have a separate setup file")
            s.wlog(1, "         for spectral line (narrow band) sources.  "
                   "You may encounter errors" )
            s.wlog(1, "         because the digital backends require "
                   "sample rate = 2 times bandwidth ")
            s.wlog(1, "         and the sample rate can only be changed "
                   "with a new setup file.")

        def check_pointing_setup(filename):
            return SetupFileCatalog.extend_with(
                filename, 
                "RDPEAK: Exceeded limit on number of setup files while adding "
                "ones needed for reference pointing.")
        
        entry.pklset = check_pointing_setup(entry.psetfile)
        entry.pklsetl = check_pointing_setup(entry.plsetfil)
        if (len(entry.pksta) == 0) and index > 0:
            entry.pksta = catalog.entries[index-1].pksta
        elif len(entry.pksta) == 0:
            s.errlog("RDPEAK:  First peak group has no stations.")

        if (len(entry.pksrc) == 0) and index > 0:
            entry.pksrc = catalog.entries[index-1].pksrc
        elif len(entry.pksrc) == 0:
            s.errlog("RDPEAK:  First peak group has no sources.")

        index += 1

    if index == 0:
        s.wlog(0, "RDPEAK:  No pointing instruction groups found in ")
        s.wlog(0, input_name)

    s.schpeakn.npkgrp = index

    s.schpeakc.psrcfile = util.resize_string(util.expand_file_name(
        state_defaults["srcfile"][0]), s.schpeakc.psrcfile.itemsize, "srcfile")

    catalog.write(range(start, index))


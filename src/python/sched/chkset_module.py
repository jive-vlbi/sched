from sched import chkdbbc
from catalog import SetupCatalog, StationCatalog, FrequencyCatalog
from util import f2str

import schedlib as s

import numpy as np

import math

def freq_range(setup_entry, ich):
    # rounds to multiples of 10 (copied from SCHED)
    bandwidth = math.floor(setup_entry.bbfilt[ich] / 10) * 10
    if setup_entry.netside[ich] == "L":
        return (setup_entry.freqref[ich] - bandwidth, setup_entry.freqref[ich])
    else:
        return (setup_entry.freqref[ich], setup_entry.freqref[ich] + bandwidth)
    

itout = 1
warn2cm = True
def chkset(ks):
    global itout, warn2cm
    setup_entry = SetupCatalog().read()[ks-1]
    station_catalog = StationCatalog()
    station_catalog.read()
    station_catalog.add_scheduled_attributes()
    station_entry = station_catalog.entries[setup_entry.isetsta-1]
    if s.schcon.debug:
        s.wlog(0, "CHKSET: Starting on {} in:".format(setup_entry.setsta[0]))
        s.wlog(0, "        {}".format(setup_entry.setname))

    errs = False
    sampwarn = True

    if s.schn1.notape and (setup_entry.format != "NONE"):
        obstype = f2str(s.schsco.obstyp)
        s.wlog(itout, "CHKSET: *** WARNING - OBSTYPE={} but setup has "
               "FORMAT={}".format(obstype, setup_entry.format))
        s.wlog(itout, "        No tapes will be recorded with this OBSTYPE.")
        if obstype == "NONE":
            s.wlog(itout, "        NONE is the default OBSTYPE. "
                   "Did you forget to specify it?")
        else:
            s.wlog(itout, "        Was this intended?")
        itout = 0
    
    if not s.schn1.vlaonly:
        if s.schn1.mark2 and (setup_entry.format != "MARKII"):
            s.wlog(1, "CHKSET: For OBSTYP=MKII, FORMAT must be MARKII, not: "
                   "{}".format(setup_entry.format))
            errs = True
        if (s.schcon.dovex or s.schcon.dovsop) and setup_entry.frswitch:
            s.wlog(1, "CHKSET:  Cannot frequency switch with VEX or VSOP file.")

        if setup_entry.nchan < 1:
            s.wlog(1, "CHKSET: Setup file must have at least 1 channel unless "
                   "OBSTYPE=VLA.")
            s.errlog("CHKSET: The error is in {}".format(setup_entry.setname))
            
        overwarn = False
        for ich in range(setup_entry.nchan):
            if setup_entry.sidebd[ich] not in ("U", "L"):
                s.wlog(1, "CHKSET: Sideband not U or L in {}".format(
                    setup_entry.setname))
                errs = True
            if setup_entry.pol[ich][:3] not in ("RCP", "LCP"):
                s.wlog(1, "CHKSET: In setup {} polarization of chan {} "
                       "not given or deduced (should be RCP or LCP).".format(
                           setup_entry.setname, ich+1))
                errs = True

            if ich > 0:
                for jch in range(ich + 1, setup_entry.nchan):
                    if (setup_entry.pol[ich] != setup_entry.pol[jch]) and \
                       (setup_entry.ifchan[ich] == setup_entry.ifchan[jch]):
                        s.wlog(1, "CHKSET:  In setup {} IF channel {} is "
                               "assigned to {} in chan {} and to {} in chan {} "
                               "Not possible.".format(setup_entry.setname, 
                                                      setup_entry.ifchan[ich], 
                                                      setup_entry.pol[ich], 
                                                      ich+1, 
                                                      setup_entry.pol[jch], 
                                                      jch+1))
                        errs = True
            
            if s.schn1.vlbitp:
                if setup_entry.bbfilt[ich] > 0.5001 * setup_entry.samprate:
                    s.wlog(1, "CHKSET: Bandwidth more than half of sample rate")
                    errs = True
                if (setup_entry.bbfilt[ich] < 0.4999 * setup_entry.samprate) \
                   and sampwarn:
                    s.wlog(0, "CHKSET note: Oversampling specified.")
                    sampwarn = False

            if ich > 0:
                freq_i = freq_range(setup_entry, ich)
                for jch in range(ich-1):
                    if setup_entry.pol[ich] == setup_entry.pol[jch]:
                        freq_j = freq_range(setup_entry, jch)
                        if (freq_j[0] <= freq_i[0] < freq_j[1]) or \
                           (freq_j[0] < freq_i[1] <= freq_j[1]):
                            overwarn = True
        # end of channel loop
        if overwarn:
            s.wlog(1, "CHKSET: Setup file: {} Has overlapping channels.  "
                   "Intended?".format(setup_entry.setname))

        if station_entry.dar.startswith("RDBE"):
            errs = s.chkrdbe(ks, errs)
        if station_entry.dar.startswith("DBBC"):
            errs = chkdbbc(setup_entry, station_entry) or errs
        if station_entry.dar == "WIDAR":
            errs = s.chkwidar(ks, errs)
        if station_entry.dar in ("VLBA", "VLBAG", "VLBA4"):
            errs = s.chkvdar(ks, station_entry.nbbc, errs)
        if station_entry.dar == "VLBA4":
            errs = s.chkv4dar(ks, errs)
        if station_entry.dar == "VLBAG":
            errs = s.chkgdar(ks, errs)
        if station_entry.dar == "MKIV":
            errs = s.chk4dar(ks, station_entry.nbbc, errs)
        if station_entry.dar == "CDAS":
            errs = s.chkcdas(ks, errs)
        
        if s.schn1.vlbitp:
            if station_entry.usedisk:
                errs = s.chkdisk(ks, errs)

            if station_entry.recorder in ("VLBA", "MKIV", "VLBA4") and \
               (setup_entry.format != "NONE"):
                errs = s.chkspd(ks, errs)

            if len(np.unique(setup_entry.bits)) > 1:
                s.wlog(1, "CHKSET: All channels must use the same number of "
                       "bits.")
                errs = True

    # end of if not vlaonly
    
    if warn2cm and (15100 < setup_entry.freqref[0] < 15500) and \
       setup_entry.setsta[0].startswith("VLBA") and (setup_entry.totbps < 1000):
        s.wlog(1, " ")
        s.wlog(1, "CHKSET:  See sched.runlog for information on 2cm "
               "frequencies.")
        s.wrtmsg(0, "CHKSET", "warn2cm")
        warn2cm = False

    if setup_entry.setsta[0].startswith("VLBA"):
        errs = s.chkvlba(ks, errs)
    if setup_entry.setsta[0].startswith("VLA"):
        errs = s.chkvla(ks, errs)

    if errs:
        s.wlog(1, "CHKSET:  Freq groups used or checked:")
        frequencies = FrequencyCatalog().read()
        for ich in range(setup_entry.nchan):
            if setup_entry.ifreqnum[ich] > 0:
                s.wlog(1, " Channel: {}  Frequency Group: {}".format(
                    ich+1, frequencies[setup_entry.ifreqnum-1].frname))
        
        s.errset(ks)

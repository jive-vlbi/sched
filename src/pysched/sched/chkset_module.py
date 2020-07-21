from . import chkdbbc, check_emerlin
from ..catalog import SetupCatalog, StationCatalog, FrequencyCatalog
from ..util import f2str

import schedlib as s

import numpy as np

import math

def freq_range(channel):
    # rounds to multiples of 10 (copied from SCHED)
    bandwidth = math.floor(channel.bbfilt / 10) * 10
    if channel.netside == "L":
        return (channel.freqref - bandwidth, channel.freqref)
    else:
        return (channel.freqref, channel.freqref + bandwidth)
    

itout = 1
warn2cm = True
def chkset(ks):
    global itout, warn2cm
    setup_entry = SetupCatalog().read()[ks-1]
    station_catalog = StationCatalog()
    station_catalog.read()
    station_catalog.read_non_scan_scheduled_attributes()
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

        if len(setup_entry.channel) < 1:
            s.wlog(1, "CHKSET: Setup file must have at least 1 channel unless "
                   "OBSTYPE=VLA.")
            s.errlog("CHKSET: The error is in {}".format(setup_entry.setname))
            
        overwarn = False
        for ich, channel in enumerate(setup_entry.channel):
            if channel.sidebd not in ("U", "L"):
                s.wlog(1, "CHKSET: Sideband not U or L in {}".format(
                    setup_entry.setname))
                errs = True
            if channel.pol[:3] not in ("RCP", "LCP", "X", "Y"):
                s.wlog(1, "CHKSET: In setup {} polarization of chan {} "
                       "not given or deduced (should be RCP, LCP, X or Y).".\
                       format(setup_entry.setname, ich+1))
                errs = True

            for jch, other in enumerate(setup_entry.channel[ich+1:], ich+1):
                if (channel.pol != other.pol) and \
                   (channel.ifchan == other.ifchan):
                    s.wlog(1, "CHKSET:  In setup {} IF channel {} is "
                           "assigned to {} in chan {} and to {} in chan {} "
                           "Not possible.".format(setup_entry.setname, 
                                                  channel.ifchan, 
                                                  channel.pol, 
                                                  ich+1, 
                                                  channel.pol, 
                                                  jch+1))
                    errs = True
            
            if s.schn1.vlbitp:
                if channel.bbfilt > 0.5001 * setup_entry.samprate:
                    s.wlog(1, "CHKSET: Bandwidth more than half of sample rate")
                    errs = True
                if (channel.bbfilt < 0.4999 * setup_entry.samprate) \
                   and sampwarn:
                    s.wlog(0, "CHKSET note: Oversampling specified.")
                    sampwarn = False

            if ich > 0:
                freq_i = freq_range(channel)
                for jch, other in enumerate(setup_entry.channel[:ich-1]):
                    if channel.pol == other.pol:
                        freq_j = freq_range(other)
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
            errs = chkdbbc(ks, setup_entry, station_entry) or errs
        if station_entry.dar == "eMERL":
            errs = check_emerlin(setup_entry, station_entry) or errs
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
    
    if warn2cm and (15100 < setup_entry.channel[0].freqref < 15500) and \
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
        for ich, channel in enumerate(setup_entry.channel):
            if channel.ifreqnum > 0:
                s.wlog(1, " Channel: {}  Frequency Group: {}".format(
                    ich+1, frequencies[channel.ifreqnum-1].frname))
        
        s.errset(ks)

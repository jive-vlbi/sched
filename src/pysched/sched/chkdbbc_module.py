from . import ifdbbc, ifdbbc3
from ..catalog import SetupCatalog

import schedlib as s

import numpy as np

def chkdbbc(ks, setup_entry, station_entry):
    if s.schcon.debug:
        s.wlog(0, "CHKDBBC: Starting")
    
    errs = False
    
    if station_entry.dar == "DBBC3":
        ifnam = "ABCDEFGH"
        ifbbc, mif = ifdbbc3()
    else:
        ifnam = "ABCD"
        e_firmware = SetupCatalog.is_dbbc_e_firmware(setup_entry)
        ifbbc, mif = ifdbbc(station_entry.dbbcver, e_firmware)

    def check_ifchan():
        somebad = False
        for ifchan in setup_entry.ifchan:
            if (ifchan[0] not in ifnam) or \
               ((len(ifchan) > 1) and (ifchan[1] not in "1234 ")):
                if_descriptors = [c + "[1-4]" for c in ifnam]
                ifnam_text = ", ".join(if_descriptors[:-1]) + \
                             " or " + if_descriptors[-1]
                s.wlog(1, "CHKDBBC: IFCHAN '{}' not {}".format(
                    ifchan, ifnam_text))
                somebad = True
        return somebad

    def check_channels():
        somebad = False
        for ich, channel in enumerate(setup_entry.channel):
            iif = ifnam.find(channel.ifchan[0])
            bbc = channel.bbc
            if (iif == -1) or (not ifbbc[bbc-1, iif]):
                s.wlog(1, "CHKDBBC: Illegal IF input {} for DBBC, channel "
                       "{} BBC {}".format(channel.ifchan, ich+1, bbc))
                s.wlog(1, "         Allowed IF index and first character "
                       "for this BBC are: {}".format(
                           "".join("({}, {})".format(i+1, ifnam[i])
                                   for i in np.where(ifbbc[bbc-1] == 1)[0])
                       ))
                somebad = True
        if somebad:
            s.wlog(1, "        Be careful of special wiring restrictions "
                   "for these DARs.")
        return somebad

    if s.schn1.vlbitp and (setup_entry.format != "NONE") and \
       (len(np.unique(setup_entry.ifchan)) > mif):
        s.wlog(1, "CHKDBBC: More than {} IF's requested for DBBC station {}.".\
               format(mif, station_entry.station))
        errs = True

    if station_entry.dar == "DBBC3":
        errs = check_ifchan() or errs
        errs = check_channels() or errs

    elif setup_entry.dbe == "DBBC_PFB":
        if not setup_entry.modetest:
            s.wlog(1, "CHKDBBC: You have specified DBE=DBBC_PFB The PFB "
                   "mode is not yet supported in the DBBC. This schedule "
                   "will probably fail. Specify MODETEST if you are "
                   "testing.")
            errs = True

        if setup_entry.nchan != 16:
            s.wlog(1, "CHKDBBC: For DBE=DBBC_PFB, NCHAN must be 16. Setup "
                   "specified: {}".format(setup_entry.nchan))
            errs = True

        if setup_entry.samprate != 64:
            s.wlog(1, "CHKDBBC: Invalid SAMPRATE specified: {} for "
                   "DBE=DBBC_PFB. Must be 64.0 Msamp/s.")
            errs = True

        bits = set(setup_entry.bits)
        if bits <= {1,2}:
            s.wlog(1, "CHKDBBC: BITS must be 1 or 2 for DBE=DBBC_PFB. "
                   "Value specified is: {}".format(
                       ", ".join(str(i) for i in sorted(bits))))
            errs = True

        if len(np.unique(setup_entry.sidebd)) != 1:
            s.wlog(1, "CHKDBBC: SIDEBAND must be the same for all channels "
                   "if DBE=DBBC_PFB.")
            errs = True

    elif setup_entry.dbe == "DBBC_DDC":
        allowed_sample_rates = SetupCatalog.dbbc_firmware_allowed_sample_rates(
            setup_entry)
        if setup_entry.samprate not in allowed_sample_rates:
            firmware_text = ", firmware {}".format(setup_entry.dbbcfw) \
                            if setup_entry.dbbcfw != "" else ""
            s.wlog(1, "CHKDBBC: Invalid SAMPRATE specified: {} for "
                   "DBE=DBBC_DDC{}. Must be {} to {} Msamp/s.".format(
                       setup_entry.samprate, firmware_text,
                       min(allowed_sample_rates), max(allowed_sample_rates)))
            errs = True

        bits = set(b for b in setup_entry.bits)
        if not (bits <= {1,2}):
            s.wlog(1, "CHKDBBC: BITS must be 1 or 2 for DBE=DBBC_DDC. "
                   "Value specified is: {}".format(
                       ", ".join(str(i) for i in sorted(bits))))
            errs = True

        errs = check_ifchan() or errs
        errs = check_channels() or errs

    if station_entry.dar != "DBBC3":
        errs = s.chkdbfq(ks, setup_entry.bbfilt, setup_entry.bbsyn, errs, True)

    return errs
                

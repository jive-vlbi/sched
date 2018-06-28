from sched import ifdbbc

import schedlib as s

import numpy as np

def chkdbbc(setup_entry, station_entry):
    if s.schcon.debug:
        s.wlog(0, "CHKDBBC: Starting")
    
    errs = False
    
    ifnam = "ABCD"
    ifbbc, mif = ifdbbc(station_entry.dbbcver, setup_entry.samprate)

    if s.schn1.vlbitp and (setup_entry.format != "NONE") and \
       (len(np.unique(setup_entry.ifchan)) > mif):
        s.wlog(1, "CHKDBBC: More than {} IF's requested for DBBC station {}.".\
               format(mif, station_entry.station))
        errs = True

        if setup_entry.dbe == "DBBC_PFB":
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
            # FIX should be e-series dependent
            if setup_entry.samprate not in (2, 4, 8, 16, 32, 64):
                s.wlog(1, "CHKDBBC: Invalid SAMPRATE specified: {} for "
                       "DBE=DBBC_DDC. Must be 2 to 64 Msamp/s.")
                errs = True

            bits = set(setup_entry.bits)
            if bits <= {1,2}:
                s.wlog(1, "CHKDBBC: BITS must be 1 or 2 for DBE=DBBC_DDC. "
                       "Value specified is: {}".format(
                           ", ".join(str(i) for i in sorted(bits))))
                errs = True

            for ifchan in setup_entry.ifchan:
                if (ifchan[0] not in ifnam) or (ifchan[1] not in "1234 "):
                    s.wlog(1, "CHKDBBC: IFCHAN '{}' not A[1-4], B[1-4], C[1-4] "
                           "or D[1-4]".format(ifchan))
                    errs = True

            somebad = False
            for channel in setup_entry.channel:
                iif = ifnam.find(channel.ifchan[0])
                bbc = channel.bbc
                if (iif != -1) and ifbbc[bbc-1, iif]:
                    s.wlog(1, "CHKDBBC: Illegal IF input {} for DBBC, channel "
                           "{} BBC {}".format(channel.ifchan, ich+1, bbc))
                    s.wlog(1, "         Allowed IF index and first character "
                           "for this BBC are: {}".format(
                               "".join("({}, {})".format(i+1, ifnam[i])
                                       for i in np.where(ifbbc[bbc-1] == 1)[0])
                           ))
                    somebad = True
                    errs = True
            if somebad:
                s.wlog(1, "        Be careful of special wiring restrictions "
                       "for these DARs.")

        errs = s.chkdbfq(ks, setup_entry.bbfilt, setup_entry.bbsyn, errs)
    return errs
                

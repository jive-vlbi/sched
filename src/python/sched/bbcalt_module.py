import schedlib as s

import numpy as np

def bbcalt(ks, setup_entry, ifbbc, ifnam, warning, caller):
    if s.schcon.debug:
        s.wlog(0, "BBCALT starting. Called by {}".format(caller))

    max_bbc = ifbbc.shape[0]
    max_if = ifbbc.shape[1]
    ubbc = np.full(max_bbc, False)
    for bbc in setup_entry.bbc:
        if bbc > 0:
            ubbc[bbc] = True
    ifinput = {}

    for ich in range(setup_entry.nchan):
        if (setup_entry.bbc[ich] == 0) and (ich > 1):
            for jch in range(ich):
                if setup_entry.freqref[ich] == setup_entry.freqref[jch]:
                    if setup_entry.ifchan[ich] == setup_entry.ifchan[jch]:
                        setup_entry.bbc[ich] = setup_entry.bbc[jch]
                        break
                    elif setup_entry.altifc[ich] == setup_entry.ifchan[jch]:
                        setup_entry.bbc[ich] = setup_entry.bbc[jch]
                        setup_entry.ifchan[ich] = setup_entry.altifc[ich]
                        break
            
        for ibbc in np.where(np.logical_not(ubbc))[0]:
            ifs = np.where(ifbbc[ibbc] == 1)[0]
            for iif in ifs:
                if setup_entry.bbc[ich] != 0:
                    break
                chknam = ifnam[iif]
                if (warning == "DBBC") and (iif in ifinput):
                    chknam = ifinput[iif]

                ifmatch = setup_entry.ifchan[ich].startswith(chknam)
                altifmatch = setup_entry.altifc[ich].startswith(chknam)
                if ifmatch or altifmatch:
                    if not ifmatch:
                        setup_entry.ifchan[ich] = setup_entry.altifc[ich]
                    setup_entry.bbc[ich] = ibbc + 1
                    ubbc[ibbc] = True
                    ifinput[iif] = setup_entry.ifchan[ich]
                    
        if setup_entry.bbc[ich] == 0:            
            s.wlog(1, " ")
            s.wlog(1, "BBCALT:  BBC setting problem: ")
            s.wlog(1, "         Setup file: {}".format(setup_entry.setname))
            s.wlog(1, "         Station: {}".format(setup_entry.setsta[0]))
            s.wlog(0, "         ICHAN={}  IFNAME={}  ALTIFC={}  NNBBC={}".\
                   format(ich+1, setup_entry.ifchan[ich], 
                          setup_entry.altifc[ich], max_bbc))
            s.wrtmsg(0, "BBCALT", "noassignbbc")
            if ich > 4:
                s.wlog(1, "         Maybe too many channels are requested for "
                       "the same IF channel.")
                
            if warning in ("GEO1", "GEO2"):
                s.wlog(0, "Note that 'geodetic' VLBA systems have restrictions "
                       "on IF assignments.")
                s.wlog(0, "        See IFCHAN in the manual.")
            if warning == "GEO2":
                s.wlog(0, "  They also cannot use 2 bits from more than "
                       "8 BBCs.")
            elif warning == "S2":
                s.wlog(0, "Note that the S2 system is not hooked to all BBCs.")
            
            s.errset(ks)

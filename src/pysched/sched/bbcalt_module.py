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
            ubbc[bbc - 1] = True
    ifinput = {}

    for ich, channel in enumerate(setup_entry.channel):
        if channel.bbc == 0:
            for other in setup_entry.channel[:ich]:
                if channel.freqref == other.freqref:
                    if channel.ifchan == other.ifchan:
                        channel.bbc = other.bbc
                        break
                    elif channel.altifc == other.ifchan:
                        channel.bbc = other.bbc
                        channel.ifchan = other.altifc
                        break
            
        for ibbc in np.where(np.logical_not(ubbc))[0]:
            ifs = np.where(ifbbc[ibbc] == 1)[0]
            for iif in ifs:
                if channel.bbc != 0:
                    break
                chknam = ifnam[iif]
                if (warning == "DBBC") and (iif in ifinput):
                    chknam = ifinput[iif]

                ifmatch = channel.ifchan.startswith(chknam)
                altifmatch = channel.altifc.startswith(chknam)
                if ifmatch or altifmatch:
                    if not ifmatch:
                        channel.ifchan = channel.altifc
                    channel.bbc = ibbc + 1
                    ubbc[ibbc] = True
                    ifinput[iif] = channel.ifchan
                    
        if channel.bbc == 0:
            s.wlog(1, " ")
            s.wlog(1, "BBCALT:  BBC setting problem: ")
            s.wlog(1, "         Setup file: {}".format(setup_entry.setname))
            s.wlog(1, "         Station: {}".format(setup_entry.setsta[0]))
            s.wlog(0, "         ICHAN={}  IFNAME={}  ALTIFC={}  NNBBC={}".\
                   format(ich+1, channel.ifchan, channel.altifc, max_bbc))
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

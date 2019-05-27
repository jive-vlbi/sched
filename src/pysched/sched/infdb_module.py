from . import toggle

import schedlib as s

def infdb(values, present, entries, index):
    entry = entries[index]

    if s.schcon.debug and (index < 2):
        s.wlog(0, "INFDB: Starting")

    if ((entry.crdch1 != 0) or (entry.crdsetch[0] != 0)) and \
       (entry.crdnch == 0):
        s.errlog("'INFDB:  If CRDCH1 or CRDSETCH is set, "
                 "CRDNCH must also be set.")
    if (entry.crdch1 != 0) and (entry.crdsetch[0] != 0):
        s.errlog("INFDB:  Do not use both CRDCH1 and CRDSETCH!")
    maxcrd = s.schn2a.crdsetch.shape[0]
    if entry.crdnch > maxcrd:
        s.errlog("INFDB:  CRDNCH set too high.  Maximum is {}.  It is {} "
                 "in input scan {}".format(maxcrd, entry.crdnch, index+1))
    if entry.crdch1 != 0:
        entry.crdsetch = [entry.crdch1+i for i in range(entry.crdnch)]

    default = False
    if index > 0:
        default = entries[index-1].crddop
    entry.crddop = toggle(values, present, "crddop", "crdnodop", default)
    entry.gotcrd = entry.crddop or any(freq > 0 for freq in entry.crdfreq)

    if ((entry.crdnch < 1) or (entry.crdnch > maxcrd)) and \
       entry.gotcrd:
        s.wlog(1, "INFDB: CRDNCH is required and must be between 1 and {}".\
               format(maxcrd))
        s.wlog(1, "       when CRDFREQ or CRDDOP is specified.")
        s.errlog("       CRDNCH is {} on scan {}".format(entry.crdnch, index+1))

    if entry.crddop and (entry.crdsetch[0] < 1):
        s.errlog("INFDB:  If invoking DOPCRD, please also set CRDCH1 or "
                 "CRDSETCH")

    if entry.dopsrc == "":
        entry.dopsrc = entry.scnsrc

    if entry.dopcal:
        s.wlog(1, "INFREQ: Use DOPPLER, not DOPCAL.")
    else:
        default = False
        if index > 0:
            default = entries[index-1].dopcal
        entry.dopcal = toggle(values, present, "doppler", "nodop", default)

    # dopincr's scan index dimension is the first, instead of the last,
    # so it doesn't fit into the catalog logic, write into the common block 
    # manually here
    s.schn2b.dopincr[index, :] = values["dopincr"]

    if (index != 0) and (entry.freq[0] != 0) and \
       (entry.setnum != entries[index-1].setnum) and \
       (entry.freq[0] == entries[index-1].freq[0]):
            s.wlog(1, "INFDB: Setup changed and specified frequency didn't. ")
            s.wlog(1, " Is this really what you wanted? ")
            s.wlog(1, " Use FREQ=0 to get the new setup default.")

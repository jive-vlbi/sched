from . import schin, getfreq, rdpeak_open, srread_open, getset, \
    parameter
from .. import util

import schedlib as s

def input_(stdin):
    if s.schcon.debug:
        s.wlog(0, "INPUT starting")
    schin(stdin)

    rdpeak_open(stdin)

    s.accsrc(True)
    s.schcsc.srver = " --- ".ljust(s.schcsc.srver.itemsize)
    s.schcsc.srver2 = " --- ".ljust(s.schcsc.srver2.itemsize)
    srread_open(util.f2str(s.schcsc.srcfile), stdin, True, '1')
    srcfile2 = util.f2str(s.schcsc.srcfile2)
    if (srcfile2 != "NONE"):
        srread_open(srcfile2, stdin, True, '3')
    if (s.schpeakn.dopoint):
        srread_open(util.f2str(s.schpeakc.psrcfile), stdin, True, '2')
    if (not s.schcon.noset):
        getset()
    if (not s.schcon.noset):
        getfreq()

    s.wlog(0, "INPUT:   Found {:5d} input scans.".format(int(s.schn1.nscans)))

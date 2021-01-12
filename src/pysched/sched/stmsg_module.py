from .. import util
from . import parameter
from ..update_catalogs import checkout_dir
from ..version import pysched_version

import schedlib as s

import os.path

def stmsg():
    s.schcon.readlog = False
    logfile = "sched.runlog"
    s.schsco.logfile = util.resize_string(logfile, s.schsco.logfile.itemsize,
                                          "logfile")
    open_stat = "OLD" if os.path.exists(logfile) else "NEW"
    error_msg, error_code = s.openwrap(parameter.ilog, logfile, "TEXT",
                                       open_stat)
    if error_code != 1:
        s.wlog(1, f"STMSG: CANNOT OPEN LOG FILE {logfile}")
        s.wlog(1, f"       Error text: {error_msg}")
        s.error(" Check why log file cannot be opened")

    s.wlog(1, " ")
    s.wlog(1, f"    Welcome to pySCHED version {pysched_version} "
           "based on ")
    s.wlog(1, f"    SCHED version: {s.vern.vernum:.6} "
           f"{util.f2str(s.verc.version)}")
    s.wlog(1, " ")
    s.wlog(1, "The manual is at http://www.aoc.nrao.edu/software/sched/"
           "index.html")
    s.wlog(1, "Bug reports, feature requests and other discussions can be "
           "posted on the GitHub page:")
    s.wlog(1, "    https://github.com/jive-vlbi/sched")
    s.wlog(1, f"pySCHED will use the catalogs under {checkout_dir} unless "
           "$SCHED is set.")
    s.wlog(1, f"Most run time messages will be in {logfile}")

    s.schsco.freqver = util.resize_string("None used",
                                          s.schsco.freqver.itemsize, "freqver")

    s.wlog(1, " ")
    s.wlog(1, f"Check 'sched.py -h' for command line paramters.")
    s.wlog(1, "Some useful commands to give now if running interactively:")
    s.wlog(1, "   SCHedule=<filename>    :  Specify input file.")
    s.wlog(1, "   PLOT                   :  Invokes uv, xy, rd, and uptime "
           "plotting.")
    s.wlog(1, "   FREQLIST=lowF,highF    :  Make frequency list (MHz).  "
           "Then exit.")
    s.wlog(1, "   EXIT                   :  Leave program.")
    s.wlog(1, "   /                      :  End of inputs - run program "
           "(or EXIT).")
    s.wlog(1, " ")
    
    if s.rdcatn.srcvel.shape[0] < s.schn2b.freq.shape[0]:
        s.errlog("STMSG: Programming error.  MVEL .LT. MAXCHN")

    

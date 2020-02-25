from . import toggle
from .. import util

import schedlib as s

import numpy as np

def invla(values, present, entries, index, case):
    warn = True
    if s.schcon.debug and ((index < 2) or (case != 1)):
        s.wlog(0, "INVLA: Starting with case: {}".format(case))

    if case == 1:
        entry = entries[index]
        
        default = False
        if index > 0:
            default = (entries[index-1].vlatsys == "T")
        entry.vlatsys = "T" if toggle(values, present, "vlantsys", "vlatsys", 
                                      default) \
                        else " "

        if entry.vlaphs == "":
            entry.vlaphs = entry.scnsrc

        if warn and (("vlabw" in present) or ("vlaband" in present)):
            s.wlog(1, "INVLA: -- WARNING -- VLABW or VLABAND  specified in "
                   "main schedule.")
            s.wlog(1, "INVLA:               They will be ignored.")
            s.wlog(1, "INVLA:               Specify them in the setup files.")
            warn = False

    elif case == 2:
        s.schc3.vlatype = util.resize_string(values["vlatype"], 
                                             s.schc3.vlatype.itemsize, "vlatype")
        s.schn3.vlausern = values["vlausern"]
        s.schn1.iatutc = values["iatutc"]

        if not s.schcon.noset:
            if [station[:3] for station in s.schc1.staname[:s.schn1.nsta]].\
               count(b"VLA") >= 2:
                s.wrtmsg(1, "INVLA", "multipleVLA")
    else:
        s.errlog("INVLA: Bad case.")

    s.schn3.vlarfant = values["vlarfant"]

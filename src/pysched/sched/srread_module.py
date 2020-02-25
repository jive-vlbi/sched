from ..catalog import SourceCatalog
from .. import util, key
from . import parameter

import schedlib as s

import numpy as np

import re
import math
import itertools

eqwarn = True
lastfile = ""
malias = s.schcsc.source.shape[0]
record_defaults = {}
state_defaults = {}

# wrapped version of function below (srread) that opens the filename
# before reading from the input
def srread_open(filename, stdin, select, thiscat):
    if select and s.schsou.msrc > 0:
        gotall = s.srcflg()
    else:
        gotall = False
    
    if ((filename.upper() == "NONE") or (gotall and not s.schcon.plot)):
        return

    try:
        f = open(filename, "r")
    except Exception as e:
        s.putout(str(e))
        s.error("Source catalog: {} not opened".format(filename))
    with f:
        return srread(key.KeyfileLister(f), stdin, select, thiscat)

def srread(input_iterator, stdin, select, thiscat):
    global eqwarn, lastfile, record_defaults, state_defaults

    if s.schcon.debug:
        s.wlog(0, "SRREAD starting")

    infile = "Program_input" if input_iterator.input_ is stdin else \
             input_iterator.input_.name

    if infile != lastfile:
        lastfile = infile
        state_defaults = {
            "version": ["Not given", util.noop],
            "epoch":   [0.,          util.noop],
            "equinox": ["",          util.upper],
            "vdef":    ["R",         util.upper],
            "vref":    ["L",         util.upper],
        }
        record_defaults = {
            "source":   [["NONAME"] + [""] * (malias - 1), 
                                    util.foreach(util.strip_upper)],
            "dec":      [-324000.,  util.multiply_by(parameter.radds)],
            "ra":       [0.,        util.multiply_by(parameter.radhs)],
            "raerr":    [0.,        util.noop],
            "decerr":   [0.,        util.noop],
            "remarks":  ["",        util.noop],
            "calcode":  [" ",       util.upper],
            "velocity": [[-1e9],    util.extend_to(s.schsou.vlsr.shape[0])],
            "dra":      [0.,        util.noop],
            "ddec":     [0.,        util.noop],
            "pmepoch":  [[0.,] * 4, util.noop],
            "pmra":     [0.,        util.multiply_by(1/1000.)],
            "pmdec":    [0.,        util.multiply_by(1/1000.)],
            "paralax":  [0.,        util.noop],
            "endcat":   [None,      util.noop],
            "parallax": [None,      util.noop],
            "flux":     [None,      util.noop], # value ignored
            "fluxref":  [None,      util.noop], # value ignored
        }

    input_iterator.set_defaults(record_defaults, state_defaults)
    
    attribute_to_key = {
        "racat": "ra",
        "deccat": "dec",
        "remark": "remarks",
        "vlsr": "velocity",
        "velref": "vref",
        "veldef": "vdef",
        "epocht": "epoch",
    }
    # update with identity mapping elements
    attribute_to_key.update(
        {key: key for key in itertools.chain(state_defaults.keys(),
                                             record_defaults.keys())
         if (key not in attribute_to_key.values()) and 
         (key not in {"version", "endcat", "parallax", "fluxref"})})
        

    s.wlog(0, "SRREAD:  Reading source catalog:  {}".format(infile[-(100-24):]))
    
    catalog = SourceCatalog()
    index = s.schsou.msrc
    
    for record in input_iterator:
        values, present = util.merge_record_with_defaults(
            record, record_defaults, state_defaults)
        
        if "endcat" in present:
            break

        if "source" not in present:
            s.wlog(1, " WARNING:  An unnamed source was found in the source "
                   "catalog:")
            s.wlog(1, "           {}".format(infile))
            s.wlog(1, "           There may be an extra '/' somewhere.")
            continue

        for source in values["source"]:
            if set(source) & set(';:=&*$ "'):
                s.wlog(1, "SRREAD: The source name {} contains an illegal "
                       "character for Vex.".format(source))
                s.wlog(1, "        The source is in catalog: {}".format(infile))
                s.wlog(1, '        The illegal characters are (tab) '
                       '(new line) ; : = & * $ " and (space).')
                s.errlog("Change the source name and rerun. ")
                
        srused = False
        keepit = False
        if select:
            # check if any of the source name aliases is in the schedule and
            # hasn't been found
            encoded_alias = [util.resize_string(
                source, s.schc1.srcname.itemsize, "source").encode()
                             for source in values["source"] if source != ""]
            if np.in1d(encoded_alias, 
                       np.where(s.schn1.srcatn[:s.schn1.nsrc] == 0, 
                                s.schc1.srcname[:s.schn1.nsrc], 0)).any():
                keepit = True
                srused = True
        else:
            keepit = True

        # backward compatibility for equinox/epoch mix-up
        valid_equinox = ("J2000", "B1950", "DATE")
        epoch = values["epoch"]
        epeq = (isinstance(epoch, str) and (epoch.upper() in valid_equinox))
        if values["equinox"] not in valid_equinox:
            if epeq:
                if eqwarn:
                    s.wlog(1, "SRREAD:  Please convert source catalog from use "
                           "of EPOCH to EQUINOX.")
                    eqwarn = False
                values["equinox"] = epoch.upper()
                values["epoch"] = 0.
            else:
                s.wlog(1, "SRREAD:  Source catalog contains invalid equinox: "
                       "{}".format(values["equinox"]))
                s.wlog(1, "         EPOCH also could not be interpreted as a "
                       "valid equinox.")
                s.errlog(" EQUINOX must be {} ".format(", ".join(valid_equinox)))
        else:
            if epeq:
                s.wrtmsg(0, "SRREAD", "eqep")
                s.errlog("SRREAD: See sched.runlog. Convert source lists to use "
                         "EQUINOX.")
            else:
                if isinstance(epoch, str):
                    s.errlog("SRREAD: Source catalog epoch is a string, but not "
                             "a valid equinox")
                if (epoch != 0.) and \
                   ((values["epoch"] < 1900.) or (values["epoch"] > 2100.)):
                    s.wlog(1, "SRREAD: *****   Very odd EPOCH: {} "
                           "for source {}".format(epoch, values["source"][0]))
                    s.wlog(1, "                Please read sched.runlog.")
                    s.wrtmsg(0, "SRREAD", "goofyepoch")

        if keepit:
            if index >= catalog.maxsource:
                s.errlog("'Too many sources for SCHED.  Max ={}".format(
                    catalog.maxsource))
            entry = catalog.entries[index]

            if ("ra" not in present) or ("dec" not in present):
                s.wlog(1, "SRREAD: WARNING: Default RA or Dec used for {}.  "
                       "Was that intended?".format(values["source"][0]))

            entry.set_keyin_values(values, attribute_to_key)

            entry.whichcat = thiscat
            (entry.c1950, entry.c2000, entry.cdata) = (" ", " ", " ")
            if values["equinox"] == "B1950":
                entry.ra1950 = entry.racat
                entry.d1950 = entry.deccat
                entry.c1950 = "*"
            elif values["equinox"] == "J2000":
                entry.ra2000 = entry.racat
                entry.d2000 = entry.deccat
                entry.c2000 = "*"
            elif values["equinox"] == "DATE":
                entry.rap = entry.racat
                entry.decp = entry.deccat
                entry.cdate = "*"
            else:
                s.errlog("SRREAD: Programming error with equinox")
                
            if not re.match("[A-Z0-9 ]", entry.calcode[0]):
                s.errlog("SRREAD: Illegal character {} in CALCODE for {}".format(
                    entry.calcode[0], entry.source[0]))

            if ("dra" in present) or ("ddec" in present):
                entry.pmtime, error = s.sla_cldj(*values["pmepoch"][:3])
                if error not in [0, 3]:
                    s.errlog(" SRREAD: Problem converting planetary motion "
                             "epoch to MJD")
                entry.pmtime += values["pmepoch"][3] * parameter.radhs / \
                                (math.pi * 2)

            # prefer correct spelling over paralax
            if "parallax" in present:
                entry.paralax = values["parallax"]
            entry.paralax /= 1000.
            if (set(("pmra", "pmdec", "paralax", "parallax")) & present) and \
               (entry.epocht < 1900. or entry.epocht > 2100.):
                s.wlog(1, "SRREAD: **** Proper motions specified for {}".format(
                    entry.source[0]))
                s.wlog(1, "             But a very unlikely EPOCH given: {}".\
                       format(entry.epocht))
                
            index += 1
        # end of if keepit
        s.srlist(values["source"][0], values["ra"], values["dec"], 
                 values["raerr"], values["decerr"], values["equinox"], 
                 values["calcode"], srused)
            
    # end of record loop
    catalog.write(range(index))
    s.schsou.msrc = index
    if thiscat == "1":
        s.schcsc.srver = util.resize_string(
            state_defaults["version"][0], s.schcsc.srver.itemsize, "version")
    elif thiscat == "3":
        s.schcsc.srver2 = util.resize_string(
            state_defaults["version"][0], s.schcsc.srver2.itemsize, "version")
        

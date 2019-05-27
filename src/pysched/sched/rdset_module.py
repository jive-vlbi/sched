from ..catalog import SetupCatalog
from .. import util, key
from . import parameter

import schedlib as s

import itertools

dbbc_firmware_warnings = set()
def rdset(setreq, input_iterator, isetf):
    s.setn1.sdebug = s.schcon.debug
    if s.setn1.sdebug:
        print("RDSET SDEBUG=", s.setn1.sdebug)

    if s.schcon.debug:
        s.wlog(0, "RDSET: Reading setup {} {}".format(isetf, setreq))
    
    catalog = SetupCatalog()

    start = int(s.setn1.nset)
    index = start
    if next((entry for entry in catalog.entries[:index] \
             if entry.setname == setreq), 
            None) is not None:
        s.wlog(1, "RDSET: Duplicate setup file name? ")
        s.wlog(1, "RDSET:{}".format(setreq))
        s.errlog("Check setup file names")
    
    s.wlog(0, "RDSET:   Reading setup file:      {}".format(setreq))
        
    mchan = s.setn2a.freqref.shape[0]
    maxpc = s.setc2.pcalx1.shape[0]
    state_defaults = {
        "logging":  ["STANDARD",        util.noop],
        "noisefrq": ["VLBA",            util.upper],
        "fe":       [["omit"] * 4,      util.foreach(util.lower)],
        "noise":    [["low-s"] * 4,     util.noop],
        "ifdist":   [[0] * 4,           util.foreach(
            lambda x: x.lower() if isinstance(x, str) else str(int(x)))],
        "azcolim":  [0.,                util.noop],
        "elcolim":  [0.,                util.noop],
        "ptincr":   [0.,                util.noop],
        "ptoff":    [0.,                util.noop],
        "period":   [1,                 util.noop],
        "level":    [-1,                util.noop],
        "samprate": [0.,                util.noop],
        "dualx":    [1.,                util.to_bool],
        "synth":    [[0.] * 3,          util.noop],
        "frswitch": [1.,                util.to_bool],
        "tpmode":   [0.,                util.noop],
        "format":   ["",                util.upper],
        "modetest": [1.,                util.to_bool],
        "dbe":      ["",                util.upper],
        "swtchdur": [15.,               util.noop],
        "firmfile": ["",                util.noop],
        "barrel":   ["not_set",         util.lower],
        "tpspeedh": [0.,                util.noop],
        "tpspeedl": [0.,                util.noop],
        "tpspeed":  [0.,                util.noop],
        "lcp50cm":  ["DEF",             util.upper],
        "rcp50cm":  ["DEF",             util.upper],
        "rchan":    ["",                util.upper],
        "lchan":    ["",                util.upper],
        "nchan":    [0,                 int],
        "band":     ["",                util.lower],
        "firstlo":  [[parameter.unset], util.extend_to(mchan)],
        "bbsyn":    [[0.] * mchan,      util.noop],
        "bbsyn2":   [[0.] * mchan,      util.noop],
        "bbfilter": [[0.],              util.extend_to(mchan)],
        "sideband": [[" "],             util.chain(util.foreach(util.upper), 
                                                   util.extend_to(mchan))],
        "netside":  [[" "],             util.chain(util.foreach(util.upper), 
                                                   util.extend_to(mchan))],
        "ifchan":   [[""] * mchan,      util.foreach(util.upper)],
        "bits":     [[1],               util.extend_to(mchan)],
        "bbc":      [[0.] * mchan,      util.noop],
        "freqoff":  [[0.] * mchan,      util.noop],
        "freqref":  [[0.],              util.extend_to(mchan)],
        "pol":      [[""],              util.chain(util.foreach(util.upper), 
                                                   util.extend_to(mchan))],
        "track1":   [[0.] * mchan,      util.noop],
        "track2":   [[0.] * mchan,      util.noop],
        "track3":   [[0.] * mchan,      util.noop],
        "track4":   [[0.] * mchan,      util.noop],
        "track5":   [[0.] * mchan,      util.noop],
        "track6":   [[0.] * mchan,      util.noop],
        "track7":   [[0.] * mchan,      util.noop],
        "track8":   [[0.] * mchan,      util.noop],
        "string1":  ["",                util.noop],
        "string2":  ["",                util.noop],
        "string3":  ["",                util.noop],
        "string4":  ["",                util.noop],
        "pcal":     ["1MHZ",            util.upper],
        "pcalxb1":  [[""] * maxpc,      util.foreach(util.upper)],
        "pcalxb2":  [[""] * maxpc,      util.foreach(util.upper)],
        "pcalfr1":  [[0.] * maxpc,      util.noop],
        "pcalfr2":  [[0.] * maxpc,      util.noop],
        "m4patch":  ["astro",           util.upper],
        "dbbcfw":   ["",                util.chain(
            lambda x: str(x).rstrip("0").rstrip(".") 
            if type(x) == float else x, 
            util.upper)],
    }
    record_defaults = {
        "station": [[""] * s.setc2.setsta.shape[0], util.foreach(util.upper)],
        "endset":  [None,                           util.noop],
    }
    input_iterator.set_defaults(record_defaults, state_defaults)

    old_vla_parameters = {"flukeset", "flukea", "flukeb", "vlafeab", "vlafecd", 
                          "vlasyna", "vlasynb", "fefilter", "vlaif", "vlarot", 
                          "vlaband", "vlabw"}
    record_defaults.update(
        {key: [None, util.noop] for key in old_vla_parameters})

    attribute_to_key = {
        "setsta": "station",
        "speedh": "tpspeedh",
        "speedl": "tpspeedl",
        "sidebd": "sideband",
        "pcalx1": "pcalxb1",
        "pcalx2": "pcalxb2",
        "bbfilt": "bbfilter",
        "spcal": "pcal",
    }
    # update with default identity mapping, 
    # skip keys not stored directly
    attribute_names = set(itertools.chain(state_defaults.keys(), 
                                          record_defaults.keys())) - \
        {"tpspeed", 
         "freqoff",
         "track1",
         "track2",
         "track3",
         "track4",
         "track5",
         "track6",
         "track7",
         "track8",
         "string1",
         "string2",
         "string3",
         "string4",
         "endset",
        } - \
        old_vla_parameters
    attribute_to_key.update({attribute: attribute 
                             for attribute in attribute_names
                             if attribute not in attribute_to_key.values()})

    for record in input_iterator:
        values, present = util.merge_record_with_defaults(
            record, record_defaults, state_defaults)
        if "endset" in present:
            break

        if index >= catalog.maxsetup:
            s.errlog("RDSET: Too many setup groups ")
        
        entry = catalog.entries[index]
        entry.set_keyin_values(values, attribute_to_key)

        entry.setname = setreq
        entry.isetnum = isetf
        if entry.ptoff <= 0.:
            entry.ptoff = 6. * entry.ptincr
        if (entry.speedl == 0.) and (values["tpspeed"] != 0.):
            entry.speedl = values["tpspeed"]
        if entry.nchan > mchan:
            s.errlog("RDSET: Too many  channels in {}".format(setreq))
        
        justvla = all(station[:3] in ("VLA", "") for station in entry.setsta) \
                  and (entry.format == "NONE") and (entry.nchan == 0)
        if not justvla:
            if entry.nchan < 1:
                s.errlog("RDSET: NCHAN must be greater than 0.")

            entry.freqref = [entry.freqref[i] + values["freqoff"][i]
                             for i in range(entry.nchan)]

            for ichan in range(entry.nchan):
                pol_map = {"R": "RCP",
                           "L": "LCP"}
                if entry.pol[ichan] in pol_map.keys():
                    entry.pol[ichan] = pol_map[entry.pol[ichan]]
                elif (entry.ifchan[ichan] in pol_map.keys()) and \
                     (entry.pol[ichan] == ""):
                    entry.pol[ichan] = pol_map[entry.ifchan[ichan]]
                    
                if entry.ifchan[ichan] == "R":
                    entry.ifchan[ichan] = entry.rchan
                if entry.ifchan[ichan] == "L":
                    entry.ifchan[ichan] = entry.lchan

            entry.track = [values["track" + n] for n in "12345678"]
        #enf of if not justvla
        
        entry.string_bn = [values["string" + n] for n in "1234"]
        if any(values[key] is not None for key in old_vla_parameters):
            s.wlog(1, "RDSET - WARNING: Old VLA parameters used in input "
                   "setup: {}".format(index + 1))
            s.wlog(1, "                 They are no longer used and will be "
                   "removed soon. ")

        pcal_map = {
            "OFF": "off",
            "1MHZ": "1MHz",
            "5MHZ": "5MHz"
        }
        if entry.spcal in pcal_map.keys():
            entry.spcal = pcal_map[entry.spcal]
        else:
            s.errlog("CHKSET: Invalid PCAL specification: {}".format(
                entry.spcal))

        if (entry.dbbcfw not in ("", "104", "105", "105E", "105F", "106E", 
                                 "106F", "107")) and \
            (entry.dbbcfw not in dbbc_firmware_warnings):
            s.wlog(1, "RDSET - WARNING: {} is not a known DBBC firmware "
                   "version.".format(entry.dbbcfw))
            s.wlog(1, "                 Heuristics might still work, but check "
                   "schedule carefully.")
            dbbc_firmware_warnings.add(entry.dbbcfw)
        
        index += 1

    s.setn1.nset = index
    catalog.write(range(start, index))

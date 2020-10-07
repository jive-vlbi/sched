from . import getsta, getfreq, schfiles, gettim, gintent, toggle, infdb, \
    invla, schrep, scndup, getcov, getcor, times, sttant, parameter, schdefs
from ..catalog import ScanCatalog, SetupFileCatalog
from .. import util, key

import schedlib as s

import numpy as np

import sys

maxsta = s.schn1.stanum.shape[0]
maxchan = s.schn2b.freq.shape[0]

record_defaults = {
    "comment":  ["",                    util.noop],
    "crddop":   [1.,                    util.to_bool],
    "crdnodop": [1.,                    util.to_bool],
    "doppler":  [1.,                    util.to_bool],
    "nodop":    [1.,                    util.to_bool],
    "duration": [parameter.unset,       util.noop],
    "freqlist": [[parameter.unset] * 2, util.noop],
    "geoseg":   [0.,                    util.multiply_by(parameter.onesec)],
    "group":    [1,                     util.noop],
    "repeat":   [1,                     util.noop],
    "intents":  [[],                    util.noop],
    "record":   [1.,                    util.to_bool],
    "norecord": [1.,                    util.to_bool],
    "ptvlba":   [1.,                    util.to_bool],
    "noptvlba": [1.,                    util.to_bool],
    "tavlba":   [1.,                    util.to_bool],
    "notavlba": [1.,                    util.to_bool],
    "tsys":     [1.,                    util.to_bool],
    "notsys":   [1.,                    util.to_bool],
    "pn3db":    [1.,                    util.to_bool],
    "nopn3db":  [1.,                    util.to_bool],
    "point":    [-999,                  util.noop],
    "scantag":  ["",                    util.noop],
    "start":    [parameter.unset,       util.noop],
    "stop":     [parameter.unset,       util.noop],
    "tapefile": [None,                  util.noop],
    "vlatsys":  [None,                  util.noop],
    "vlantsys": [None,                  util.noop],
    "nopeak":   [None,                  util.noop],
    "exit":     [None,                  util.noop],
    "lineinit": [None,                  util.noop],
    "srccat":   [None,                  util.noop],
    "peakinit": [None,                  util.noop],
    "stacat":   [None,                  util.noop],
    "satinit":  [None,                  util.noop],
    "setinit":  [None,                  util.noop],
    "pcenters": [None,                  util.noop],
    "coverlet": [None,                  util.noop],
    "nchan":    [None,                  util.noop],
    "autotape": [None,                  util.noop],
    "tape":     [None,                  util.noop],
    "fastfor":  [None,                  util.noop],
    "reverse":  [None,                  util.noop],
    "stations": [[],                    util.foreach(util.upper)],
}

# if an inline catalog is read, these values have to be passed to the next 
# record read
keep_for_next = {
    'start', 'stop', 'repeat', 'group', 'duration', 'dwell', 'nopeak', 
    'point', 'scantag', 'geoseg', 'higroup', 'doppler', 'nodop', 'crddop', 
    'crdnodop', 'record', 'norecord', 'ptvlba', 'noptvlba', 'tavlba', 
    'notavlba', 'tsys', 'notsys', 'vlatsys', 'vlantsys', 'stations', 
    'comment', 'intents', 'scanexps' }

state_defaults = {
    "schedule":  ["",                      util.noop],
    "freqfile":  [schdefs("frequency"),    util.noop],
    "stafile":   [schdefs("stations"),     util.noop],
    "locfile":   [schdefs("location"),     util.noop],
    "msgfile":   [schdefs("messages"),     util.noop],
    "peakfile":  [schdefs("peakcommand"),  util.noop],
    "srcfile":   [schdefs("sources"),      util.noop],
    "srcfile2":  ["NONE",                  util.noop],
    "ephfile":   ["NONE",                  util.noop],
    "dosta":     ["ALL",                   util.upper],
    "day":       [0,                       util.noop],
    "month":     [1,                       util.noop],
    "year":      [0,                       util.noop],
    "dwell":     [[0.] * 3,                util.noop],
    "gap":       [0.,                      util.multiply_by(1/86400.)],
    "prescan":   [0.,                      util.multiply_by(1/86400.)],
    "datapath":  ["IN2DISK",               util.upper],
    "grabto":    ["NONE",                  util.upper],
    "grabtime":  [[parameter.unset] * 2,   util.noop],
    "grabgap":   [0.,                      util.noop],
    "source":    ["DUMMY",                 util.upper],
    "qual":      [0.,                      util.noop],
    "centers":   ["",                      util.upper],
    "tant1":     [0.,                      util.to_bool],
    "tant2":     [0.,                      util.to_bool],
    "caltime":   [120.,                    util.noop],
    "ptslew":    [160.,                    util.noop],
    "pcal":      ["",                      util.upper],
    "focus":     [0.,                      util.noop],
    "rotation":  [0.,                      util.noop],
    "azcolim":   [0.,                      util.noop],
    "elcolim":   [0.,                      util.noop],
    "opmiss":    [0.,                      util.noop],
    "crdline":   ["",                      util.noop],
    "dodown":    [1.,                      util.to_bool],
    "preempt":   ["--",                    util.upper],
    "peak":      [parameter.unset,         util.noop],
    "autopeak":  [1.,                      util.to_bool],
    "pkwatch":   [1.,                      util.to_bool],
    "higroup":   [1.,                      util.noop],
    "setup":     ["DUMMY",                 util.noop],
    "freq":      [[0.],                    util.chain(
        util.foreach(lambda x: round(x, 7)), util.extend_to(maxchan))],
    "bw":        [[0.],                    util.chain(
        util.foreach(lambda x: round(x, 7)), util.extend_to(maxchan))],
    "crdnch":    [0,                       int],
    "crdch1":    [0.,                      util.noop],
    "crdsetch":  [[0.] * 4,                util.noop],
    "crdfreq":   [[0.],                    util.chain(
        util.foreach(lambda x: round(x, 7)), util.extend_to(maxchan))],
    "crdbw":     [[0.],                    util.chain(
        util.foreach(lambda x: round(x, 7)), util.extend_to(maxchan))],
    "dopsrc":    ["",                      util.upper],
    "dopcal":    [1.,                      util.to_bool],
    "linename":  ["",                      util.upper],
    "dopincr":   [[0., 0.],                util.noop],
    "vlamode":   ["ZZ",                    util.upper],
    "vlapeak":   ["OFF",                   util.upper],
    "vlapsrc":   ["",                      util.upper],
    "vlabw":     [None,                    util.noop],
    "vlaband":   [None,                    util.noop],
    "vlainteg":  [0.,                      util.noop],
    "vlaptime":  [10.,                     util.noop],
    "vlatype":   ["VLBI",                  util.noop],
    "vlausern":  [600.,                    util.noop],
    "iatutc":    [0.,                      util.noop],
    "vlarfant":  [10.,                     util.noop],
    "opminel":   [2.,                      util.noop],
    "opminant":  [0.,                      util.noop],
    "opminsep":  [0.,                      util.noop],
    "opslewwt":  [0.,                      util.noop],
    "opslewti":  [360.,                    util.noop],
    "ophlimwt":  [0.,                      util.noop],
    "ophlimti":  [1800.,                   util.noop],
    "opha":      [0.,                      util.noop],
    "ophawid":   [0.,                      util.noop],
    "ophawt":    [1.,                      util.noop],
    "ophmaxdt":  [7200.,                   util.noop],
    "wrap24":    [1.,                      util.to_bool],
    "expt":      ["No description given.", util.noop],
    "expcode":   ["NUG",                   util.noop],
    "linepg":    [55.,                     util.noop],
    "tpref":     [-1.,                     util.noop],
    "ptdur":     [20.,                     util.noop],
    "precdate":  [1979.9,                  util.noop],
    "dovex":     [0.,                      util.to_bool],
    "vextest":   [1.,                      util.to_bool],
    "domka":     [1.,                      util.to_bool],
    "obstype":   ["NONE",                  util.upper],
    "doscans":   [[0., 0.],                util.noop],
    "optmode":   ["NONE",                  util.upper],
    "opdur":     [0.,                      util.multiply_by(parameter.onesec)],
    "opnosub":   [1.,                      util.to_bool],
    "opskip":    [0.,                      util.noop],
    "optslew":   [1.,                      util.noop],
    "optlowt":   [15.,                     util.noop],
    "ophasta":   ["PT",                    util.upper],
    "tapesync":  [1.,                      util.to_bool],
    "opprtlev":  [0.,                      util.noop],
    "opelprio":  [[0.] * 4,                util.noop],
    "maplim":    [[0.] * 4,                util.noop],
    "gridnr":    [20.,                     util.noop],
    "gridnt":    [36.,                     util.noop],
    "gridmin":   [25.,                     util.noop],
    "gridmax":   [250.,                    util.noop],
    "gridw0":    [0.,                      util.noop],
    "gridstep":  [3.,                      util.noop],
    "gridmeas":  ["COUNT",                 util.upper],
    "gridvla":   [1.,                      util.to_bool],
    "uvmfs":     [[1., 1.],                util.noop],
    "geoprt":    [-1.,                     util.noop],
    "geotries":  [20,                      util.noop],
    "geoback":   [100.,                    util.noop],
    "geoslew":   [1.,                      util.noop],
    "geoslow":   [40.,                     util.noop],
    "geosrep":   [100,                     util.noop],
    "geohiel":   [40,                      util.noop],
    "geolowel":  [23.,                     util.noop],
    "geosrcs":   [[],                      util.foreach(util.upper)],
    "version":   [0.,                      util.noop],
    "piname":    ["",                      util.noop],
    "address1":  ["",                      util.noop],
    "address2":  ["",                      util.noop],
    "address3":  ["",                      util.noop],
    "address4":  ["",                      util.noop],
    "phone":     ["",                      util.noop],
    "email":     ["",                      util.noop],
    "fax":       ["",                      util.noop],
    "obsphone":  ["",                      util.noop],
    "obsmode":   ["",                      util.noop],
    "note1":     ["",                      util.noop],
    "note2":     ["",                      util.noop],
    "note3":     ["",                      util.noop],
    "note4":     ["",                      util.noop],
    "correl":    ["",                      util.strip_upper],
    "coravg":    [[0., ""],                util.noop],
    "coravg2":   [[0., ""],                util.noop],
    "corchan":   [[0., 0.],                util.noop],
    "cornant":   [0.,                      util.noop],
    "corsrcs":   ["Not specified.",        util.upper],
    "corpol":    ["",                      util.upper],
    "corwtfn":   ["UNIFORM",               util.upper],
    "cortape":   ["",                      util.upper],
    "cordfmt":   ["FITS",                  util.upper],
    "corship1":  ["",                      util.noop],
    "corship2":  ["",                      util.noop],
    "corship3":  ["",                      util.noop],
    "corship4":  ["",                      util.noop],
    "cornote1":  ["",                      util.noop],
    "cornote2":  ["",                      util.noop],
    "cornote3":  ["",                      util.noop],
    "cornote4":  ["",                      util.noop],
    "rotpat":    [0.,                      util.noop],
    "focoff":    [[0.] * 20,               util.noop],
    "rotoff":    [[0.] * 20,               util.noop],
    "sumitem":   [[""] * 10,               util.foreach(util.upper)],
    # the parameter LST is both a boolean flag (implemented by floats) 
    # and a string for the LST station, the logic is implemented in times()
    "lst":       [1.0,                     util.noop],
    "tantsta1":  [[""] * maxsta,           util.foreach(util.upper)],
    "tantsta2":  [[""] * maxsta,           util.foreach(util.upper)],
    "prestart":  [parameter.unset,         util.multiply_by(parameter.onesec)],
    "minpause":  [parameter.unset,         util.multiply_by(parameter.onesec)],
    "debug":     [1.,                      util.to_bool],
    "overwrite": [1.,                      util.to_bool],
    "override":  [1.,                      util.to_bool],
    "nosetup":   [1.,                      util.to_bool],
    "plot":      [1.,                      util.to_bool],
    "pubplot":   [1.,                      util.to_bool],
    "scanexps":  [[],                      util.noop],
}

attribute_to_key = {
    "prescan": "prescan",
    "gap": "gap",
    "datapath": "datapath",
    "grabto": "grabto",
    "grabtime": "grabtime",
    "grabgap": "grabgap",
    "scnsrc": "source",
    "qual": "qual",
    "geolen": "geoseg",
    "centers": "centers",
    "tant1": "tant1",
    "tant2": "tant2",
    "annot": "comment",
    "caltime": "caltime",
    "ptslew": "ptslew",
    "pcal": "pcal",
    "focus": "focus",
    "rotation": "rotation",
    "opmiss": "opmiss",
    "scantag": "scantag",
    "crdline": "crdline",
    "dodown": "dodown",
    "preempt": "preempt",
    "dopeak": "peak",
    "point": "point",
    "higroup": "higroup",
    "freq": "freq",
    "bw": "bw",
    "crdnch": "crdnch",
    "crdch1": "crdch1",
    "crdsetch": "crdsetch",
    "crdfreq": "crdfreq",
    "crdbw": "crdbw",
    "dopsrc": "dopsrc",
    "dopcal": "dopcal",
    "lines": "linename",
    "vlamode": "vlamode",
    "vlapeak": "vlapeak",
    "vlaphs": "vlapsrc",
    "vlainteg": "vlainteg",
    "vlaptime": "vlaptime",
    "opminel": "opminel",
    "opmian": "opminant",
    "opminsep": "opminsep",
    "opslewwt": "opslewwt",
    "opslewti": "opslewti",
    "ophlimwt": "ophlimwt",
    "ophlimti": "ophlimti",
    "opha": "opha",
    "ophawid": "ophawid",
    "ophawt": "ophawt",
    "ophmaxdt": "ophmaxdt",
    "dur": "duration",
    "prestart": "prestart",
    "minpause": "minpause",
    "sazcol": "azcolim",
    "selcol": "elcolim",
    "scanexps": "scanexps",
}

def schin(stdin):
    global record_defaults, state_defaults

    catalog = ScanCatalog()

    year = np.zeros(shape=(catalog.maxscan,), dtype="int32")
    day = np.zeros(shape=(catalog.maxscan,), dtype="int32")
    stop = np.zeros(shape=(catalog.maxscan,), dtype="double")
    start = np.zeros(shape=(catalog.maxscan,), dtype="double")
    mjd1 = 0
    if state_defaults["schedule"][0] == "":
        input_ = stdin
    else:
        try:
            input_ = open(util.expand_file_name(state_defaults["schedule"][0]),
                          "r")
        except Exception as e:
            s.wlog(1, str(e))
            s.errlog("SCHFILES: Problem opening schedule file {}".format(
                state_defaults["schedule"][0]))
    # the iterator might change, from file/stdin to (another) file,
    input_iterator = key.KeyfileIterator(input_, 
                                         record_defaults, state_defaults)
    
    s.schn1.nsta = 0
    s.schsat.nsat = 0
    marker = -1
    s.schsta.msta = 0
    s.schsou.msrc = 0
    s.schsf.nsetf = 0
    s.schn1.scan1 = 1
    s.schsou.ngeo = 0
    gotsat = False
    doinit = True
    s.schn1.dwells = False
    dostwarn = True
    s.schcon.dovex = True
    gotvex = False
    s.schcon.coverlet = False
    s.schn5.allvlba = True
    s.schsou.anygeo = False
    s.schn1.fuzzy = False
    s.schn1.gotvlba = False
    s.schn1.gotpreem = False
    
    s.schn2a.srcnum.fill(0)
    s.schn2a.idopsrc.fill(0)
    s.schn3.ivlaphs.fill(0)
    s.schn3.vlainteg.fill(0)
    
    s.schsou.sused.fill(False)
    s.schsou.usedrec.fill(False)
    s.schsou.usedphs.fill(False)
    s.schsou.usedcent.fill(False)
    s.schn1.srcatn.fill(0)
    s.schsou.srlstn.fill(0)
    s.schsou.didndop.fill(0)
    s.schsou.dopped.fill(False)
    s.schcsc.csused.fill(b' ')
    s.schsou.planet.fill(False)
    s.schsou.satel.fill(False)
    s.schsou.satn.fill(0)

    s.schn2a.nintent = 0
    s.schc2c.intent.fill(b' ' * s.schc2c.intent.itemsize)
    s.schn2a.nscint.fill(0)
    s.schn2a.iscint.fill(0)

    restart = False

    index = 0
    while True:
        try:
            record = next(input_iterator)
            # a weird one, only dwell[0]'s default does not carry over
            state_defaults["dwell"][0][0] = parameter.unset
            # make a new scanexps overwrite the old list, 
            # instead of the default behaviour of slicing into it
            old_scanexps = state_defaults["scanexps"][0]
            state_defaults["scanexps"][0] = []
            if restart:
                old_values, old_present = values, present
                values, present = util.merge_record_with_defaults(
                    record, record_defaults, state_defaults)
                values.update({k: old_values[k] for k in keep_for_next
                               if k not in present})
                present.update(old_present & keep_for_next)
                restart = False
            else:
                values, present = util.merge_record_with_defaults(
                    record, record_defaults, state_defaults)
            if "scanexps" not in present:
                state_defaults["scanexps"][0] = old_scanexps
                values["scanexps"] = old_scanexps
            
            if "exit" in present:
                if index == 0:
                    s.wlog(1, "SCHIN:  EXIT requested.  Shutting down.")
                    s.delscr(False)
                    sys.exit(0)
                break

            s.schcon.debug = values["debug"]
            s.schcon.overwrit = values["overwrite"]
            s.schcon.override = values["override"]

            s.schsco.msgfile = util.resize_string(util.expand_file_name(
                values["msgfile"]), s.schsco.msgfile.itemsize, "msgfile")

            if s.schcon.debug and (index < 3):
                s.wlog(0, "DIVERT: Starting")

            s.schcon.freqlist = values["freqlist"]
            if "freqlist" in present:
                s.schsco.freqfile = util.resize_string(util.expand_file_name(
                    values["freqfile"]), s.schsco.freqfile.itemsize, "freqfile")
                getfreq()
                s.wlog(1, "DIVERT:   Frequency table written.  Stopping.")
                sys.exit(0)

            s.schcon.noset = values["nosetup"]
            s.schcon.plot = values["plot"]
            s.schcon.pubplot = values["pubplot"]

            schedule = util.expand_file_name(values["schedule"])
            if (schedule != "") and \
               ((input_iterator.input_ is stdin) or 
                (input_iterator.input_.name != schedule)):
                if s.schcon.debug:
                    s.wlog(0, "SCHFILES:  About to open {}".format(schedule))
                if input_iterator.input_ is not stdin:
                    input_iterator.input_.close()
                try:
                    input_ = open(schedule, "r")
                    input_iterator = key.KeyfileIterator(
                        input_, record_defaults, state_defaults)
                    restart = True
                    continue # read next record from new iterator
                except Exception as e:
                    s.wlog(1, str(e))
                    s.errlog("SCHFILES: Problem opening schedule file {}".\
                             format(schedule))
            gotsat, restart = schfiles(input_iterator, stdin, 
                                       values, present, gotsat)
            input_iterator.set_defaults(record_defaults, state_defaults)
            if restart:
                continue

            if index > catalog.maxscan:
                s.errlog("SCHIN: Too many scans, maximum {}".format(
                    catalog.maxscan))
            entry = catalog.entries[index]

            s.schsco.dosta = util.resize_string(
                values["dosta"], s.schsco.dosta.itemsize, "dosta")
            if (values["dosta"] != "ALL") and dostwarn:
                s.wlog(0, "SCHIN:  DOSTA specified as {}".format(
                    values["dosta"]))
                s.wlog(0, "        Some stations may be skipped.")
                dostwarn = False

            # nopeak overwrites peak and its default
            if "nopeak" in present:
                state_defaults["peak"][0] = values["peak"] = -1.

            if ("scanexps" in present) and \
               ("NONE" in (v.upper() for v in values["scanexps"])):
                state_defaults["scanexps"][0] = values["scanexps"] = []
            
            entry.set_keyin_values(values, attribute_to_key)

            mjd1 = gettim(values, catalog.entries, index,
                          start, stop, day, year, mjd1)
            gotvex = getsta(stdin, values, index, gotvex, mjd1)

            if entry.scnsrc == "":
                s.errlog("SCHIN: Need source name - blank specified.")

            if entry.geolen > 0:
                s.schsou.anygeo = True
                entry.geoiscn = index + 1

            pcal_map = {
                "OFF": "off",
                "1MHZ": "1MHz",
                "5MHZ": "5MHz"
            }
            if entry.pcal in pcal_map.keys():
                entry.pcal = pcal_map[entry.pcal]
            elif entry.pcal != "":
                s.errlog("SCHIN: Invalid PCAL ({}) specified in scan {}".\
                         format(entry.pcal, index+1))
            if entry.preempt == "EXTRA":
                s.schn1.fuzzy = True
            elif entry.preempt == "--":
                if entry.geolen > 0.:
                    entry.preempt = "NO"
                else:
                    entry.preempt = "OK"
            if entry.preempt != "OK":
                s.schn1.gotpreem = True

            gintent(values["intents"], catalog.entries, index)
            
            # toggle pairs
            if index == 0:
                get_default = lambda attribute: False
            else:
                prev_entry = catalog.entries[index-1]
                get_default = lambda attribute: getattr(prev_entry, attribute)
            for attribute, key1, key2 in (
                    ("notsys", "notsys", "tsys"),
                    ("norec", "norecord", "record"),
                    ("pntvlba", "ptvlba", "noptvlba"),
                    ("tanvlba", "tavlba", "notavlba"),
                    ("dopn3db", "pn3db", "nopn3db")):
                setattr(entry, attribute,
                        toggle(values, present, key1, key2, 
                               get_default(attribute)))
            
            s.schcon.autopeak = values["autopeak"]
            s.schcon.pkwatch = values["pkwatch"]
            s.schsco.peakfile = util.resize_string(util.expand_file_name(
                values["peakfile"]), s.schsco.peakfile.itemsize, "peakfile")

            entry.setnum = SetupFileCatalog.extend_with(
                util.expand_file_name(values["setup"]), 
                " SCHIN: Too many setup files. ")
            
            infdb(values, present, catalog.entries, index)
            invla(values, present, catalog.entries, index, 1)
            
            index, marker = schrep(values, catalog.entries, index, marker,
                                   start, stop, day, year)

            if "nchan" in present:
                s.wlog(1, "SCHIN: NCHAN in main schedule now ignored.")

            if s.schcon.debug:
                s.wlog(0, "SCHIN: Finished reading scan: {}".format(index+1))

            index += 1
        except StopIteration:
            break
    # end of scan reading loop
    
    if input_iterator.input_ is not stdin:
        input_iterator.input_.close()

    s.schn1.nscans = index
    s.schn1.scanl = s.schn1.nscans
    if index <= 0:
        s.errlog("SCHIN: No input scans")

    s.schn1.wrap24 = values["wrap24"]
    if s.schn1.wrap24:
        for from_ in range(s.schn1.scanl):
            if from_ > 0:
                if (start[from_] != parameter.unset) or \
                   (stop[from_] != parameter.unset):
                    s.errlog("SCHIN: Do not use START or STOP times "
                             "after scan 1 with WRAP24. See scan: {}".\
                             format(from_+1))
            to = from_ + s.schn1.nscans
            scndup(to, from_, False, "SCHIN", use_direct_access=False)
            catalog.entries[to].annot = catalog.entries[from_].annot
            start[to] = parameter.unset
            stop[to] = parameter.unset
            day[to] = day[from_]
            year[to] = year[from_]
        s.schn1.scanl = 2 * s.schn1.nscans
        s.schn1.nscans = s.schn1.scanl
        index *= 2
    
    s.schc1.expt = util.resize_string(values["expt"], s.schc1.expt.itemsize, 
                                      "expt")
    s.schc1.expcode = util.resize_string(values["expcode"], 
                                         s.schc1.expcode.itemsize, "expcode")
    s.schcon.linepg = values["linepg"]
    s.schn5.tpref = values["tpref"]
    s.schn4.ptdur = values["ptdur"]
    s.schcon.precdate = values["precdate"]

    s.schcon.dovex = (values["dovex"] or gotvex)
    s.schcon.vextest = values["vextest"]

    s.schcon.domka = values["domka"]

    s.schsco.ephfile = util.resize_string(util.expand_file_name(
        values["ephfile"]), s.schsco.ephfile.itemsize, "ephfile")

    s.chkcode(s.schc1.expcode)

    obstype = values["obstype"]
    if obstype[:4] == "MARK":
        obstype = "MK" + obstype[4:]
    s.schsco.obstyp = util.resize_string(obstype, s.schsco.obstyp.itemsize, 
                                         "obstype")
    s.schn1.mark2 = (obstype == "MKII")
    s.schn1.vlbitp = (obstype in ("VLBA", "MKIII", "VLBI", "MKIV"))
    s.schn1.vlaonly = (obstype == "VLA")
    s.schn1.notape = (obstype in ("VLA", "NONE", "PTVLBA", "CONFIG"))
    s.schn1.config = (obstype == "CONFIG")
    if not (s.schn1.mark2 or s.schn1.vlbitp or s.schn1.vlaonly or 
            obstype in ("NONE", "PTVLBA", "CONFIG")):
        s.wlog(1, " SCHIN: Invalid OBSTYPE: {}".format(obstype))
        s.errlog(" SCHIN: OBSTYPE must be MKII, MKIII, VLBA, MKIV, "
                 "VLBI, VLA, NONE, or CONFIG")

    s.schn1.doscans = values["doscans"]
    if ((s.schn1.doscans[0] == 0) != (s.schn1.doscans[1] == 0)):
        s.errlog("If using DOSCANS, specify both!")

    s.schsco.optmode = util.resize_string(values["optmode"], 
                                          s.schsco.optmode.itemsize, "optmode")
    s.schcon.opdur   = values["opdur"]
    s.schcon.opnosub = values["opnosub"]
    s.schcon.opskip  = values["opskip"]
    s.schcon.optslew = values["optslew"]
    s.schcon.optlowt = values["optlowt"]
    s.schsco.ophasta = util.resize_string(values["ophasta"], 
                                          s.schsco.ophasta.itemsize, "ophasta")
    s.schcon.tapesync = values["tapesync"]
    s.schcon.opprtlev = values["opprtlev"]

    s.schcon.opelprio = values["opelprio"]
    s.schcon.maplim = values["maplim"]

    s.schcon.gridnr   = values["gridnr"]
    s.schcon.gridnt   = values["gridnt"]
    s.schcon.gridmin  = values["gridmin"]
    s.schcon.gridmax  = values["gridmax"]
    s.schcon.gridw0   = values["gridw0"]
    s.schcon.gridstep = values["gridstep"]
    s.schsco.gridmeas = util.resize_string(values["gridmeas"], 
                                           s.schsco.gridmeas.itemsize, 
                                           "gridmeas")
    s.schcon.gridvla  = values["gridvla"]
    s.schcon.gridused = False
    s.schcon.nmfs = values["uvmfs"][0]
    s.schcon.mfsrat = values["uvmfs"][1]

    mgeo = s.schsou.geosrci.shape[0]
    geosrcs = [util.resize_string(src, s.schcsc.geosrc.itemsize, "geosrcs") 
               for src in values["geosrcs"]]
    s.schsou.ngeo = len(geosrcs[:mgeo])
    for i, src in enumerate(geosrcs):
        s.schcsc.geosrc[i] = src

    if s.schsou.anygeo and s.schsou.ngeo == 0:
        s.errlog("Geodetic segments requested, but no GEOSRCS given.")

    if s.schsou.anygeo:
        for entry in catalog.entries:
            if entry.scnsrc == "GEOSEG":
                entry.scnsrc = geosrcs[0]
    
    s.schsou.geoprt   = values["geoprt"]
    s.schsou.geotries = values["geotries"]
    s.schsou.geoback  = values["geoback"]
    s.schsou.geoslew  = values["geoslew"]
    s.schsou.geoslow  = values["geoslow"]
    s.schsou.geosrep  = values["geosrep"]
    s.schsou.geohiel  = values["geohiel"]
    s.schsou.geolowel = values["geolowel"]

    getcov(values)
    getcor(values)

    s.schn4.rotpat = values["rotpat"]
    s.schn4.foc = values["focoff"]
    s.schn4.rot = values["rotoff"]

    if "tapefile" in present:
        s.errlog("TAPEFILE given but tape initialization no longer "
                 "supported by SCHED")
    s.tptpns()

    for i, sumitem in enumerate(values["sumitem"]):
        s.schsco.sumitem[i] = util.resize_string(sumitem, 
                                                 s.schsco.sumitem.itemsize, 
                                                 "sumitem")
    if values["sumitem"][0] == "":
        s.schsco.sumitem[0] = "ELA".ljust(s.schsco.sumitem.itemsize)
        s.schsco.sumitem[1] = "DWELL".ljust(s.schsco.sumitem.itemsize)

    invla(values, set(), catalog.entries, index, 2) # index is dummy in case 2

    if s.schcon.debug:
        s.wlog(0, "SCHIN: About to read catalogs.")

    catalog.write(range(index))
    
    times(values["lst"], start, stop, day, year)
    
    s.schcsc.srcfile = util.resize_string(util.expand_file_name(
        values["srcfile"]), s.schcsc.srcfile.itemsize, "srcfile")
    s.schcsc.srcfile2 = util.resize_string(util.expand_file_name(
        values["srcfile2"]), s.schcsc.srcfile2.itemsize, "srcfile2")

    sttant(values["tantsta1"], values["tantsta2"])

    s.schsco.freqfile = util.resize_string(util.expand_file_name(
        values["freqfile"]), s.schsco.freqfile.itemsize, "freqfile")
    s.schcon.freqlist = values["freqlist"]

    if "autotape" in present:
        s.wlog(1, "SCHIN:  Obsolete parameter AUTOTAPE given.  Ignored.")
    if "tape" in present:
        s.wlog(1, "'SCHIN:  Obsolete parameter TAPE given.  Ignored.")
    if "fastfor" in present:
        s.wlog(1, "SCHIN:  Obsolete parameter FASTFOR given.  Ignored.")
    if "reverse" in present:
        s.wlog(1, "SCHIN:  Obsolete parameter REVERSE given.  Ignored.")

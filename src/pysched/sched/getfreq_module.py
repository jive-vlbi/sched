from ..catalog import FrequencyCatalog
from .. import util
from ..key import read_keyfile

import schedlib as s

def getfreq():
    freqfile = bytes(s.schsco.freqfile).decode().strip()
    
    s.wlog(0, "GETFREQ: Reading frequency file:  {}".format(freqfile))
    
    try:
        f = open(freqfile, "r")
    except Exception as e:
        s.wlog(1, str(e))
        s.errlog("GETFREQ: Problem opening frequency file")
    else:
        with f:
            keyin_data = read_keyfile(f)
    
    catalog = FrequencyCatalog()
    
    if len(keyin_data) > catalog.maxfreq:
        s.errlog("GETFREQ: Too many frequency groups. ")

    maxsta = s.frqc.fstnam.shape[0]
    state_defaults = {
        "version": ["", util.noop]
    }
    record_defaults = {
        "station":  [[""] * maxsta, util.foreach(util.upper)],
        "name":     ["",            util.noop],
        "note":     ["",            util.noop],
        "priority": [0.,            util.noop],
        "ifname":   [[],            util.noop],
        "altifn":   [[],            util.noop],
        "fe":       [[],            util.foreach(util.lower)],
        "pol":      [[],            util.foreach(util.upper)],
        "rf1":      [[],            util.noop],
        "rf2":      [[],            util.noop],
        "lo1":      [[],            util.noop],
        "ch1rf1":   [[],            util.noop],
        "ch1rf2":   [[],            util.noop],
        "dualx":    [1.,            util.to_bool],
        "syn":      [[0., 0., 0.],  util.noop],
        "lcp50cm":  ["NARROW",      util.upper],
        "rcp50cm":  ["NARROW",      util.upper],
        "vlabw":    [None,          util.noop], # not used
        "vlaband":  [None,          util.noop], # not used
        "flukea":   [None,          util.noop], # not used
        "flukeb":   [None,          util.noop], # not used
        "vlafeab":  [None,          util.noop], # not used
        "vlafecd":  [None,          util.noop], # not used
        "vlasyna":  [None,          util.noop], # not used
        "vlasynb":  [None,          util.noop], # not used
        "fefilter": [None,          util.noop], # not used
        "tscal":    [None,          lambda x: x if x is None else x.upper()]
    }

    # map from frequency catalog entry attributes to keyin keywords
    attribute_to_key = {
        "frname": "name",
        "frnote": "note",
        "fstnam": "station",
        "prio": "priority",
        "fifnam": "ifname",
        "faltif": "altifn",
        "tscal": "tscal"
    }
    # above are the exceptions to the rule: remove first 'f' to go from 
    # attribute to key
    attribute_to_key.update({
        attribute: attribute[1:] for attribute in catalog.attributes
        if attribute not in attribute_to_key.keys()})

    for index, record in enumerate(keyin_data):
        values, present = util.merge_record_with_defaults(
            record, record_defaults, state_defaults)

        # copy all values to the common block placeholder
        entry = catalog.entries[index]
        entry.set_keyin_values(values, attribute_to_key)
        if entry.tscal not in (None, "CONT", "GAP"):
            raise RuntimeError("Unknown TSCAL in frequency catalog for {} "
                               "entry {}:  {}".format(
                                   entry.station, entry.name, entry.tscal))
    
    catalog.write(range(len(keyin_data)))

    s.frqn.nfreq = len(keyin_data)
    s.schsco.freqver =  util.resize_string(state_defaults["version"][0], 
                                           s.schsco.freqver.itemsize, "version")

    s.listfreq()

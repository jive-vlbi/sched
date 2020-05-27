from ..catalog import SatelliteCatalog
from . import parameter
from .. import util

import schedlib as s

record_defaults = {
    "satfile": ["NONE", util.expand_file_name],
    "tlefile": ["NONE", util.expand_file_name],
    "endsat":  [None, util.noop],
}
state_defaults = {
    "satname": ["", util.upper],
    "satnum": [parameter.unset, util.noop],
    "kerfile": ["NONE", util.expand_file_name],
}

def satini(input_iterator):
    global record_defaults, state_defaults
    input_iterator.set_defaults(record_defaults, state_defaults)
    
    if s.schcon.debug:
        s.wlog(0, "SATINI: starting.")
    
    catalog = SatelliteCatalog()
    attribute_to_key = dict(zip(catalog.attributes, catalog.attributes))

    start = int(s.schsat.nsat)
    index = start
    for record in input_iterator:
        values, present = util.merge_record_with_defaults(
            record, record_defaults, state_defaults)
        if "endsat" in present:
            break
        entry = catalog.entries[index]
        entry.set_keyin_values(values, attribute_to_key)
        index += 1
        s.wlog(0, "SATINI: Satellite {} {} {}".format(
            index, entry.satname, entry.satnum))
        for attribute in ["kerfile", "satfile", "tlefile"]:
            s.wlog(0, "'        {}: {}".format(attribute.upper(), 
                                               getattr(entry, attribute)))

        error = False
        if ("satfile" in present) and ("tlefile" in present):
            s.wlog(1, "A satellite can only have a SATFILE or a TLEFILE, "
                   "not both.")
            error = True

        if ("satfile" not in present) and ("tlefile" not in present):
            s.wlog(1, "'SATINI:  A satellite must have a SATFILE or a TLEFILE.")
            error = True

        if error:
            s.wlog(1, "  The problem occurred with satellite {} {}".format(
                index, entry.satname))
            s.wlog(1, "'     SATFILE:   {}".format(entry.satfile))
            s.wlog(1, "'     TLEFILE:   {}".format(entry.tlefile))
            s.errlog(" Exactly one of the file names should be specified.")

    s.schsat.nsat = index
    catalog.write(range(start, index))

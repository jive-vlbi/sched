from ..catalog import PhaseCenterCatalog
from .. import util
from .. import key

import schedlib as s

def pcread(input_iterator):
    global record_defaults
    record_defaults = {
        "name":    ["",   util.noop],
        "sources": [[],   util.foreach(util.upper)],
        "endcent": [None, util.noop],
    }
    input_iterator.set_defaults(record_defaults, {})

    attribute_to_key = {
        "ctrname": "name",
        "ctrsrcn": "sources"
    }
    
    if s.schcon.debug:
        s.wlog(0, "PCREAD:  Starting to read groups of phase centers")
    
    catalog = PhaseCenterCatalog()

    start = int(s.schsou.ncent)
    index = start
    for record in input_iterator:
        values, present = util.merge_record_with_defaults(
            record, record_defaults, {})
        if "endcent" in present:
            break
        entry = catalog.entries[index]
        entry.set_keyin_values(values, attribute_to_key)
        index += 1

    s.schsou.ncent = index
    catalog.write(range(start, index))

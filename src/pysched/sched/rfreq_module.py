from ..catalog import LineRestFrequencyCatalog
from .. import util, key

import schedlib as s

s.schlin.nlgp = 0

def rfreq(input_iterator):
    if s.schcon.debug:
        s.wlog(0, "RFREQ:  Starting to read line frequencies")
    maxlch = s.schlin.restfreq.shape[0]

    record_defaults = {
        "lineset":  ["",   util.upper],
        "restfreq": [[0.], util.extend_to(maxlch)],
        "endlines": [None, util.noop],
    }
    input_iterator.set_defaults(record_defaults, {})

    attribute_to_key = {
        "linename": "lineset",
        "restfreq": "restfreq",
    }
    catalog = LineRestFrequencyCatalog()
    start = int(s.schlin.nlgp)
    index = start

    found_end = False
    for record in input_iterator:
        values, present = util.merge_record_with_defaults(
            record, record_defaults, {})
        if "endlines" in present:
            found_end = True
            break
        if index >= catalog.maxlgp:
            s.errlog(" SCHED only set up to use {} rest frequency groups.".\
                     format(catalog.maxlgp))
        entry = catalog.entries[index]
        entry.set_keyin_values(values, attribute_to_key)
        index += 1

    if not found_end:
        s.errlog("RFREQ: Input data ended while reading spectral line rest "
                 "frequencies.")

    catalog.write(range(start, index))

    s.schlin.nlgp = index

from .catalog import Catalog

import schedlib as s

class LineRestFrequencyCatalog(Catalog):
    """
    INTEGER          NRESTFQ(MAXLGP)
    DOUBLE PRECISION RESTFREQ(MAXLCH,MAXLGP)
    CHARACTER        LINENAME(MAXLGP)*8
    """

    maxlgp = s.schlin.nrestfq.shape[0]
    block_items = {
        s.schlin : [
            "restfreq",
            "nrestfq" # according to SCHED comment, not used
        ],
        s.schcli : [
            "linename"
        ]
    }

    def __init__(self):
        super().__init__(self.maxlgp, self.block_items)


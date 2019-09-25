from .catalog import Catalog

import schedlib as s

class SourcePlotCatalog(Catalog):
    """
    CHARACTER    SRLNAME(MAXSRL)*12, SRLCALC(MAXSRL)*1
    CHARACTER    SRLEPO(MAXSRL)*1
    REAL         SRLRA(MAXSRL), SRLDEC(MAXSRL)
    REAL         SRLRAE(MAXSRL), SRLDECE(MAXSRL)
    LOGICAL      SRLUSED(MAXSRL)
    """

    max_source = s.srlis.srlused.shape[0]

    block_items = {
        s.srlis: [
            "srlra",
            "srlrae",
            "srldece",
            "srldec",
            "srlused"
        ],
        s.srcli: [
            "srlname",
            "srlcalc",
            "srlepo"
        ]
    }

    def __init__(self):
        super().__init__(self.max_source, self.block_items)

    def scheduled_slice(self):
        return slice(s.srlis.srln)
    

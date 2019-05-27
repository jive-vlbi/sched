from .catalog import Catalog

import schedlib as s

class PhaseCenterCatalog(Catalog):
    """
    CHARACTER        CTRNAME(MCENT)*12
    CHARACTER        CTRSRCN(MCSRC,MCENT)*12
    INTEGER          CTRSRCI(MCSRC,MCENT)
    INTEGER          NCSRC(MCENT)
    """
    maxphasecenter = s.schsou.ncsrc.shape[0]
    block_items = {
        s.schcsc: [
            "ctrname",
            "ctrsrcn"
        ],
        s.schsou: [
            "ctrsrci",
            "ncsrc"
        ]
    }

    def __init__(self):
        super().__init__(self.maxphasecenter, self.block_items)

    def write(self, indices=None):
        if indices is None:
            entries = self.entries
        else:
            entries = [self.entries[i] for i in indices]
        for entry in entries:
            entry.ncsrc = len(entry.ctrsrcn)
        return super().write(indices)

    def adjust_lengths(self, entries):
        for entry in entries:
            length = entry.ncsrc
            entry.ctrsrcn = entry.ctrsrcn[:length]
            entry.ctrsrci = entry.ctrsrci[:length]

    def prime(self):
        ret = super().prime()
        self.adjust_lengths(ret)
        return ret

    def read(self):
        ret = super().read()
        self.adjust_lengths(ret)
        return ret

    def scheduled(self):
        return self.entries[:s.schsou.ncent]

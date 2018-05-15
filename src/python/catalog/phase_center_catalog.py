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

    def prime(self):
        super().prime()
        for entry in self.entries:
            length = entry.ncsrc
            entry.ctrsrcn = entry.ctrsrcn[:length]
            entry.ctrsrci = entry.ctrsrci[:length]
        return self.entries

    def read(self):
        super().read()
        self.entries = self.entries[:s.schsou.ncent]
        return self.entries

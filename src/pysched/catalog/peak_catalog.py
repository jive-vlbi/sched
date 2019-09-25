from .catalog import Catalog

import schedlib as s

class PeakCatalog(Catalog):
    """
    INTEGER      NPKSTA(MPKGRP), NPKSRC(MPKGRP)
    INTEGER      PKLSET(MPKGRP), PKLSETL(MPKGRP)
    CHARACTER    PKSRC(MPKSRC,MPKGRP)*12
    INTEGER      PKSRNUM(MPKSRC,MPKGRP)
C     Looking at the code, it seems like this should be PKMINEL(MPKGRP)
    REAL         PKMINEL(MPKSTA)
    CHARACTER    PKLINES(MPKGRP)*8
    CHARACTER    PSETFILE(MPKGRP)*80
    CHARACTER    PLSETFIL(MPKGRP)*80
    CHARACTER    PVLAMODE(MPKGRP)*2
    CHARACTER    PKSTA(MPKSTA,MPKGRP)*8
    DOUBLE PRECISION  PKDWELL(MPKGRP), PKMINFQ(MPKGRP)
    """

    maxpeak = s.schpeakn.npksta.shape[0]
    block_items = {
        s.schpeakn: [
            "npksta",
            "npksrc",
            "pklset",
            "pklsetl",
            "pksrnum",
            "pkminel",
            "pkdwell",
            "pkminfq"
        ],
        s.schpeakc: [
            "pklines",
            "psetfile",
            "plsetfil",
            "pvlamode",
            "pksta",
            "pksrc"
        ]
    }
        
    def __init__(self):
        super().__init__(self.maxpeak, self.block_items)

    def write(self, indices=None):
        if indices is None:
            entries = self.entries
        else:
            entries = [self.entries[i] for i in indices]
        for entry in entries:
            entry.npksrc = len(entry.pksrc)
            entry.npksta = len(entry.pksta)
        return super().write(indices)

    def adjust_lengths(self, entries):
        for entry in entries:
            entry.pksrc = entry.pksrc[:entry.npksrc]
            entry.pksta = entry.pksta[:entry.npksta]

    def scheduled_slice(self):
        return slice(s.schpeakn.npkgrp)

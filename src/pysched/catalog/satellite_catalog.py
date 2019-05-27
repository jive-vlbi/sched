from .catalog import Catalog

import schedlib as s

class SatelliteCatalog(Catalog):
    """
    INTEGER          SATNUM(MAXSAT)
    CHARACTER        SATNAME(MAXSAT)*12
    CHARACTER        SATFILE(MAXSAT)*128
    CHARACTER        TLEFILE(MAXSAT)*128
    CHARACTER        KERFILE(MAXSAT)*128
    """
    maxsatellite = s.schsat.satnum.shape[0]
    block_items = {
        s.schsat: [
            "satnum"
        ],
        s.schcat: [
            "satname", 
            "satfile",
            "tlefile",
            "kerfile"
        ]
    }

    def __init__(self):
        super().__init__(self.maxsatellite, self.block_items)


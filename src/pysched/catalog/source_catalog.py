from .catalog import Catalog

import schedlib as s

import numpy as np

class SourceCatalog(Catalog):
    """
    CHARACTER        SOURCE(MALIAS,MAXSRC)*12, SOUR8(MAXSRC)*8
    CHARACTER        CALCODE(MAXSRC)*1, REMARK(MAXSRC)*80
    DOUBLE PRECISION RACAT(MAXSRC),  DECCAT(MAXSRC)
    DOUBLE PRECISION RA1950(MAXSRC), D1950(MAXSRC)
    DOUBLE PRECISION RA2000(MAXSRC), D2000(MAXSRC)
    DOUBLE PRECISION RAP(MAXSRC), DECP(MAXSRC), PMTIME(MAXSRC)
    DOUBLE PRECISION EPOCHT(MAXSRC)
    DOUBLE PRECISION PMRA(MAXSRC), PMDEC(MAXSRC), PARALAX(MAXSRC)
    CHARACTER*1      C1950(MAXSRC), C2000(MAXSRC), CDATE(MAXSRC)
    REAL             RAERR(MAXSRC), DECERR(MAXSRC)
    REAL             DRA(MAXSRC), DDEC(MAXSRC), VLSR(MAXLCH,MAXSRC)
    CHARACTER        WHICHCAT(MAXSRC)*1 
    CHARACTER        VELREF(MAXSRC)*1, VELDEF(MAXSRC)*1
    LOGICAL          SUSED(MAXSRC), DOPPED(MAXSRC)
    LOGICAL          USEDREC(MAXSRC), USEDPHS(MAXSRC)
    LOGICAL          USEDCENT(MAXSRC)
    INTEGER          SRLSTN(MAXSRC), DIDNDOP(MAXSRC), SATN(MAXSRC)
    REAL             SUNDIS(MAXSRC)
    LOGICAL          PLANET(MAXSRC), SATEL(MAXSRC)
    CHARACTER        CSUSED(MALIAS,MAXSRC)*1
    INTEGER          SRCATN(MAXSRC)
    """
    maxsource = s.schn1.srcatn.shape[0]
    block_items = {
        s.schsou: [
            "racat",
            "deccat",
            "ra1950",
            "d1950",
            "ra2000",
            "d2000",
            "rap",
            "decp",
            "pmtime",
            "epocht",
            "pmra",
            "pmdec",
            "paralax",
            "raerr",
            "decerr",
            "dra",
            "ddec",
            "vlsr",
            "sused",
            "dopped",
            "usedrec",
            "usedphs",
            "usedcent",
            "srlstn",
            "didndop",
            "satn",
            "sundis",
            "planet",
            "satel"
        ],
        s.schn1: [
            "srcatn"
        ],
        s.schcsc: [
            "source",
            "sour8",
            "calcode",
            "remark",
            "c1950",
            "c2000",
            "cdate",
            "whichcat",
            "velref",
            "veldef",
            "csused"
        ]
    }

    def __init__(self):
        super().__init__(self.maxsource, self.block_items)

    def scheduled_slice(self):
        return slice(s.schsou.msrc)

    def used(self):
        return [source for source in self.entries if source.sused]

    def set_aliases(self):
        """
        Pre: self has initialized entries
        """
        for source in self.scheduled():
            source.aliases = source.source[np.in1d(source.csused, ["+", "*"])]
        


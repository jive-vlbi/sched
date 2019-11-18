from .catalog import Catalog, map_attr_array
from ..util import f2str

import schedlib as s

class ScanCatalog(Catalog):
    """
    CHARACTER        CENTERS(MAXSCN)*12
    INTEGER          ICENT(MAXSCN)
    INTEGER          GEOISCN(MAXSCN)
    DOUBLE PRECISION GEOLEN(MAXSCN)
    INTEGER          ORIGEN(MAXSCN)
    LOGICAL          DODOWN(MAXSCN)
    REAL             OPMINEL(MAXSCN)
    REAL             OPSLEWWT(MAXSCN), OPSLEWTI(MAXSCN)
    REAL             OPHLIMWT(MAXSCN), OPHLIMTI(MAXSCN)
    REAL             OPHMAXDT(MAXSCN)
    REAL             OPMINSEP(MAXSCN), OPHA(MAXSCN)
    REAL             OPHAWID(MAXSCN), OPHAWT(MAXSCN)
    INTEGER          OPMIAN(MAXSCN)
    INTEGER          OPMISS(MAXSCN)
    DOUBLE PRECISION PRESCAN(MAXSCN), DUR(MAXSCN), GAP(MAXSCN)
    DOUBLE PRECISION MINDW(MAXSCN), STARTJ(MAXSCN), STOPJ(MAXSCN)
    DOUBLE PRECISION FREQ(MAXCHN,MAXSCN)
    DOUBLE PRECISION BW(MAXCHN,MAXSCN)
    DOUBLE PRECISION CRDFREQ(MAXCHN,MAXSCN), CRDBW(MAXCHN,MAXSCN)
    INTEGER          SRCNUM(MAXSCN), IDOPSRC(MAXSCN), QUAL(MAXSCN)
    INTEGER          SETNUM(MAXSCN)
    INTEGER          DOPEAK(MAXSCN)
    INTEGER          HIGROUP(MAXSCN)
    INTEGER          POINT(MAXSCN), DURONLY(MAXSCN), NOWAIT(MAXSCN)
    INTEGER          ISCINT(MSCINT,MAXSCN), NSCINT(MAXSCN)
    INTEGER          CRDNCH(MAXSCN), CRDCH1(MAXSCN)
    INTEGER          CRDSETCH(MAXCRD,MAXSCN)
    LOGICAL          GOTCRD(MAXSCN)
    LOGICAL          NOTSYS(MAXSCN), DOPCAL(MAXSCN), CRDDOP(MAXSCN)
    LOGICAL          DWELL(MAXSCN), TANT1(MAXSCN), TANT2(MAXSCN)
    CHARACTER        ANNOT(MAXSCN)*128, SCNSRC(MAXSCN)*12
    CHARACTER        DOPSRC(MAXSCN)*12, LINES(MAXSCN)*8
    CHARACTER        PCAL(MAXSCN)*4, SCANTAG(MAXSCN)*4
    CHARACTER        PREEMPT(MAXSCN)*5
    INTEGER          IVLAPHS(MAXSCN), VLAINTEG(MAXSCN)
    INTEGER          VLAPTIME(MAXSCN)
    LOGICAL          PHASING(MAXSCN)
    CHARACTER        VLATSYS(MAXSCN)*1, VLAPEAK(MAXSCN)*9
    CHARACTER        VLAMODE(MAXSCN)*2
    CHARACTER        VLAPHS(MAXSCN)*12
    INTEGER          PTSLEW(MAXSCN), CALTIME(MAXSCN)
    LOGICAL          PNTVLBA(MAXSCN), TANVLBA(MAXSCN), DOPN3DB(MAXSCN)
    REAL             FOCUS(MAXSCN), ROTATION(MAXSCN)
    REAL             SAZCOL(MAXSCN), SELCOL(MAXSCN)
    CHARACTER        CRDLINE(MAXSCN)*80
    DOUBLE PRECISION MINPAUSE(MAXSCN), PRESTART(MAXSCN)
    LOGICAL          NOREC(MAXSCN)
    REAL             GRABTIME(2,MAXSCN), GRABGAP(MAXSCN)
    CHARACTER        DATAPATH(MAXSCN)*8, GRABTO(MAXSCN)*4
    """
    maxscan = s.schn2b.prescan.shape[0]
    block_items = {
        s.schcsc: [
            "centers"
        ],
        s.schsou: [
            "icent",
            "geoiscn",
            "geolen"
        ],
        s.schcon: [
            "origen",
            "dodown",
            "opminel",
            "opslewwt",
            "opslewti",
            "ophlimwt",
            "ophlimti",
            "ophmaxdt",
            "opminsep",
            "opha",
            "ophawid",
            "ophawt",
            "opmian",
            "opmiss"
        ],
        s.schn2a: [
            "srcnum",
            "idopsrc",
            "qual",
            "setnum",
            "dopeak",
            "higroup",
            "point",
            "duronly",
            "nowait",
            "iscint",
            "nscint",
            "crdnch",
            "crdch1",
            "crdsetch",
            "gotcrd",
            "notsys",
            "dopcal",
            "crddop",
            "dwell",
            "tant1",
            "tant2"
        ],
        s.schn2b: [
            "prescan",
            "dur",
            "gap",
            "mindw",
            "freq"
        ],
        s.schn2c: [
            "startj",
            "stopj",
            "bw"
        ],
        s.schn2d: [
            "crdfreq",
            "crdbw"
        ],
        s.schc2a: [
            "annot"
        ],
        s.schc2b: [
            "scnsrc",
            "dopsrc",
            "lines",
            "pcal",
            "scantag",
            "preempt"
        ],
        s.schn3: [
            "ivlaphs",
            "vlainteg",
            "vlaptime",
            "phasing"
        ],
        s.schc3: [
            "vlatsys",
            "vlapeak",
            "vlamode",
            "vlaphs"
        ],
        s.schn4: [
            "ptslew",
            "caltime",
            "pntvlba",
            "tanvlba",
            "dopn3db",
            "focus",
            "rotation",
            "sazcol",
            "selcol",
        ],
        s.schc4: [
            "crdline"
        ],
        s.schn5: [
            "minpause",
            "prestart",
            "norec",
            "grabtime",
            "grabgap"
        ],
        s.schc5: [
            "datapath",
            "grabto"
        ]
    }

    class DirectAccessCatalogEntry(object):
        # a catalog entry which maps setattr and getattr directly to
        # the COMMON blocks in schedlib

        def __init__(self, index, attr_array):
            self.__dict__["_index"] = index
            self.__dict__["_attr_array"] = attr_array

        def __getattr__(self, attr):
            try:
                array = self._attr_array[attr]
            except KeyError:
                raise AttributeError("'{}' object has no attribute '{}'".\
                                     format(type(self).__name__, attr))

            if len(array.shape) > 1:
                return array[..., self._index]
            else:
                ret = array[self._index]
                if array.dtype.kind == "S":
                    return f2str(ret)
                return ret

        def __setattr__(self, attr, value):
            try:
                array = self._attr_array[attr]
            except KeyError:
                return super().__setattr__(attr, value)
            
            array[..., self._index] = value


    def __init__(self):
        super().__init__(self.maxscan, self.block_items, {"scanexps"})
        self.attr_array = map_attr_array(self.block_items)
        self.direct_access_entries = [
            self.DirectAccessCatalogEntry(i, self.attr_array) 
            for i in range(self.maxscan)]

    def read(self, selection_slice=None):
        self.scan_offset = s.schn1.scan1 - 1
        return super().read(selection_slice)

    def scheduled_slice(self):
        return slice(s.schn1.scanl)

    def used(self):
        return self.entries[self.scan_offset:s.schn1.scanl]

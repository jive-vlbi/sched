from .catalog import Catalog

import schedlib as s

class SetupCatalog(Catalog):
    """
    INTEGER           LISTKS(MSET)
    INTEGER           ISETNUM(MSET)
    INTEGER           IFREQNUM(MCHAN,MSET)
    INTEGER           IFREQIF(MCHAN,MSET)
    CHARACTER         SETNAME(MSET)*80
    INTEGER           SFCHAN(MCHAN,MSET), 
                      SGCHAN(MCHAN,MSET)
    DOUBLE PRECISION  FREQREF(MCHAN,MSET)
    DOUBLE PRECISION  FIRSTLO(MCHAN,MSET)
    DOUBLE PRECISION  FIFMIN(MCHAN,MSET)
    DOUBLE PRECISION  FIFMAX(MCHAN,MSET)
    DOUBLE PRECISION  BBSYN(MCHAN,MSET)
    DOUBLE PRECISION  BBSYN2(MCHAN,MSET)
    DOUBLE PRECISION  BBFILT(MCHAN,MSET)
    DOUBLE PRECISION  CORINV(MCHAN,MSET)
    DOUBLE PRECISION  SYNTH(3,MSET)
    REAL              AZCOLIM(MSET), 
                      ELCOLIM(MSET)
    REAL              PTINCR(MSET)
    REAL              PTOFF(MSET)
    REAL              SAMPRATE(MSET)
    REAL              SPEEDH(MSET)
    REAL              SPEEDL(MSET)
    REAL              FANOUT(MSET)
    REAL              SPEEDUP(MSET)
    REAL              BESTOVER(MSET)
    REAL              TOTBW(MSET)
    INTEGER           ISETSTA(MSET)
    INTEGER           NCHAN(MSET)
    INTEGER           TRACK(MCHAN,MTPMOD,MSET)
    INTEGER           PERIOD(MSET)
    INTEGER           BITS(MCHAN,MSET)
    INTEGER           LEVEL(MSET)
    INTEGER           BBC(MCHAN,MSET)
    INTEGER           TAPEMODE(MSET)      
    INTEGER           TPMODE(MSET)      
    INTEGER           SWTCHDUR(MSET)
    INTEGER           PCALFR1(MAXPC,MSET)
    INTEGER           PCALFR2(MAXPC,MSET)
    INTEGER           VFESYN(MCHAN,MSET)
    LOGICAL           DUALPOL(MSET)
    LOGICAL           FRSWITCH(MSET)
    LOGICAL           DUALX(MSET)
    LOGICAL           MODETEST(MSET)
    LOGICAL           USED(MSET)
    CHARACTER         SETSTA(MANT,MSET)*8
    CHARACTER         BAND(MSET)*5
    CHARACTER         IFDIST(4,MSET)*3
    CHARACTER         SIDEBD(MCHAN,MSET)*1
    CHARACTER         CRDSIDE(MCHAN,MSET)*1
    CHARACTER         POL(MCHAN,MSET)*4
    CHARACTER         NOISE(4,MSET)*6
    CHARACTER         NOISEFRQ(MSET)*4
    CHARACTER         FE(4,MSET)*6
    CHARACTER         IFCHAN(MCHAN,MSET)*2
    CHARACTER         ALTIFC(MCHAN,MSET)*2
    CHARACTER         LOGGING(MSET)*8
    CHARACTER         STRING(4,MSET)*80
    CHARACTER         FORMAT(MSET)*8
    CHARACTER         DBE(MSET)*8
    CHARACTER         FIRMFILE(MSET)*80
    CHARACTER         SPCAL(MSET)*4
    CHARACTER         PCALX1(MAXPC,MSET)*3
    CHARACTER         PCALX2(MAXPC,MSET)*3
    CHARACTER         NETSIDE(MCHAN,MSET)*1
    CHARACTER         SIDE1(MCHAN,MSET)*1
    CHARACTER         RCHAN(MSET)*2
    CHARACTER         LCHAN(MSET)*2
    CHARACTER         LCP50CM(MSET)*6
    CHARACTER         RCP50CM(MSET)*6
    CHARACTER         BARREL(MSET)*9
    CHARACTER         M4PATCH(MSET)*8
    INTEGER           MINTRAK(MSET)
    INTEGER           MAXTRAK(MSET)
    REAL              MINTBPS(MSET)
    REAL              MAXTBPS(MSET)
    REAL              TBPS(MSET)
    REAL              TOTBPS(MSET)
    REAL              WRTBPS(MSET)
    LOGICAL           VLBAMKIV(MSET)
    LOGICAL           RECUSED(MSET)
    INTEGER           FLUKESET(MSET)   
    DOUBLE PRECISION  FLUKEA(MSET)     
    DOUBLE PRECISION  FLUKEB(MSET)     
    DOUBLE PRECISION  VLAFEAB(MSET)    
    DOUBLE PRECISION  VLAFECD(MSET)    
    DOUBLE PRECISION  VLASYNA(MSET)    
    DOUBLE PRECISION  VLASYNB(MSET)    
    CHARACTER         FEFILTER(MSET)*4
    CHARACTER         VLAIF(MSET)*10
    CHARACTER         VLAROT(MSET)*10
    CHARACTER         VLABAND(MSET)*2
    CHARACTER         VLABW(MSET)*4
    LOGICAL           VLALOFI(MSET)
    LOGICAL           VLAVA(MSET)
    LOGICAL           VLAVB(MSET)
    LOGICAL           VLAVR(MSET)
    LOGICAL           VLAVL(MSET)
    """
    
    maxsetup = s.setn1.isetnum.shape[0]
    block_items = {
        s.setn1: [
            "listks",
            "isetnum",
            "ifreqnum",
            "ifreqif",
            "sfchan",
            "sgchan"
        ],
        s.setc1: [
            "setname"
        ],
        s.setn2a: [
            "freqref",
            "firstlo",
            "fifmin",
            "fifmax",
            "bbsyn",
            "bbsyn2",
            "bbfilt",
            "corinv",
            "synth",
            "azcolim",
            "elcolim",
            "ptincr",
            "ptoff",
            "samprate",
            "speedh",
            "speedl",
            "fanout",
            "speedup",
            "bestover",
            "totbw"
        ],
        s.setn2b: [
            "isetsta",
            "nchan",
            "track",
            "period",
            "bits",
            "level",
            "bbc",
            "tapemode",
            "tpmode",
            "swtchdur",
            "pcalfr1",
            "pcalfr2",
            "vfesyn",
            "dualpol",
            "frswitch",
            "dualx",
            "modetest",
            "used",
            "mintrak",
            "maxtrak",
            "mintbps",
            "maxtbps",
            "tbps",
            "totbps",
            "wrtbps",
            "vlbamkiv",
            "recused"
        ],
        s.setc2: [
            "setsta",
            "band",
            "ifdist",
            "sidebd",
            "crdside",
            "pol",
            "noise",
            "noisefrq",
            "fe",
            "ifchan",
            "altifc",
            "logging",
            "string_bn",
            "format",
            "dbe",
            "firmfile",
            "spcal",
            "pcalx1",
            "pcalx2",
            "netside",
            "side1",
            "rchan",
            "lchan",
            "lcp50cm",
            "rcp50cm",
            "barrel",
            "m4patch"
        ],
        s.setn3: [
            "flukeset",
            "flukea",
            "flukeb",
            "vlafeab",
            "vlafecd",
            "vlasyna",
            "vlasynb",
            "vlalofi",
            "vlava",
            "vlavb",
            "vlavr",
            "vlavl"
        ],
        s.setc3: [
            "fefilter",
            "vlaif",
            "vlarot",
            "vlaband",
            "vlabw",
        ]
    }

    def __init__(self):
        super().__init__(self.maxsetup, self.block_items, {"dbbcfw"})

    class Channel(object):
        attributes = {
            "ifreqnum",
            "ifreqif",
            "sfchan",
            "sgchan",
            "freqref",
            "firstlo",
            "fifmin",
            "fifmax",
            "bbsyn",
            "bbsyn2",
            "bbfilt",
            "corinv",
            "bits",
            "bbc",
            "vfesyn",
            "sidebd",
            "crdside",
            "pol",
            "ifchan",
            "altifc",
            "netside",
            "side1"}
        def __init__(self, entry, index):
            self.entry = entry
            self.index = index

        # forward attribute get and set to entry
        def __getattr__(self, attr):
            if attr in self.attributes:
                return getattr(self.entry, attr)[self.index]
            # track is the only multidimensional attribute, 
            # so do it separately
            elif attr == "track":
                return getattr(self.entry, attr)[:, self.index]
            raise AttributeError(attr)

        def __setattr__(self, attr, value):
            if attr in self.attributes:
                getattr(self.entry, attr)[self.index] = value
            # track is the only multidimensional attribute, 
            # so do it separately
            elif attr == "track":
                getattr(self.entry, attr)[:, self.index] = value
            else:
                super().__setattr__(attr, value)

    def read(self):
        ret = super().read()
        # nchan is not derived from the length of keyin channel parameters,
        # but is a parameter by itself. Therefore do the reduction of the 
        # size of arrays which depend on it here.
        for entry in ret:
            nchan = entry.nchan
            # track is the only multidimensional attribute, so do it separately
            entry.track = entry.track[:, :nchan]
            for attr in self.Channel.attributes:
                setattr(entry, attr, getattr(entry, attr)[:nchan])
            # create a view to all parameters accessed by channel index
            entry.channel = [self.Channel(entry, index) 
                             for index in range(nchan)]
        return ret
    
    def scheduled_slice(self):
        return slice(s.setn1.nset)

    def used(self):
        """
        Pre: catalog is read.
        """
        return [entry for entry in self.scheduled() if entry.used]
        
    @staticmethod
    def is_dbbc_e_firmware(entry):
        """
        Returns whether the setup entry represent a setup with e-series
        DBBC firmware (True|False).
        """
        if entry.dbbcfw == "":
            return False # assume 107 (or better)
        return entry.dbbcfw[-1] in ("E", "F")

    @classmethod
    def dbbc_firmware_allowed_sample_rates(cls, entry):
        """
        Returns the set of allowed sample rates for the configured 
        DBBC firmware.
        """
        if cls.is_dbbc_e_firmware(entry):
            return {4, 8, 16, 32, 64}
        if entry.dbbcfw == "": # assume 107 (or better)
            return {2, 4, 8, 16, 32, 64, 128}
        if entry.dbbcfw < "107": # known firmwares are 104 and 105
            return {2, 4, 8, 16, 32}
        return {2, 4, 8, 16, 32, 64}

from .catalog import Catalog

import schedlib as s

class FrequencyCatalog(Catalog):
    """
    INTEGER           PRIO(MFREQ)
    INTEGER           FNIF(MFREQ)
    CHARACTER         FSTNAM(MFSTA,MFREQ)*8
    CHARACTER         FIFNAM(MFIF,MFREQ)*2
    CHARACTER         FALTIF(MFIF,MFREQ)*2
    DOUBLE PRECISION  FRF1(MFIF,MFREQ), FRF2(MFIF,MFREQ)
    DOUBLE PRECISION  FLO1(MFIF,MFREQ)
    CHARACTER         FRNAME(MFREQ)*12
    CHARACTER         FRNOTE(MFREQ)*80
    CHARACTER         FPOL(MFIF,MFREQ)*3
    CHARACTER         FFE(MFIF,MFREQ)*4
    DOUBLE PRECISION  FSYN(3,MFREQ)
    DOUBLE PRECISION  FCH1RF1(MFIF,MFREQ), FCH1RF2(MFIF,MFREQ)
    CHARACTER         FRCP50CM(MFREQ)*6, FLCP50CM(MFREQ)*6
    LOGICAL           FDUALX(MFREQ)
    CHARACTER         FVCHNSTA(MFIF,MFREQ)*8
    CHARACTER         FVBAND(MFREQ)*2
    CHARACTER         FVBW(MFREQ)*4
    CHARACTER         FVFILT(MFREQ)*4
    DOUBLE PRECISION  FVFEAB(MFREQ), FVFECD(MFREQ)
    DOUBLE PRECISION  FVSYNA(MFREQ), FVSYNB(MFREQ)
    DOUBLE PRECISION  FVFLKA(MFREQ), FVFLKB(MFREQ)
    """

    maxfreq = s.frqn.prio.shape[0]
    block_items = {
        s.frqn: [
            "prio",
            "fnif",
            "frf1",
            "frf2",
            "flo1",
            "fsyn",
            "fch1rf1",
            "fch1rf2",
            "fdualx",
            "fvfeab",
            "fvfecd",
            "fvsyna",
            "fvsynb",
            "fvflka",
            "fvflkb"],
        s.frqc: [
            "fstnam",
            "fifnam",
            "faltif",
            "frname",
            "frnote",
            "fpol",
            "ffe",
            "frcp50cm",
            "flcp50cm",
            "fvchnsta",
            "fvband",
            "fvbw",
            "fvfilt"]
    }
    
    def __init__(self):
        super().__init__(self.maxfreq, self.block_items, {"tscal"})

    def write(self, indices=None):
        if indices is None:
            entries = self.entries
        else:
            entries = [self.entries[i] for i in indices]
        for entry in entries:
            entry.fnif = len(entry.fifnam)
        return super().write(indices)

    def adjust_lengths(self, entries):
        for entry in entries:
            length = entry.fnif
            for attribute in ("fifnam", "faltif", "frf1", "frf2", "flo1",
                              "fpol", "ffe", "fch1rf1", "fch1rf2", "fvchnsta"):
                setattr(entry, attribute, getattr(entry, attribute)[:length])
    
    def scheduled_slice(self):
        return slice(s.frqn.nfreq)

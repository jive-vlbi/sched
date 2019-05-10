from .catalog import Catalog, get_arrays

import schedlib as s

class StationCatalog(Catalog):
    """
    INTEGER          ISCHSTA(MAXCAT), MJDRATE(MAXCAT)
    CHARACTER        STATION(MAXCAT)*8
    CHARACTER        STCODE(MAXCAT)*3
    CHARACTER        STCODEU(MAXCAT)*3
    DOUBLE PRECISION XPOS(MAXCAT), YPOS(MAXCAT), ZPOS(MAXCAT)
    DOUBLE PRECISION DXPOS(MAXCAT), DYPOS(MAXCAT), DZPOS(MAXCAT)
    DOUBLE PRECISION ELEV(MAXCAT), LAT(MAXCAT), LONG(MAXCAT)
    CHARACTER        POSREF(MAXCAT)*80
    CHARACTER        CONTROL(MAXCAT)*4, MOUNT(MAXCAT)*5
    CHARACTER        DAR(MAXCAT)*5, RECORDER(MAXCAT)*6
    CHARACTER        DISK(MAXCAT)*6, MEDIADEF(MAXCAT)*6
    CHARACTER        TSCAL(MAXCAT)*4, DBBCVER(MAXCAT)*8
    INTEGER          NBBC(MAXCAT), STNDRIV(MAXCAT), NHEADS(MAXCAT)
    LOGICAL          VLBADAR(MAXCAT), USEONSRC(MAXCAT)
    INTEGER          NHORIZ(MAXCAT)
    REAL             HORAZ(200,MAXCAT), HOREL(200,MAXCAT)
    INTEGER          NAXLIM(MAXCAT)
    REAL             AX1LIM(6,MAXCAT), AX2LIM(6,MAXCAT)
    REAL             AX1RATE(MAXCAT), AX2RATE(MAXCAT)
    REAL             AX1ACC(2,MAXCAT), AX2ACC(2,MAXCAT)
    REAL             TSETTLE(MAXCAT), MINSETUP(MAXCAT)
    REAL             MAXSRCHR(MAXCAT), TLEVSET(MAXCAT)
    REAL             ZALIM(MAXCAT), AXOFF(MAXCAT)
    """

    maxcat = s.schsta.ischsta.shape[0]
    block_items = {
        s.schcst: [
            'control', 
            'dar', 
            'dbbcver', 
            'disk', 
            'mediadef', 
            'mount', 
            'posref', 
            'recorder', 
            'station', 
            'stcode', 
            'stcodeu', 
            'tscal'],
        s.schsta: [
            'ax1acc', 
            'ax1lim', 
            'ax1rate', 
            'ax2acc', 
            'ax2lim', 
            'ax2rate', 
            'axoff', 
            'dxpos', 
            'dypos', 
            'dzpos', 
            'elev', 
            'horaz', 
            'horel', 
            'ischsta', 
            'lat', 
            'long_bn', 
            'maxsrchr', 
            'minsetup', 
            'mjdrate', 
            'naxlim', 
            'nbbc', 
            'nheads', 
            'nhoriz', 
            'stndriv', 
            'tlevset', 
            'tsettle', 
            'useonsrc', 
            'vlbadar', 
            'xpos', 
            'ypos', 
            'zalim', 
            'zpos']
        }

    def __init__(self):
        super().__init__(self.maxcat, self.block_items)

    def write(self, indices=None):
        if indices is None:
            entries = self.entries
        else:
            entries = [self.entries[i] for i in indices]
        for entry in entries:
            entry.nhoriz = len(entry.horaz)
        return super().write(indices)

    def adjust_lengths(self, entries):
        for entry in entries:
            length = entry.nhoriz
            entry.horaz = entry.horaz[:length]
            entry.horel = entry.horel[:length]
        
    def prime(self):
        ret = super().prime()
        self.adjust_lengths(ret)
        return ret

    def read(self):
        ret = super().read()
        self.adjust_lengths(ret)
        return ret

    def scheduled(self):
        return self.entries[:s.schsta.msta]

    def used(self):
        return [self.entries[i-1] for i in s.schn1.stanum[:s.schn1.nsta]]

    """
    INTEGER          NSETUP(MAXSCN,MAXSTA)
    INTEGER          FSETI(MAXSCN,MAXSTA)
    LOGICAL          STASCN(MAXSCN,MAXSTA)
    LOGICAL          USETAPE(MAXSTA), USEDISK(MAXSTA)
    DOUBLE PRECISION TPSTART(MAXSCN,MAXSTA)
    REAL             GBYTES(MAXSCN,MAXSTA)
    DOUBLE PRECISION TONSRC(MAXSCN,MAXSTA), TSLEW(MAXSCN,MAXSTA)
    REAL             EL1(MAXSCN,MAXSTA), AZ1(MAXSCN,MAXSTA)
    REAL             HA1(MAXSCN,MAXSTA), PA1(MAXSCN,MAXSTA)
    REAL             EL2(MAXSCN,MAXSTA), AZ2(MAXSCN,MAXSTA)
    REAL             HA2(MAXSCN,MAXSTA), PA2(MAXSCN,MAXSTA)
    CHARACTER        UP1(MAXSCN,MAXSTA)*1, UP2(MAXSCN,MAXSTA)*1
    """
    scheduled_station_items = {
        s.schn2a: [
            'nsetup',
            'fseti',
            'stascn'],
        s.schn5: [
            'usetape',
            'usedisk',
            'tpstart',
            'gbytes'],
        s.schn6: [
            'tonsrc',
            'tslew',
            'el1',
            'az1',
            'ha1',
            'pa1',
            'el2',
            'az2',
            'ha2',
            'pa2'],
        s.schc6: [
            'up1',
            'up2']
    }
    
    def add_scheduled_attributes(self):
        """
        Pre: self has entries
        """
        arrays = get_arrays(self.scheduled_station_items)
        for index, entry in enumerate(self.used()):
            for key, value in arrays.items():
                setattr(entry, key, value[index])
            

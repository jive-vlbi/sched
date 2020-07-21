from .catalog import Catalog, get_arrays, write, map_attr_array
from ..util import f2str

import schedlib as s

import numpy as np

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

    """
    INTEGER          NSETUP(MAXSCN,MAXSTA)
    INTEGER          FSETI(MAXSCN,MAXSTA)
    LOGICAL          STASCN(MAXSCN,MAXSTA)
    LOGICAL          USETAPE(MAXSTA), USEDISK(MAXSTA)
    DOUBLE PRECISION TPSTART(MAXSCN,MAXSTA)
    REAL             GBYTES(MAXSCN,MAXSTA)
    DOUBLE PRECISION LST1(MAXSCN,MAXSTA), LST2(MAXSCN,MAXSTA)
    DOUBLE PRECISION TONSRC(MAXSCN,MAXSTA), TSLEW(MAXSCN,MAXSTA)
    REAL             EL1(MAXSCN,MAXSTA), AZ1(MAXSCN,MAXSTA)
    REAL             HA1(MAXSCN,MAXSTA), PA1(MAXSCN,MAXSTA)
    REAL             EL2(MAXSCN,MAXSTA), AZ2(MAXSCN,MAXSTA)
    REAL             HA2(MAXSCN,MAXSTA), PA2(MAXSCN,MAXSTA)
    CHARACTER        UP1(MAXSCN,MAXSTA)*1, UP2(MAXSCN,MAXSTA)*1
C   from schpeak.inc, MPKSTA is defined as MAXSTA
    INTEGER          PKGROUP(MPKSTA)
    """
    maxsta = s.schn5.usedisk.shape[0]
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
            'lst1',
            'lst2',
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
            'up2'],
        s.schpeakn: [
            'pkgroup']
    }
    
    # all scheduled scan related attributes have an array shape of
    # (scan, station)
    scan_station_items = {
        block: [attr for attr in attrs if len(getattr(block, attr).shape) == 2]
        for block, attrs in scheduled_station_items.items()}
    non_scan_station_items = {
        block: [attr for attr in attrs if len(getattr(block, attr).shape) == 1]
        for block, attrs in scheduled_station_items.items()}
    
    class DirectAccessCatalogEntry(object):
        # a catalog entry which maps setattr and getattr directly to
        # the COMMON blocks in schedlib

        def __init__(self, index, attr_array, scheduled_attr_array):
            self.__dict__["_index"] = index
            self.__dict__["_attr_array"] = attr_array
            self.__dict__["_scheduled_attr_array"] = scheduled_attr_array

        def __getattr__(self, attr):
            try:
                array = self._attr_array[attr]
                index = self._index
            except KeyError:
                # try the scheduled attributes next
                try:
                    array = self._scheduled_attr_array[attr]
                    # map the index into all (catalog) stations to 
                    # an index into scheduled stations
                    index = next(i for i, e in enumerate(
                        s.schn1.stanum[:s.schn1.nsta])
                                 if e == self._index + 1)
                except KeyError:
                    raise AttributeError("'{}' object has no attribute '{}'".\
                                         format(type(self).__name__, attr))
                except StopIteration:
                    raise RuntimeError(
                        "Station {} has no scheduled attributes.".format(
                            self.station))

            if len(array.shape) > 1:
                return array[..., index]
            else:
                ret = array[index]                    
                if array.dtype.kind == "S":
                    return f2str(ret)
                return ret

        def __setattr__(self, attr, value):
            try:
                array = self._attr_array[attr]
                index = self._index
            except KeyError:
                try:
                    # try the scheduled attributes next
                    array = self._scheduled_attr_array[attr]
                    index = next(i for i, e in enumerate(s.schn1.stanum)
                                 if e == self._index + 1)
                except KeyError:
                    return super().__setattr__(attr, value)
                except StopIteration:
                    raise RuntimeError(
                        "Station {} has no scheduled attributes.".format(
                            self.station))

            array[..., index] = value


    def __init__(self):
        super().__init__(self.maxcat, self.block_items)
        self.scheduled_attr_array = map_attr_array(self.scheduled_station_items)
        self.non_scan_attr_array = map_attr_array(self.non_scan_station_items)
        self.has_scheduled_attributes_for = []
        self.direct_access_entries = [
            self.DirectAccessCatalogEntry(i, self.attr_array, 
                                          self.scheduled_attr_array) 
            for i in range(self.maxcat)]
    
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
        
    def scheduled_slice(self):
        return slice(s.schsta.msta)

    def used(self, use_direct_access=False):
        entries = self.direct_access_entries if use_direct_access \
                  else self.entries
        return [entries[i-1] for i in s.schn1.stanum[:s.schn1.nsta]]

    def _read_scheduled_attributes_implementation(self, arrays):
        for index, entry in enumerate(self.used()):
            for key, value in arrays.items():
                setattr(entry, key, value[index])
    
    def read_scheduled_attributes(self):
        """
        Pre: self has entries
        """
        arrays = get_arrays(self.scheduled_attr_array)
        self._read_scheduled_attributes_implementation(arrays)
        self.has_scheduled_attributes_for = s.schn1.stanum[:s.schn1.nsta].copy()
    
    def read_scheduled_attributes_for_scan(self, scan_index):
        """
        Pre: self has entries
        Read scan related attributes for @scan_index into station entries.
        """
        if not np.array_equal(self.has_scheduled_attributes_for,
                              s.schn1.stanum[:s.schn1.nsta]):
            # initialize the station entry scan arrays
            self.read_scheduled_attributes()
            return
        
        for block, attrs in self.scan_station_items.items():
            for attr in attrs:
                array = getattr(block, attr)
                for station_index, entry in enumerate(self.used()):
                    value = array[scan_index, station_index]
                    if array.dtype.kind == "S":
                        value = value.decode()
                    getattr(entry, attr)[scan_index] = value

    def read_non_scan_scheduled_attributes(self):
        """
        Pre: self has entries
        Read scheduled attributes not related to scans into station entries.
        """
        arrays = get_arrays(self.non_scan_attr_array)
        self._read_scheduled_attributes_implementation(arrays)

    def write_scheduled_attributes(self):
        """
        Pre: read_scheduled_attributes is up to date
        """
        entries = self.used()
        write(self.scheduled_attr_array, entries, range(len(entries)))
            
